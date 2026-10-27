#include "program.h"
#include "alloc.h"
#include "interrupts.h"
#include "keyboard.h"
#include "memory.h"
#include "stdio.h"
#include "string.h"
#include "x86.h"

#define PSP_PARAGRAPHS          0x10        // the PSP is 256 bytes, right before the program
#define COM_LOAD_OFFSET         0x0100
#define COM_MAX_SIZE            0xFF00      // a .COM has to fit in 1 segment w/ its PSP & stack
#define COM_PARAGRAPHS          0x1000      // .COM programs get a whole 64KiB segment if there's room
#define MIN_STACK_BYTES         256
#define MZ_SIGNATURE            0x5A4D      // "MZ"
#define LOAD_CHUNK              0x8000      // read files in 32KiB pieces
#define IVT_ENTRIES             256

#pragma pack(push, 1)

// Program Segment Prefix: 256 bytes DOS puts in front of every program, w/ its command line
typedef struct
{
    uint8_t Int20[2];                       // CD 20 = int 20h, a .COM's "ret" lands here
    uint16_t MemoryTop;                     // first segment after the program's memory
    uint8_t Reserved1;
    uint8_t FarCallDos[5];
    uint32_t TerminateVector;               // int 22h, 23h, 24h when the program started
    uint32_t BreakVector;
    uint32_t CriticalErrorVector;
    uint16_t ParentPsp;
    uint8_t FileTable[PROGRAM_MAX_FILES];   // DOS's per program handle table (we keep our own)
    uint16_t Environment;                   // segment of the environment block
    uint32_t SavedStack;
    uint16_t FileTableSize;
    uint32_t FileTablePointer;
    uint8_t Reserved2[24];
    uint8_t DosCall[3];                     // CD 21 CB = int 21h, retf
    uint8_t Reserved3[9];
    uint8_t Fcb1[16];
    uint8_t Fcb2[20];
    uint8_t CommandTailLength;              // # of chars in the command line after the name
    char CommandTail[127];                  // " arguments", ends w/ a carriage return
} PSP;

typedef struct
{
    uint16_t Signature;
    uint16_t BytesInLastPage;               // 0 means the last page is full
    uint16_t Pages;                         // file size in 512 byte pages, including this header
    uint16_t RelocationCount;
    uint16_t HeaderParagraphs;              // header size in 16 byte paragraphs
    uint16_t MinExtraParagraphs;            // memory needed after the image (BSS + stack)
    uint16_t MaxExtraParagraphs;            // memory the program would like
    uint16_t InitialSS;                     // relative to the start of the image
    uint16_t InitialSP;
    uint16_t Checksum;
    uint16_t InitialIP;
    uint16_t InitialCS;                     // relative to the start of the image
    uint16_t RelocationTableOffset;
    uint16_t OverlayNumber;
} MZ_Header;

typedef struct
{
    uint16_t Offset;
    uint16_t Segment;
} MZ_Relocation;

#pragma pack(pop)

// from syscall.asm
void _cdecl syscall_int20();
void _cdecl syscall_int21();
void _cdecl x86_RunProgram(uint16_t cs, uint16_t ip, uint16_t ss, uint16_t sp, uint16_t psp);
void _cdecl x86_ExitProgram();
extern uint16_t __far syscall_depth;

static bool g_Running;
static volatile bool g_BreakPending;
static uint16_t g_Psp;
static uint16_t g_ExitCode;
static uint16_t g_LastExitCode;
static FS_Handle g_Files[PROGRAM_MAX_FILES];

// the program can change any interrupt vector, they all get put back when it exits
static ISR g_SavedVectors[IVT_ENTRIES];

static void PROGRAM_OnBreak()
{
    // in the middle of a syscall the kernel could be halfway through changing the disk, so only
    // stop the program right away if it's running its own code. Otherwise the syscall code
    // checks for Ctrl+Break itself
    if (!g_Running)
    {
        return;
    }
    if (syscall_depth == 0)
    {
        puts("^Break\n");
        PROGRAM_Terminate(PROGRAM_EXIT_CTRL_C << 8);
    }
    g_BreakPending = true;
}

bool PROGRAM_InSyscall()
{
    return syscall_depth > 0;
}

void PROGRAM_CheckBreak()
{
    if (g_Running && g_BreakPending)
    {
        g_BreakPending = false;
        puts("^Break\n");
        PROGRAM_Terminate(PROGRAM_EXIT_CTRL_C << 8);
    }
}

void PROGRAM_Initialize()
{
    IVT_SetVector(VECTOR_DOS_TERMINATE, (ISR)syscall_int20);
    IVT_SetVector(VECTOR_DOS_SYSCALL, (ISR)syscall_int21);
    KBD_SetBreakHandler(PROGRAM_OnBreak);
}

bool PROGRAM_IsRunning()
{
    return g_Running;
}

uint16_t PROGRAM_GetPsp()
{
    return g_Psp;
}

uint16_t PROGRAM_GetLastExitCode()
{
    return g_LastExitCode;
}

FS_Handle* PROGRAM_GetFiles()
{
    return g_Files;
}

void PROGRAM_Terminate(uint16_t exitCode)
{
    g_ExitCode = exitCode;
    x86_ExitProgram();
}

// moves a far pointer forward, keeping the offset small so it never wraps past 64KiB
static void __far* PROGRAM_Advance(void __far* pointer, uint32_t bytes)
{
    uint32_t linear = (uint32_t)FP_SEG(pointer) * 16 + FP_OFF(pointer) + bytes;
    return MK_FP((uint16_t)(linear >> 4), (uint16_t)(linear & 0x0F));
}

// reads size bytes from the file into memory starting at segment:0000, any size
static FS_Status PROGRAM_ReadInto(FS_Handle file, uint16_t segment, uint32_t size)
{
    void __far* destination = MK_FP(segment, 0);
    FS_Status status;

    while (size > 0)
    {
        uint16_t chunk = (size > LOAD_CHUNK) ? LOAD_CHUNK : (uint16_t)size;
        uint16_t bytesRead;

        if ((status = FS_Read(file, destination, chunk, &bytesRead)) != FS_OK)
        {
            return status;
        }
        if (bytesRead != chunk)
        {
            return FS_CORRUPT;                          // file is shorter than its header says
        }

        destination = PROGRAM_Advance(destination, chunk);
        size -= chunk;
    }
    return FS_OK;
}

// zeroes any amount of memory, 32KiB at a time so the offset never wraps
static void PROGRAM_ZeroMemory(void __far* start, uint32_t size)
{
    start = PROGRAM_Advance(start, 0);
    while (size > 0)
    {
        uint16_t chunk = (size > LOAD_CHUNK) ? LOAD_CHUNK : (uint16_t)size;

        memset(start, 0, chunk);
        start = PROGRAM_Advance(start, chunk);
        size -= chunk;
    }
}

// DOS programs (& C runtimes) look at the environment block to find their own path:
// "VAR=value\0" ... "\0", then a word 1, then the program's full path
static uint16_t PROGRAM_CreateEnvironment(const char* path, const char* name)
{
    uint16_t length = strlen(path);
    uint16_t segment = MEM_Allocate((uint16_t)((2 + 2 + length + 1 + 15) / 16), MEM_OWNER_KERNEL, name);
    char __far* environment;

    if (segment == 0)
    {
        return 0;
    }

    environment = MK_FP(segment, 0);
    environment[0] = '\0';                              // no variables
    environment[1] = '\0';
    environment[2] = 1;                                 // 1 string follows
    environment[3] = 0;
    memcpy(environment + 4, path, length + 1);
    return segment;
}

static void PROGRAM_CreatePsp(uint16_t psp, uint16_t paragraphs, uint16_t environment, const char* arguments)
{
    PSP __far* p = MK_FP(psp, 0);
    uint32_t __far* ivt = (uint32_t __far*)MK_FP(0, 0);
    uint8_t length = 0;
    uint8_t i;

    memset(p, 0, sizeof(PSP));
    p->Int20[0] = 0xCD;
    p->Int20[1] = 0x20;
    p->MemoryTop = psp + paragraphs;
    p->TerminateVector = ivt[0x22];
    p->BreakVector = ivt[0x23];
    p->CriticalErrorVector = ivt[0x24];
    p->Environment = environment;
    p->FileTableSize = PROGRAM_MAX_FILES;
    p->FileTablePointer = (uint32_t)MK_FP(psp, 0x18);
    p->DosCall[0] = 0xCD;
    p->DosCall[1] = 0x21;
    p->DosCall[2] = 0xCB;

    // DOS's own handle table: 0-2 console, 3-4 aux & printer, the rest closed (FF)
    memset(p->FileTable, 0xFF, PROGRAM_MAX_FILES);
    p->FileTable[0] = p->FileTable[1] = p->FileTable[2] = 1;
    p->FileTable[3] = 0;
    p->FileTable[4] = 2;

    // blank FCBs (an old way of passing file names, some programs still look at them)
    memset(p->Fcb1 + 1, ' ', 11);
    memset(p->Fcb2 + 1, ' ', 11);

    // command tail: " arguments\r", the space is part of it like in DOS
    if (arguments[0] != '\0')
    {
        p->CommandTail[length++] = ' ';
        for (i = 0; arguments[i] != '\0' && length < 126; i++)
        {
            p->CommandTail[length++] = arguments[i];
        }
    }
    p->CommandTailLength = length;
    p->CommandTail[length] = '\r';
}

bool PROGRAM_Run(const char* path, const char* arguments, uint16_t* exitCodeOut)
{
    FS_Handle file;
    MZ_Header header;
    MZ_Relocation __far* relocations = 0;
    uint32_t fileSize;
    uint32_t imageSize;
    uint32_t needed, wanted;
    uint16_t bytesRead;
    uint16_t neededParagraphs, wantedParagraphs, largest;
    uint16_t r;
    uint16_t environment, psp, imageSegment;
    uint16_t cs, ip, ss, sp;
    bool isExe;
    char name[9];
    const char* baseName = strrchr(path, '\\') + 1;
    uint8_t i;
    FS_Status status;

    if (g_Running)
    {
        printf("A program is already running\n");
        return false;
    }

    // name for the memory blocks, "C:\BIN\HELLO.COM" -> "HELLO"
    for (i = 0; i < 8 && baseName[i] != '\0' && baseName[i] != '.'; i++)
    {
        name[i] = baseName[i];
    }
    name[i] = '\0';

    if ((status = FS_Open(path, FS_READ, &file)) != FS_OK)
    {
        printf("%s\n", FS_StatusString(status));
        return false;
    }
    FS_GetSize(file, &fileSize);

    // .EXE files start w/ "MZ", anything else is a .COM (a raw memory image)
    memset(&header, 0, sizeof(header));
    FS_Read(file, &header, sizeof(header), &bytesRead);
    isExe = (bytesRead == sizeof(header) && header.Signature == MZ_SIGNATURE);

    if (isExe)
    {
        uint32_t headerSize = (uint32_t)header.HeaderParagraphs * 16;

        imageSize = (uint32_t)header.Pages * 512;
        if (header.BytesInLastPage != 0)
        {
            imageSize -= 512 - header.BytesInLastPage;
        }
        if (imageSize < headerSize || imageSize - headerSize > fileSize
            || (uint32_t)header.RelocationTableOffset + (uint32_t)header.RelocationCount * sizeof(MZ_Relocation) > headerSize)
        {
            printf("Not a valid program (bad EXE header)\n");
            FS_Close(file);
            return false;
        }
        imageSize -= headerSize;

        // the relocation table is in the header, read it before the image
        if (header.RelocationCount > 0)
        {
            uint16_t tableSize = header.RelocationCount * sizeof(MZ_Relocation);

            relocations = (MZ_Relocation __far*)kmalloc(tableSize);
            if (relocations == 0)
            {
                printf("Not enough memory\n");
                FS_Close(file);
                return false;
            }
            FS_Seek(file, header.RelocationTableOffset);
            FS_Read(file, relocations, tableSize, &bytesRead);
        }
        FS_Seek(file, headerSize);

        needed = PSP_PARAGRAPHS + (imageSize + 15) / 16 + header.MinExtraParagraphs;
        wanted = needed - header.MinExtraParagraphs + header.MaxExtraParagraphs;
        if (wanted < needed)
        {
            wanted = needed;                            // MaxExtra < MinExtra, bad header
        }

        // more than 1MiB can't fit anyway, the check against free memory below catches it
        neededParagraphs = (needed > 0xFFFF) ? 0xFFFF : (uint16_t)needed;
        wantedParagraphs = (wanted > 0xFFFF) ? 0xFFFF : (uint16_t)wanted;
    }
    else
    {
        if (fileSize > COM_MAX_SIZE)
        {
            printf("Program is too big for a .COM\n");
            FS_Close(file);
            return false;
        }
        imageSize = fileSize;
        FS_Seek(file, 0);

        neededParagraphs = (uint16_t)((COM_LOAD_OFFSET + imageSize + MIN_STACK_BYTES + 15) / 16);
        wantedParagraphs = COM_PARAGRAPHS;
    }

    // memory: a small block for the environment, then 1 block w/ the PSP & program. Like DOS
    // the program gets as much as it asks for, up to everything that's free
    environment = PROGRAM_CreateEnvironment(path, name);
    largest = MEM_GetLargestFree();
    if (environment == 0 || largest < neededParagraphs)
    {
        printf("Not enough memory\n");
        if (environment != 0)
        {
            MEM_Free(environment);
        }
        kfree(relocations);
        FS_Close(file);
        return false;
    }
    if (wantedParagraphs > largest)
    {
        wantedParagraphs = largest;
    }
    psp = MEM_Allocate(wantedParagraphs, MEM_OWNER_KERNEL, name);

    // the program owns its own memory (owner = its PSP), so it's all freed when it exits
    MEM_SetOwner(psp, psp, name);
    MEM_SetOwner(environment, psp, name);

    // load it
    imageSegment = isExe ? psp + PSP_PARAGRAPHS : psp;
    status = PROGRAM_ReadInto(file, isExe ? imageSegment : psp + COM_LOAD_OFFSET / 16, imageSize);
    FS_Close(file);
    if (status != FS_OK)
    {
        printf("%s\n", FS_StatusString(status));
        kfree(relocations);
        MEM_FreeOwner(psp);
        return false;
    }

    PROGRAM_CreatePsp(psp, wantedParagraphs, environment, arguments);

    if (isExe)
    {
        // the linker assumed the image would be at segment 0, add where it really is to
        // every segment value it wrote (far calls, data segments, ...)
        for (r = 0; r < header.RelocationCount; r++)
        {
            uint16_t __far* target = MK_FP(imageSegment + relocations[r].Segment, relocations[r].Offset);
            *target += imageSegment;
        }
        kfree(relocations);

        // BSS (uninitialized data) isn't stored in the file, it starts right after the image
        PROGRAM_ZeroMemory(PROGRAM_Advance(MK_FP(imageSegment, 0), imageSize),
                           (uint32_t)header.MinExtraParagraphs * 16);

        cs = imageSegment + header.InitialCS;
        ip = header.InitialIP;
        ss = imageSegment + header.InitialSS;
        sp = header.InitialSP;
    }
    else
    {
        // .COM: everything in 1 segment, starts at 0100, stack at the top of the segment w/ a 0
        // on it, so a plain "ret" goes to PSP:0000 = int 20h = exit
        uint32_t segmentBytes = (uint32_t)wantedParagraphs * 16;

        cs = ss = psp;
        ip = COM_LOAD_OFFSET;
        sp = (segmentBytes >= 0x10000) ? 0xFFFE : (uint16_t)(segmentBytes - 2);
        *(uint16_t __far*)MK_FP(psp, sp) = 0;
    }

    // remember every interrupt vector, & set up the program's file handles
    memcpy(g_SavedVectors, MK_FP(0, 0), sizeof(g_SavedVectors));
    for (i = 0; i < PROGRAM_MAX_FILES; i++)
    {
        g_Files[i] = -1;
    }

    g_Psp = psp;
    g_ExitCode = 0;
    g_BreakPending = false;
    g_Running = true;

    x86_RunProgram(cs, ip, ss, sp, psp);

    // the program has exited (from anywhere, through x86_ExitProgram). Clean up after it:
    // put back interrupt vectors it changed, close its files, free its memory
    g_Running = false;
    x86_DisableInterrupts();
    memcpy(MK_FP(0, 0), g_SavedVectors, sizeof(g_SavedVectors));
    x86_EnableInterrupts();

    for (i = 0; i < PROGRAM_MAX_FILES; i++)
    {
        if (g_Files[i] >= 0)
        {
            FS_Close(g_Files[i]);
            g_Files[i] = -1;
        }
    }
    MEM_FreeOwner(psp);

    g_Psp = 0;
    g_LastExitCode = g_ExitCode;
    *exitCodeOut = g_ExitCode;
    return true;
}
