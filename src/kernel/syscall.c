#include "stdint.h"
#include "stdbool.h"
#include "alloc.h"
#include "fs.h"
#include "interrupts.h"
#include "keyboard.h"
#include "memory.h"
#include "program.h"
#include "stdio.h"
#include "string.h"
#include "x86.h"

// ------------------------------------------------------------------------------------------------
// int 21h: the DOS system call interface. ah = function #, results come back in registers, &
// errors set the carry flag w/ a DOS error code in ax. Only the commonly used functions are here
// ------------------------------------------------------------------------------------------------

#define DOS_ERROR_INVALID_FUNCTION      1
#define DOS_ERROR_FILE_NOT_FOUND        2
#define DOS_ERROR_PATH_NOT_FOUND        3
#define DOS_ERROR_TOO_MANY_OPEN         4
#define DOS_ERROR_ACCESS_DENIED         5
#define DOS_ERROR_INVALID_HANDLE        6
#define DOS_ERROR_NOT_ENOUGH_MEMORY     8
#define DOS_ERROR_INVALID_BLOCK         9
#define DOS_ERROR_INVALID_DRIVE         15
#define DOS_ERROR_CURRENT_DIRECTORY     16
#define DOS_ERROR_NO_MORE_FILES         18
#define DOS_ERROR_GENERAL_FAILURE       31
#define DOS_ERROR_SHARING_VIOLATION     32

#define STDIN                           0
#define STDOUT                          1
#define STDERR                          2
#define FIRST_FILE_HANDLE               5   // 0-4 are the standard devices
#define LINE_MAX                        128

#define KEY_CTRL_C                      3
#define BDA_TIMER_TICKS                 MK_FP(0x0040, 0x006C)

// the extended key (arrows, F keys) in the middle of being read: DOS gives a 0 then the scancode
static uint8_t g_PendingScancode;

#define AH(r)                   ((uint8_t)((r)->ax >> 8))
#define AL(r)                   ((uint8_t)(r)->ax)
#define DL(r)                   ((uint8_t)(r)->dx)
#define SET_AL(r, value)        ((r)->ax = ((r)->ax & 0xFF00) | (uint8_t)(value))

static void SYSCALL_Success(ISR_Registers __far* r)
{
    r->flags &= ~FLAG_CARRY;
}

static void SYSCALL_Fail(ISR_Registers __far* r, uint16_t error)
{
    r->ax = error;
    r->flags |= FLAG_CARRY;
}

static uint16_t SYSCALL_DosError(FS_Status status)
{
    switch (status)
    {
        case FS_NOT_FOUND:              return DOS_ERROR_FILE_NOT_FOUND;
        case FS_PATH_NOT_FOUND:
        case FS_BAD_NAME:
        case FS_NOT_DIRECTORY:          return DOS_ERROR_PATH_NOT_FOUND;
        case FS_TOO_MANY_OPEN:          return DOS_ERROR_TOO_MANY_OPEN;
        case FS_BAD_HANDLE:             return DOS_ERROR_INVALID_HANDLE;
        case FS_INVALID_DRIVE:          return DOS_ERROR_INVALID_DRIVE;
        case FS_CURRENT_DIRECTORY:      return DOS_ERROR_CURRENT_DIRECTORY;
        case FS_END:                    return DOS_ERROR_NO_MORE_FILES;
        case FS_IN_USE:                 return DOS_ERROR_SHARING_VIOLATION;
        case FS_IO_ERROR:
        case FS_CORRUPT:
        case FS_NOT_FAT:                return DOS_ERROR_GENERAL_FAILURE;
        default:                        return DOS_ERROR_ACCESS_DENIED;     // exists, full, ...
    }
}

static void SYSCALL_Result(ISR_Registers __far* r, FS_Status status)
{
    if (status == FS_OK)
    {
        SYSCALL_Success(r);
    }
    else
    {
        SYSCALL_Fail(r, SYSCALL_DosError(status));
    }
}

// copies a null terminated string from the program's memory, false if it's too long
static bool SYSCALL_CopyString(char* out, uint16_t segment, uint16_t offset, uint16_t size)
{
    const char __far* in = MK_FP(segment, offset);
    uint16_t i;

    for (i = 0; i < size; i++)
    {
        if ((out[i] = in[i]) == '\0')
        {
            return true;
        }
    }
    return false;
}

// ds:dx = path, used by most file functions
static bool SYSCALL_GetPath(ISR_Registers __far* r, char* path)
{
    if (!SYSCALL_CopyString(path, r->ds, r->dx, FS_PATH_MAX))
    {
        SYSCALL_Fail(r, DOS_ERROR_PATH_NOT_FOUND);
        return false;
    }
    return true;
}

// ------------------------------------------------------------------------------------------------
// console
// ------------------------------------------------------------------------------------------------

static char SYSCALL_ReadChar(bool echo, bool checkCtrlC)
{
    uint16_t key;
    char c;

    if (g_PendingScancode != 0)
    {
        c = (char)g_PendingScancode;
        g_PendingScancode = 0;
        return c;
    }

    // wait for a key, Ctrl+Break ends the program while it waits
    while (!KBD_HasKey())
    {
        PROGRAM_CheckBreak();
        x86_Halt();
    }
    key = KBD_GetKey();
    c = KEY_ASCII(key);

    if (checkCtrlC && c == KEY_CTRL_C)
    {
        puts("^C\n");
        PROGRAM_Terminate(PROGRAM_EXIT_CTRL_C << 8);
    }
    if (c == '\0')
    {
        g_PendingScancode = KEY_SCANCODE(key);          // returned by the next call
    }
    else if (echo)
    {
        putc(c);
    }
    return c;
}

// reads a line w/ echo & backspace into buffer (no newline), returns its length
static uint8_t SYSCALL_ReadLine(char __far* buffer, uint8_t max)
{
    uint8_t length = 0;

    for (;;)
    {
        char c = SYSCALL_ReadChar(false, true);

        if (c == '\r')
        {
            putc('\n');
            return length;
        }
        if (c == '\b')
        {
            if (length > 0)
            {
                length--;
                puts("\b \b");
            }
        }
        else if (c == '\0')
        {
            SYSCALL_ReadChar(false, true);              // drop the scancode of special keys
        }
        else if ((uint8_t)c >= ' ' && length < max)
        {
            buffer[length++] = c;
            putc(c);
        }
    }
}

// ------------------------------------------------------------------------------------------------
// files
// ------------------------------------------------------------------------------------------------

static void SYSCALL_OpenFile(ISR_Registers __far* r, uint8_t flags)
{
    FS_Handle* files = PROGRAM_GetFiles();
    char path[FS_PATH_MAX];
    FS_Handle handle;
    FS_Status status;
    uint8_t i;

    if (!SYSCALL_GetPath(r, path))
    {
        return;
    }

    for (i = FIRST_FILE_HANDLE; i < PROGRAM_MAX_FILES && files[i] >= 0; i++);
    if (i == PROGRAM_MAX_FILES)
    {
        SYSCALL_Fail(r, DOS_ERROR_TOO_MANY_OPEN);
        return;
    }

    if ((status = FS_Open(path, flags, &handle)) != FS_OK)
    {
        SYSCALL_Result(r, status);
        return;
    }

    files[i] = handle;
    r->ax = i;
    SYSCALL_Success(r);
}

// the file system handle for a program's handle, -1 if it's not an open file
static FS_Handle SYSCALL_GetFile(uint16_t handle)
{
    return (handle >= FIRST_FILE_HANDLE && handle < PROGRAM_MAX_FILES) ? PROGRAM_GetFiles()[handle] : -1;
}

static void SYSCALL_Read(ISR_Registers __far* r)
{
    uint16_t bytesRead = 0;
    FS_Handle file;

    if (r->bx == STDIN)
    {
        // a line from the keyboard, ending in CR LF like DOS gives it
        char line[LINE_MAX + 2];
        uint8_t length = SYSCALL_ReadLine(line, LINE_MAX);

        line[length++] = '\r';
        line[length++] = '\n';
        bytesRead = (r->cx < length) ? r->cx : length;
        memcpy(MK_FP(r->ds, r->dx), line, bytesRead);
        r->ax = bytesRead;
        SYSCALL_Success(r);
        return;
    }

    if ((file = SYSCALL_GetFile(r->bx)) < 0)
    {
        SYSCALL_Fail(r, DOS_ERROR_INVALID_HANDLE);
        return;
    }
    SYSCALL_Result(r, FS_Read(file, MK_FP(r->ds, r->dx), r->cx, &bytesRead));
    if (!(r->flags & FLAG_CARRY))
    {
        r->ax = bytesRead;
    }
}

static void SYSCALL_Write(ISR_Registers __far* r)
{
    uint16_t written = 0;
    FS_Handle file;
    FS_Status status;

    if (r->bx == STDOUT || r->bx == STDERR)
    {
        const char __far* text = MK_FP(r->ds, r->dx);
        uint16_t i;

        for (i = 0; i < r->cx; i++)
        {
            putc(text[i]);
        }
        r->ax = r->cx;
        SYSCALL_Success(r);
        return;
    }
    if (r->bx < FIRST_FILE_HANDLE)
    {
        r->ax = r->cx;                                  // aux & printer: nothing's there
        SYSCALL_Success(r);
        return;
    }

    if ((file = SYSCALL_GetFile(r->bx)) < 0)
    {
        SYSCALL_Fail(r, DOS_ERROR_INVALID_HANDLE);
        return;
    }

    // DOS reports a full disk as writing fewer bytes than asked, not as an error
    status = FS_Write(file, MK_FP(r->ds, r->dx), r->cx, &written);
    if (status == FS_OK || status == FS_DISK_FULL || status == FS_ROOT_FULL)
    {
        r->ax = written;
        SYSCALL_Success(r);
    }
    else
    {
        SYSCALL_Result(r, status);
    }
}

static void SYSCALL_Seek(ISR_Registers __far* r)
{
    FS_Handle file = SYSCALL_GetFile(r->bx);
    int32_t offset = (int32_t)(((uint32_t)r->cx << 16) | r->dx);
    uint32_t base = 0;
    uint32_t position;

    if (file < 0)
    {
        SYSCALL_Fail(r, (r->bx < FIRST_FILE_HANDLE) ? DOS_ERROR_ACCESS_DENIED : DOS_ERROR_INVALID_HANDLE);
        return;
    }

    // al = where to count from: 0 start, 1 current position, 2 end
    switch (AL(r))
    {
        case 0:     base = 0; break;
        case 1:     FS_GetPosition(file, &base); break;
        case 2:     FS_GetSize(file, &base); break;
        default:    SYSCALL_Fail(r, DOS_ERROR_INVALID_FUNCTION); return;
    }

    if (offset < 0 && (uint32_t)-offset > base)
    {
        SYSCALL_Fail(r, DOS_ERROR_INVALID_FUNCTION);
        return;
    }

    FS_Seek(file, base + offset);
    FS_GetPosition(file, &position);
    r->dx = (uint16_t)(position >> 16);
    r->ax = (uint16_t)position;
    SYSCALL_Success(r);
}

static void SYSCALL_Close(ISR_Registers __far* r)
{
    FS_Handle file = SYSCALL_GetFile(r->bx);

    if (r->bx < FIRST_FILE_HANDLE)
    {
        SYSCALL_Success(r);                             // standard devices stay open
        return;
    }
    if (file < 0)
    {
        SYSCALL_Fail(r, DOS_ERROR_INVALID_HANDLE);
        return;
    }

    FS_Close(file);
    PROGRAM_GetFiles()[r->bx] = -1;
    SYSCALL_Success(r);
}

// ------------------------------------------------------------------------------------------------
// date & time
// ------------------------------------------------------------------------------------------------

static uint8_t SYSCALL_FromBcd(uint8_t bcd)
{
    return (bcd >> 4) * 10 + (bcd & 0x0F);
}

// 0 = Sunday
static uint8_t SYSCALL_DayOfWeek(uint16_t year, uint8_t month, uint8_t day)
{
    static const uint8_t offsets[] = { 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4 };

    if (month < 3)
    {
        year--;
    }
    return (uint8_t)((year + year / 4 - year / 100 + year / 400 + offsets[month - 1] + day) % 7);
}

static void SYSCALL_GetDate(ISR_Registers __far* r)
{
    uint32_t date = x86_RTC_GetDate();
    uint16_t year = 1980;
    uint8_t month = 1, day = 1;

    // a PC/XT has no clock chip, it's 1980-01-01 there like DOS w/o a date set
    if (date != 0xFFFFFFFF)
    {
        year = SYSCALL_FromBcd((uint8_t)(date >> 24)) * 100 + SYSCALL_FromBcd((uint8_t)(date >> 16));
        month = SYSCALL_FromBcd((uint8_t)(date >> 8));
        day = SYSCALL_FromBcd((uint8_t)date);
    }

    r->cx = year;
    r->dx = ((uint16_t)month << 8) | day;
    SET_AL(r, SYSCALL_DayOfWeek(year, month, day));
}

static void SYSCALL_GetTime(ISR_Registers __far* r)
{
    // the BIOS counts timer ticks since midnight (on every PC, even w/o a clock chip):
    // 1573040 ticks per day = 18.2 per second
    uint32_t ticks = *(uint32_t __far*)BDA_TIMER_TICKS;
    uint32_t hundredths = ticks * 5 + ticks * 49 / 100;    // * 5.49, ~100 / 18.2

    r->cx = (uint16_t)((hundredths / 360000) << 8) | (uint8_t)(hundredths / 6000 % 60);
    r->dx = (uint16_t)((hundredths / 100 % 60) << 8) | (uint8_t)(hundredths % 100);
}

// ------------------------------------------------------------------------------------------------
// the dispatcher
// ------------------------------------------------------------------------------------------------

void _cdecl SYSCALL_Handle(ISR_Registers __far* r)
{
    char path[FS_PATH_MAX];
    uint16_t owner, size;

    switch (AH(r))
    {
        // --- programs ---
        case 0x00:                                      // terminate
            PROGRAM_Terminate(0);
            break;

        case 0x4C:                                      // terminate w/ return code al
            PROGRAM_Terminate(PROGRAM_EXIT_NORMAL << 8 | AL(r));
            break;

        case 0x4D:                                      // return code of the last program
            r->ax = PROGRAM_GetLastExitCode();
            SYSCALL_Success(r);
            break;

        case 0x51:                                      // get PSP segment -> bx
        case 0x62:
            r->bx = PROGRAM_GetPsp();
            break;

        case 0x30:                                      // version: pretend to be DOS 3.30
            r->ax = 0x1E03;
            r->bx = 0;
            r->cx = 0;
            break;

        // --- console ---
        case 0x01:                                      // read char w/ echo -> al
            SET_AL(r, SYSCALL_ReadChar(true, true));
            break;

        case 0x02:                                      // write char dl
            putc(DL(r));
            SET_AL(r, DL(r));
            break;

        case 0x06:                                      // direct console I/O
            if (DL(r) == 0xFF)
            {
                // input if there's a key (ZF = 0), w/o waiting
                if (g_PendingScancode != 0 || KBD_HasKey())
                {
                    SET_AL(r, SYSCALL_ReadChar(false, false));
                    r->flags &= ~FLAG_ZERO;
                }
                else
                {
                    SET_AL(r, 0);
                    r->flags |= FLAG_ZERO;
                }
            }
            else
            {
                putc(DL(r));
                SET_AL(r, DL(r));
            }
            break;

        case 0x07:                                      // read char, no echo, no Ctrl+C check
            SET_AL(r, SYSCALL_ReadChar(false, false));
            break;

        case 0x08:                                      // read char, no echo
            SET_AL(r, SYSCALL_ReadChar(false, true));
            break;

        case 0x09:                                      // write "$" terminated string at ds:dx
        {
            const char __far* text = MK_FP(r->ds, r->dx);
            uint16_t i;

            for (i = 0; text[i] != '$' && i < 0xFFFF; i++)
            {
                putc(text[i]);
            }
            SET_AL(r, '$');
            break;
        }

        case 0x0A:                                      // buffered line input at ds:dx
        {
            // [0] = max chars (incl. the CR), [1] = chars read (w/o the CR), [2...] = the line
            uint8_t __far* buffer = MK_FP(r->ds, r->dx);

            if (buffer[0] > 0)
            {
                buffer[1] = SYSCALL_ReadLine((char __far*)(buffer + 2), buffer[0] - 1);
                buffer[2 + buffer[1]] = '\r';
            }
            break;
        }

        case 0x0B:                                      // key available? al = FF yes, 00 no
            SET_AL(r, (g_PendingScancode != 0 || KBD_HasKey()) ? 0xFF : 0x00);
            break;

        case 0x0C:                                      // flush keyboard, then input function al
            g_PendingScancode = 0;
            while (KBD_HasKey())
            {
                KBD_GetKey();
            }
            if (AL(r) == 0x01 || AL(r) == 0x06 || AL(r) == 0x07 || AL(r) == 0x08 || AL(r) == 0x0A)
            {
                r->ax = (uint16_t)AL(r) << 8 | AL(r);
                SYSCALL_Handle(r);
            }
            break;

        // --- drives & directories ---
        case 0x0E:                                      // set default drive dl (0 = A:)
            FS_SetDefaultDrive('A' + DL(r));
            SET_AL(r, 4);                               // # of drive letters
            break;

        case 0x19:                                      // default drive -> al (0 = A:)
            SET_AL(r, FS_GetDefaultDrive() - 'A');
            break;

        case 0x39:                                      // mkdir ds:dx
            if (SYSCALL_GetPath(r, path))
            {
                SYSCALL_Result(r, FS_MakeDirectory(path));
            }
            break;

        case 0x3A:                                      // rmdir ds:dx
            if (SYSCALL_GetPath(r, path))
            {
                SYSCALL_Result(r, FS_RemoveDirectory(path));
            }
            break;

        case 0x3B:                                      // chdir ds:dx
            if (SYSCALL_GetPath(r, path))
            {
                SYSCALL_Result(r, FS_ChangeDirectory(path));
            }
            break;

        case 0x47:                                      // current dir of drive dl (0 = default) -> ds:si
        {
            char drive = (DL(r) == 0) ? FS_GetDefaultDrive() : 'A' + DL(r) - 1;
            const char* current = FS_GetCurrentDirectory(drive);

            if (current == 0)
            {
                SYSCALL_Fail(r, DOS_ERROR_INVALID_DRIVE);
                break;
            }
            // DOS leaves out the drive & first backslash: "DOCS\SUB", "" for the root
            memcpy(MK_FP(r->ds, r->si), current + 1, strlen(current));
            SYSCALL_Success(r);
            break;
        }

        // --- files ---
        case 0x3C:                                      // create (or empty) file ds:dx -> handle ax
            SYSCALL_OpenFile(r, FS_READ | FS_WRITE | FS_CREATE | FS_TRUNCATE);
            break;

        case 0x3D:                                      // open ds:dx, al = 0 read, 1 write, 2 both
        {
            static const uint8_t modes[] = { FS_READ, FS_WRITE, FS_READ | FS_WRITE };

            if ((AL(r) & 3) == 3)
            {
                SYSCALL_Fail(r, DOS_ERROR_ACCESS_DENIED);
                break;
            }
            SYSCALL_OpenFile(r, modes[AL(r) & 3]);
            break;
        }

        case 0x3E:                                      // close bx
            SYSCALL_Close(r);
            break;

        case 0x3F:                                      // read cx bytes from bx into ds:dx
            SYSCALL_Read(r);
            break;

        case 0x40:                                      // write cx bytes from ds:dx to bx
            SYSCALL_Write(r);
            break;

        case 0x41:                                      // delete ds:dx
            if (SYSCALL_GetPath(r, path))
            {
                SYSCALL_Result(r, FS_Delete(path));
            }
            break;

        case 0x42:                                      // seek bx by cx:dx from al -> dx:ax
            SYSCALL_Seek(r);
            break;

        case 0x44:                                      // IOCTL, C runtimes use al = 0 at startup
            if (AL(r) == 0x00)
            {
                // device info: standard handles are the console, files report their drive
                if (r->bx < FIRST_FILE_HANDLE)
                {
                    r->dx = 0x80D3;                     // character device, console in & out
                }
                else if (SYSCALL_GetFile(r->bx) >= 0)
                {
                    r->dx = FS_GetDefaultDrive() - 'A';
                }
                else
                {
                    SYSCALL_Fail(r, DOS_ERROR_INVALID_HANDLE);
                    break;
                }
                SYSCALL_Success(r);
            }
            else if (AL(r) == 0x01)
            {
                SYSCALL_Success(r);                     // set device info: nothing to set
            }
            else
            {
                SYSCALL_Fail(r, DOS_ERROR_INVALID_FUNCTION);
            }
            break;

        case 0x4E:                                      // find first: no wildcard search yet
        case 0x4F:
            SYSCALL_Fail(r, DOS_ERROR_NO_MORE_FILES);
            break;

        // --- memory ---
        case 0x48:                                      // allocate bx paragraphs -> segment ax
        {
            uint16_t segment = MEM_Allocate(r->bx, PROGRAM_GetPsp(), 0);

            if (segment == 0)
            {
                r->bx = MEM_GetLargestFree();
                SYSCALL_Fail(r, DOS_ERROR_NOT_ENOUGH_MEMORY);
                break;
            }
            r->ax = segment;
            SYSCALL_Success(r);
            break;
        }

        case 0x49:                                      // free block es
            if (!MEM_GetBlockInfo(r->es, &owner, &size) || owner != PROGRAM_GetPsp() || !MEM_Free(r->es))
            {
                SYSCALL_Fail(r, DOS_ERROR_INVALID_BLOCK);
                break;
            }
            SYSCALL_Success(r);
            break;

        case 0x4A:                                      // resize block es to bx paragraphs
            if (!MEM_GetBlockInfo(r->es, &owner, &size) || owner != PROGRAM_GetPsp())
            {
                SYSCALL_Fail(r, DOS_ERROR_INVALID_BLOCK);
                break;
            }
            if (!MEM_Resize(r->es, r->bx))
            {
                r->bx = MEM_GetMaxSize(r->es);          // the most it could be
                SYSCALL_Fail(r, DOS_ERROR_NOT_ENOUGH_MEMORY);
                break;
            }
            SYSCALL_Success(r);
            break;

        // --- system ---
        case 0x25:                                      // set interrupt vector al to ds:dx
            IVT_SetVector(AL(r), (ISR)MK_FP(r->ds, r->dx));
            break;

        case 0x35:                                      // get interrupt vector al -> es:bx
        {
            void __far* handler = (void __far*)IVT_GetVector(AL(r));

            r->es = FP_SEG(handler);
            r->bx = FP_OFF(handler);
            break;
        }

        case 0x2A:                                      // date -> cx year, dh month, dl day, al weekday
            SYSCALL_GetDate(r);
            break;

        case 0x2C:                                      // time -> ch hour, cl min, dh sec, dl 1/100s
            SYSCALL_GetTime(r);
            break;

        default:
            printf("[int 21h ah=%02xh isn't supported]\n", AH(r));
            SYSCALL_Fail(r, DOS_ERROR_INVALID_FUNCTION);
            break;
    }

    // Ctrl+Break pressed while we were busy, now's a safe time to stop the program
    PROGRAM_CheckBreak();
}
