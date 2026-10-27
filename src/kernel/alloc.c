#include "alloc.h"
#include "memory.h"
#include "panic.h"
#include "stddef.h"
#include "stdio.h"
#include "x86.h"

#define MCB_TYPE_MORE       'M'             // more blocks follow
#define MCB_TYPE_LAST       'Z'             // last block in the chain
#define MCB_NAME_LENGTH     8

// first paragraph free after boot: below it is the interrupt vector table (0000-03FF) & the
// BIOS data area (0400-04FF), & 0500-05FF is reserved for BIOS/DOS use. Everything from here up
// to the kernel held stage1, stage2 & their buffers, which aren't needed anymore
#define MEM_LOW_START       0x0060

#pragma pack(push, 1)

// exactly 1 paragraph, same layout as DOS
typedef struct
{
    uint8_t Type;
    uint16_t Owner;
    uint16_t Size;                          // in paragraphs, not counting this header
    uint8_t Reserved[3];
    char Name[MCB_NAME_LENGTH];             // not null terminated if all 8 chars are used
} MCB;

#pragma pack(pop)

// labels from entry.asm marking the first & last byte of the kernel in memory. __far makes
// Watcom use each label's own segment, otherwise it assumes small variables are in DGROUP
// (kernel_start is in the code segment)
extern uint8_t __far kernel_start;
extern uint8_t __far kernel_end;

static uint16_t g_FirstBlock;

static MCB __far* MCB_At(uint16_t segment)
{
    MCB __far* mcb = (MCB __far*)MK_FP(segment, 0);

    // a bad pointer or a program writing past its block would wreck the chain, stop before
    // we hand out memory that's in use
    if (mcb->Type != MCB_TYPE_MORE && mcb->Type != MCB_TYPE_LAST)
    {
        printf("\nBad memory control block at %04x:0000", segment);
        panic("memory chain is corrupted");
    }
    return mcb;
}

static uint16_t MCB_Next(uint16_t segment, MCB __far* mcb)
{
    return segment + 1 + mcb->Size;
}

static void MCB_Create(uint16_t segment, uint8_t type, uint16_t owner, uint16_t size, const char* name)
{
    MCB __far* mcb = (MCB __far*)MK_FP(segment, 0);
    uint8_t i;

    mcb->Type = type;
    mcb->Owner = owner;
    mcb->Size = size;
    for (i = 0; i < sizeof(mcb->Reserved); i++)
    {
        mcb->Reserved[i] = 0;
    }

    // copy up to 8 chars, pad w/ zeros
    for (i = 0; i < MCB_NAME_LENGTH; i++)
    {
        mcb->Name[i] = (name != NULL && *name) ? *name++ : '\0';
    }
}

// converts a far pointer to the first paragraph at or after it
static uint16_t MEM_ParagraphOf(void __far* pointer)
{
    return FP_SEG(pointer) + (FP_OFF(pointer) + 15) / 16;
}

void MEM_Initialize()
{
    uint16_t kernelStart = MEM_ParagraphOf(&kernel_start);
    uint16_t kernelEnd = MEM_ParagraphOf(&kernel_end);
    uint16_t codeSegment = FP_SEG((void __far*)MEM_Initialize);
    uint16_t memoryTop = x86_GetMemorySize() * 64;      // KiB -> paragraphs

    // the kernel's own block header goes in the paragraph right before it, which is part of
    // the MZ header stage2 loaded & doesn't need anymore
    uint16_t kernelBlock = kernelStart - 1;

    // if the kernel's bounds are wrong we'd hand out memory the kernel is running from
    if (codeSegment < kernelStart || codeSegment >= kernelEnd)
    {
        printf("\nKernel %04x-%04x doesn't contain code segment %04x", kernelStart, kernelEnd, codeSegment);
        panic("can't find the kernel's bounds");
    }

    g_FirstBlock = MEM_LOW_START;
    MCB_Create(MEM_LOW_START, MCB_TYPE_MORE, MEM_OWNER_FREE, kernelBlock - MEM_LOW_START - 1, NULL);
    MCB_Create(kernelBlock, MCB_TYPE_MORE, MEM_OWNER_KERNEL, kernelEnd - kernelStart, "KERNEL");
    MCB_Create(kernelEnd, MCB_TYPE_LAST, MEM_OWNER_FREE, memoryTop - kernelEnd - 1, NULL);
}

// merges every run of neighbouring free blocks into 1 block
static void MEM_Coalesce()
{
    uint16_t segment = g_FirstBlock;
    MCB __far* mcb = MCB_At(segment);

    while (mcb->Type != MCB_TYPE_LAST)
    {
        uint16_t nextSegment = MCB_Next(segment, mcb);
        MCB __far* next = MCB_At(nextSegment);

        if (mcb->Owner == MEM_OWNER_FREE && next->Owner == MEM_OWNER_FREE)
        {
            // swallow the next block (& its header), then check the new neighbour
            mcb->Size += 1 + next->Size;
            mcb->Type = next->Type;
        }
        else
        {
            segment = nextSegment;
            mcb = next;
        }
    }
}

// splits a block so it's exactly paragraphs long, the rest becomes a free block after it
static void MEM_Split(uint16_t segment, MCB __far* mcb, uint16_t paragraphs)
{
    if (mcb->Size > paragraphs)
    {
        MCB_Create(segment + 1 + paragraphs, mcb->Type, MEM_OWNER_FREE, mcb->Size - paragraphs - 1, NULL);
        mcb->Type = MCB_TYPE_MORE;
        mcb->Size = paragraphs;
    }
}

// finds the block whose data starts at dataSegment, so we never trust a pointer that isn't
// actually in the chain
static MCB __far* MEM_FindBlock(uint16_t dataSegment)
{
    uint16_t segment = g_FirstBlock;

    for (;;)
    {
        MCB __far* mcb = MCB_At(segment);

        if (segment + 1 == dataSegment)
        {
            return mcb;
        }
        if (mcb->Type == MCB_TYPE_LAST || segment + 1 > dataSegment)
        {
            return NULL;
        }
        segment = MCB_Next(segment, mcb);
    }
}

uint16_t MEM_Allocate(uint16_t paragraphs, uint16_t owner, const char* name)
{
    uint16_t segment = g_FirstBlock;

    if (paragraphs == 0 || owner == MEM_OWNER_FREE)
    {
        return 0;
    }

    for (;;)
    {
        MCB __far* mcb = MCB_At(segment);

        if (mcb->Owner == MEM_OWNER_FREE && mcb->Size >= paragraphs)
        {
            MEM_Split(segment, mcb, paragraphs);
            MCB_Create(segment, mcb->Type, owner, paragraphs, name);
            return segment + 1;
        }

        if (mcb->Type == MCB_TYPE_LAST)
        {
            return 0;
        }
        segment = MCB_Next(segment, mcb);
    }
}

bool MEM_Free(uint16_t segment)
{
    MCB __far* mcb = MEM_FindBlock(segment);

    if (mcb == NULL || mcb->Owner == MEM_OWNER_FREE)
    {
        return false;
    }

    MCB_Create(segment - 1, mcb->Type, MEM_OWNER_FREE, mcb->Size, NULL);
    MEM_Coalesce();
    return true;
}

bool MEM_Resize(uint16_t segment, uint16_t paragraphs)
{
    MCB __far* mcb = MEM_FindBlock(segment);

    if (mcb == NULL || mcb->Owner == MEM_OWNER_FREE || paragraphs == 0)
    {
        return false;
    }

    if (paragraphs > mcb->Size)
    {
        MCB __far* next;

        // can only grow into a free block right after this one
        if (mcb->Type == MCB_TYPE_LAST)
        {
            return false;
        }
        next = MCB_At(MCB_Next(segment - 1, mcb));
        if (next->Owner != MEM_OWNER_FREE || (uint32_t)mcb->Size + 1 + next->Size < paragraphs)
        {
            return false;
        }

        mcb->Size += 1 + next->Size;
        mcb->Type = next->Type;
    }

    MEM_Split(segment - 1, mcb, paragraphs);
    MEM_Coalesce();
    return true;
}

bool MEM_SetOwner(uint16_t segment, uint16_t owner, const char* name)
{
    MCB __far* mcb = MEM_FindBlock(segment);

    if (mcb == NULL || mcb->Owner == MEM_OWNER_FREE || owner == MEM_OWNER_FREE)
    {
        return false;
    }
    MCB_Create(segment - 1, mcb->Type, owner, mcb->Size, name);
    return true;
}

void MEM_FreeOwner(uint16_t owner)
{
    uint16_t segment = g_FirstBlock;

    for (;;)
    {
        MCB __far* mcb = MCB_At(segment);

        if (mcb->Owner == owner)
        {
            MCB_Create(segment, mcb->Type, MEM_OWNER_FREE, mcb->Size, NULL);
        }
        if (mcb->Type == MCB_TYPE_LAST)
        {
            break;
        }
        segment = MCB_Next(segment, mcb);
    }

    MEM_Coalesce();
}

bool MEM_GetBlockInfo(uint16_t segment, uint16_t* ownerOut, uint16_t* sizeOut)
{
    MCB __far* mcb = MEM_FindBlock(segment);

    if (mcb == NULL)
    {
        return false;
    }
    *ownerOut = mcb->Owner;
    *sizeOut = mcb->Size;
    return true;
}

uint16_t MEM_GetMaxSize(uint16_t segment)
{
    MCB __far* mcb = MEM_FindBlock(segment);
    uint32_t size;

    if (mcb == NULL)
    {
        return 0;
    }

    // it can grow into a free block right after it
    size = mcb->Size;
    if (mcb->Type != MCB_TYPE_LAST)
    {
        MCB __far* next = MCB_At(MCB_Next(segment - 1, mcb));
        if (next->Owner == MEM_OWNER_FREE)
        {
            size += 1 + next->Size;
        }
    }
    return (size > 0xFFFF) ? 0xFFFF : (uint16_t)size;
}

uint16_t MEM_GetLargestFree()
{
    uint32_t freeBytes, largestBytes;
    uint16_t blocks;

    MEM_GetStats(&freeBytes, &largestBytes, &blocks);
    return (uint16_t)(largestBytes / 16);
}

void MEM_GetStats(uint32_t* freeBytesOut, uint32_t* largestFreeBytesOut, uint16_t* blockCountOut)
{
    uint16_t segment = g_FirstBlock;
    uint16_t largest = 0;
    uint32_t total = 0;
    uint16_t blocks = 0;

    for (;;)
    {
        MCB __far* mcb = MCB_At(segment);

        blocks++;
        if (mcb->Owner == MEM_OWNER_FREE)
        {
            total += mcb->Size;
            if (mcb->Size > largest)
            {
                largest = mcb->Size;
            }
        }

        if (mcb->Type == MCB_TYPE_LAST)
        {
            break;
        }
        segment = MCB_Next(segment, mcb);
    }

    *freeBytesOut = total * 16;
    *largestFreeBytesOut = (uint32_t)largest * 16;
    *blockCountOut = blocks;
}

void MEM_Dump()
{
    uint16_t segment = g_FirstBlock;

    printf("Block  Data   Size        Owner  Name\n");
    for (;;)
    {
        MCB __far* mcb = MCB_At(segment);
        uint8_t i;

        printf("%04x   %04x   %7lu B   ", segment, segment + 1, (uint32_t)mcb->Size * 16);
        if (mcb->Owner == MEM_OWNER_FREE)
        {
            printf("free   ");
        }
        else
        {
            printf("%04x   ", mcb->Owner);
        }
        for (i = 0; i < MCB_NAME_LENGTH && mcb->Name[i]; i++)
        {
            putc(mcb->Name[i]);
        }
        putc('\n');

        if (mcb->Type == MCB_TYPE_LAST)
        {
            break;
        }
        segment = MCB_Next(segment, mcb);
    }
}

void __far* kmalloc(uint32_t bytes)
{
    uint32_t paragraphs = (bytes + 15) / 16;
    uint16_t segment;

    if (paragraphs == 0 || paragraphs > 0xFFFF)
    {
        return NULL;
    }

    segment = MEM_Allocate((uint16_t)paragraphs, MEM_OWNER_KERNEL, "kmalloc");
    return segment ? MK_FP(segment, 0) : NULL;
}

void kfree(void __far* pointer)
{
    if (pointer == NULL)
    {
        return;
    }

    // freeing something kmalloc didn't return means a bug somewhere, catch it right away
    if (FP_OFF(pointer) != 0 || !MEM_Free(FP_SEG(pointer)))
    {
        printf("\nkfree(%04x:%04x)", FP_SEG(pointer), FP_OFF(pointer));
        panic("kfree of a pointer kmalloc didn't return");
    }
}
