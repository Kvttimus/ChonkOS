#include "memtest.h"
#include "stdint.h"
#include "stdbool.h"
#include "stdio.h"
#include "memory.h"
#include "alloc.h"

// stress test for the allocator: random allocations & frees, each block filled w/ a pattern
// that's checked before it's freed, then everything has to merge back to how it started
#define MEMTEST_SLOTS       24
#define MEMTEST_ROUNDS      3000

static uint16_t g_RandomState = 12345;

static uint16_t random()
{
    g_RandomState = g_RandomState * 25173 + 13849;
    return g_RandomState;
}

static uint8_t memtest_pattern(uint16_t segment, uint16_t index)
{
    return (uint8_t)(segment ^ (segment >> 8) ^ index ^ (index >> 4));
}

static void memtest_fill(uint16_t segment, uint16_t paragraphs)
{
    uint16_t p, i;

    for (p = 0; p < paragraphs; p++)
    {
        uint8_t __far* data = (uint8_t __far*)MK_FP(segment + p, 0);
        for (i = 0; i < 16; i++)
        {
            data[i] = memtest_pattern(segment, p * 16 + i);
        }
    }
}

static bool memtest_check(uint16_t segment, uint16_t paragraphs)
{
    uint16_t p, i;

    for (p = 0; p < paragraphs; p++)
    {
        uint8_t __far* data = (uint8_t __far*)MK_FP(segment + p, 0);
        for (i = 0; i < 16; i++)
        {
            if (data[i] != memtest_pattern(segment, p * 16 + i))
            {
                printf("memtest: block %04x was overwritten at paragraph %u\n", segment, p);
                return false;
            }
        }
    }
    return true;
}

void memtest()
{
    uint16_t segments[MEMTEST_SLOTS] = { 0 };
    uint16_t sizes[MEMTEST_SLOTS];
    uint32_t freeBefore, largestBefore, freeAfter, largestAfter;
    uint16_t blocksBefore, blocksAfter;
    uint16_t round, slot, allocations = 0, failures = 0, resizes = 0;
    uint16_t big;
    bool ok = true;

    MEM_GetStats(&freeBefore, &largestBefore, &blocksBefore);

    for (round = 0; round < MEMTEST_ROUNDS && ok; round++)
    {
        slot = random() % MEMTEST_SLOTS;

        if (segments[slot] != 0)
        {
            ok = memtest_check(segments[slot], sizes[slot]);

            // sometimes resize instead of free, to test growing & shrinking in place
            if (ok && random() % 4 == 0)
            {
                uint16_t newSize = 1 + random() % 1024;
                if (MEM_Resize(segments[slot], newSize))
                {
                    sizes[slot] = newSize;
                    memtest_fill(segments[slot], newSize);
                    resizes++;
                }
                continue;
            }

            if (ok && !MEM_Free(segments[slot]))
            {
                printf("memtest: free of %04x failed\n", segments[slot]);
                ok = false;
            }
            segments[slot] = 0;
        }
        else
        {
            // mostly small blocks, sometimes up to 64KiB
            sizes[slot] = (random() % 8 == 0) ? 1 + random() % 4096 : 1 + random() % 64;
            segments[slot] = MEM_Allocate(sizes[slot], MEM_OWNER_KERNEL, "memtest");
            if (segments[slot] != 0)
            {
                memtest_fill(segments[slot], sizes[slot]);
                allocations++;
            }
            else
            {
                failures++;
            }
        }
    }

    for (slot = 0; slot < MEMTEST_SLOTS && ok; slot++)
    {
        if (segments[slot] != 0)
        {
            ok = memtest_check(segments[slot], sizes[slot]) && MEM_Free(segments[slot]);
        }
    }

    // a block bigger than 64KiB, using the biggest free block there is
    big = MEM_Allocate((uint16_t)(largestBefore / 16), MEM_OWNER_KERNEL, "big");
    if (ok && (big == 0 || !MEM_Free(big)))
    {
        printf("memtest: couldn't allocate the largest free block (%lu bytes)\n", largestBefore);
        ok = false;
    }

    // bad frees must be refused, not wreck the chain
    if (ok && (MEM_Free(0x1234) || MEM_Free(big)))
    {
        printf("memtest: a bad free was accepted\n");
        ok = false;
    }

    MEM_GetStats(&freeAfter, &largestAfter, &blocksAfter);
    if (ok && (freeAfter != freeBefore || largestAfter != largestBefore || blocksAfter != blocksBefore))
    {
        printf("memtest: chain didn't merge back (%lu free, %u blocks before; %lu free, %u blocks after)\n",
               freeBefore, blocksBefore, freeAfter, blocksAfter);
        ok = false;
    }

    printf("memtest: %u allocations, %u resizes, %u out of memory, largest %lu bytes - %s\n",
           allocations, resizes, failures, largestBefore, ok ? "PASS" : "FAIL");
}
