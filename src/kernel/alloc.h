#pragma once
#include "stdint.h"
#include "stdbool.h"

// Conventional memory is managed in 16 byte paragraphs, as a chain of memory control blocks
// (MCBs) like DOS does. Each block is a 1 paragraph header followed by its data:
//
//   [MCB][data ...][MCB][data ...] ... [MCB 'Z'][data ...]
//
// so the next block is always at (block segment + 1 + size)

#define MEM_OWNER_FREE      0x0000
#define MEM_OWNER_KERNEL    0x0008          // DOS uses 8 for memory owned by the system

// Builds the chain over all free memory: low memory below the kernel, & everything from the
// end of the kernel up to the top of conventional memory
void MEM_Initialize();

// Allocates paragraphs * 16 bytes, returns the segment of the data (0 if there's no room).
// First fit, like DOS. name (up to 8 chars) shows up in MEM_Dump
uint16_t MEM_Allocate(uint16_t paragraphs, uint16_t owner, const char* name);

// segment is the one MEM_Allocate returned. Returns false if it isn't an allocated block
bool MEM_Free(uint16_t segment);

// Grows or shrinks a block in place, returns false if there isn't room to grow it
bool MEM_Resize(uint16_t segment, uint16_t paragraphs);

// changes who owns a block (programs own the blocks they run in, owner = their PSP segment)
bool MEM_SetOwner(uint16_t segment, uint16_t owner, const char* name);
// frees every block owner has, used when a program exits
void MEM_FreeOwner(uint16_t owner);
// size of the largest free block, in paragraphs
uint16_t MEM_GetLargestFree();

// owner & size (paragraphs) of an allocated block, false if segment isn't one
bool MEM_GetBlockInfo(uint16_t segment, uint16_t* ownerOut, uint16_t* sizeOut);
// how big MEM_Resize could make a block, in paragraphs
uint16_t MEM_GetMaxSize(uint16_t segment);

void MEM_GetStats(uint32_t* freeBytesOut, uint32_t* largestFreeBytesOut, uint16_t* blockCountOut);
void MEM_Dump();

// byte sized wrappers for kernel use, the pointer is always segment:0000
void __far* kmalloc(uint32_t bytes);
void kfree(void __far* pointer);
