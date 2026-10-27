#pragma once
#include "stdint.h"
#include "stdbool.h"

// where to start a loaded MZ executable
typedef struct
{
    uint16_t cs;
    uint16_t ip;
    uint16_t ss;
    uint16_t sp;
} MZ_StartInfo;

// Prepares an MZ (DOS .EXE) file that was already read into memory at loadSegment:0000:
// applies its relocations & zeroes its BSS. memoryAvailable = bytes free starting at loadSegment
bool MZ_Prepare(uint16_t loadSegment, uint32_t fileSize, uint32_t memoryAvailable, MZ_StartInfo* startOut);
