#pragma once
#include "stdint.h"
#include "stdbool.h"

#define DISK_SECTOR_SIZE    512

typedef struct
{
    uint8_t id;             // BIOS drive number (0x00 = first floppy, 0x80 = first hard disk)
    uint16_t cylinders;
    uint16_t sectors;       // sectors per track
    uint16_t heads;
} DISK;

bool DISK_Initialize(DISK* disk, uint8_t driveNumber);

// Reads sectors one at a time, so reads never cross a track or a 64KiB DMA boundary
// as long as dataOut is 512 byte aligned in physical memory
bool DISK_ReadSectors(DISK* disk, uint32_t lba, uint8_t count, void __far* dataOut);
