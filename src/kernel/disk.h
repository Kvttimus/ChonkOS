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

// # of hard disks the BIOS found
uint8_t DISK_GetHardDiskCount();

// Transfers go through a bounce buffer 1 sector at a time, so data can be anywhere in memory
// (the floppy's DMA can't cross a 64KiB boundary) & can't be split across a track
bool DISK_ReadSectors(DISK* disk, uint32_t lba, uint16_t count, void __far* data);
bool DISK_WriteSectors(DISK* disk, uint32_t lba, uint16_t count, const void __far* data);
