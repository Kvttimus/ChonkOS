#pragma once
#include "stdint.h"
#include "stdbool.h"
#include "disk.h"

#pragma pack(push, 1)

// directory entry structure
typedef struct
{
    uint8_t Name[11];
    uint8_t Attributes;
    uint8_t Reserved;
    uint8_t CreatedTimeTenths;
    uint16_t CreatedTime;
    uint16_t CreatedDate;
    uint16_t AccessedDate;
    uint16_t FirstClusterHigh;
    uint16_t ModifiedTime;
    uint16_t ModifiedDate;
    uint16_t FirstClusterLow;
    uint32_t Size;
} FAT_DirectoryEntry;

#pragma pack(pop)

#define FAT_ATTRIBUTE_READ_ONLY     0x01
#define FAT_ATTRIBUTE_HIDDEN        0x02
#define FAT_ATTRIBUTE_SYSTEM        0x04
#define FAT_ATTRIBUTE_VOLUME_ID     0x08
#define FAT_ATTRIBUTE_DIRECTORY     0x10
#define FAT_ATTRIBUTE_ARCHIVE       0x20

// Mounts the FAT12 or FAT16 volume starting at partitionLba (0 for a floppy / unpartitioned disk)
bool FAT_Initialize(DISK* disk, uint32_t partitionLba);

// 12 or 16
uint8_t FAT_GetType();
uint16_t FAT_GetClusterCount();

// Finds a file in the root directory, name is a normal "NAME.EXT" (case doesn't matter)
bool FAT_FindFile(const char* name, FAT_DirectoryEntry* entryOut);

// Reads a whole file into memory, whole clusters at a time, failing if they won't fit in bufferSize
bool FAT_ReadFile(const FAT_DirectoryEntry* entry, void __far* buffer, uint32_t bufferSize);
