#pragma once
#include "stdint.h"
#include "stdbool.h"
#include "disk.h"
#include "fsstatus.h"

#define FAT_NAME_LENGTH             11      // "NAME    EXT", no dot
#define FAT_ROOT_DIRECTORY          0       // directories are identified by their first cluster
#define FAT_CLUSTER_END             0xFFFF  // end of a cluster chain (normalized for FAT12 & 16)

#define FAT_ATTRIBUTE_READ_ONLY     0x01
#define FAT_ATTRIBUTE_HIDDEN        0x02
#define FAT_ATTRIBUTE_SYSTEM        0x04
#define FAT_ATTRIBUTE_VOLUME_ID     0x08
#define FAT_ATTRIBUTE_DIRECTORY     0x10
#define FAT_ATTRIBUTE_ARCHIVE       0x20
#define FAT_ATTRIBUTE_LFN           0x0F    // long file name piece, we only use the 8.3 names

#define FAT_ENTRY_END               0x00    // first name byte: this & every entry after is unused
#define FAT_ENTRY_DELETED           0xE5    // first name byte: deleted entry, can be reused

#pragma pack(push, 1)

typedef struct
{
    char Name[FAT_NAME_LENGTH];
    uint8_t Attributes;
    uint8_t Reserved;
    uint8_t CreatedTimeTenths;
    uint16_t CreatedTime;
    uint16_t CreatedDate;
    uint16_t AccessedDate;
    uint16_t FirstClusterHigh;              // FAT32 only, always 0 here
    uint16_t ModifiedTime;
    uint16_t ModifiedDate;
    uint16_t FirstCluster;
    uint32_t Size;
} FAT_DirectoryEntry;

#pragma pack(pop)

typedef struct
{
    DISK disk;
    uint32_t partitionLba;
    uint8_t fatType;                        // 12 or 16
    uint8_t sectorsPerCluster;
    uint8_t fatCount;
    uint16_t sectorsPerFat;
    uint16_t rootEntryCount;
    uint16_t clusterCount;
    uint32_t fatLba;
    uint32_t rootLba;
    uint32_t dataLba;
    char label[12];
    uint16_t freeHint;                      // where to start looking for a free cluster

    // the FAT is up to 128KiB on FAT16, so only 1 sector of it is kept in memory. changes are
    // written back to every copy of the FAT
    uint8_t fatCache[DISK_SECTOR_SIZE];
    uint32_t fatCacheSector;                // sector # within the FAT
    bool fatCacheDirty;

    // last sector read/written for directories & file data
    uint8_t buffer[DISK_SECTOR_SIZE];
    uint32_t bufferLba;
    bool bufferValid;
} FAT_Volume;

FS_Status FAT_Mount(FAT_Volume* volume, uint8_t drive, uint32_t partitionLba);

// writes any pending FAT changes to disk
FS_Status FAT_Flush(FAT_Volume* volume);

uint16_t FAT_ClusterBytes(FAT_Volume* volume);
uint32_t FAT_ClusterLba(FAT_Volume* volume, uint16_t cluster);
FS_Status FAT_GetNextCluster(FAT_Volume* volume, uint16_t cluster, uint16_t* nextOut);
FS_Status FAT_AllocateCluster(FAT_Volume* volume, uint16_t previous, bool zero, uint16_t* clusterOut);
FS_Status FAT_FreeChain(FAT_Volume* volume, uint16_t first);
FS_Status FAT_CountFreeClusters(FAT_Volume* volume, uint16_t* countOut);

// sector access through volume->buffer
FS_Status FAT_ReadSector(FAT_Volume* volume, uint32_t lba);
FS_Status FAT_WriteSector(FAT_Volume* volume, uint32_t lba);

// directory entries, by index within a directory. FS_END past the end of the directory
FS_Status FAT_ReadEntry(FAT_Volume* volume, uint16_t directory, uint16_t index, FAT_DirectoryEntry* entryOut);
FS_Status FAT_WriteEntry(FAT_Volume* volume, uint16_t directory, uint16_t index, const FAT_DirectoryEntry* entry);
FS_Status FAT_FindEntry(FAT_Volume* volume, uint16_t directory, const char* shortName,
                        FAT_DirectoryEntry* entryOut, uint16_t* indexOut);
FS_Status FAT_AddEntry(FAT_Volume* volume, uint16_t directory, const FAT_DirectoryEntry* entry, uint16_t* indexOut);
FS_Status FAT_DeleteEntry(FAT_Volume* volume, uint16_t directory, uint16_t index);
// deletes just the long file name pieces in front of an entry (used when renaming it)
FS_Status FAT_DeleteLongName(FAT_Volume* volume, uint16_t directory, uint16_t index);

// true for entries a directory listing should skip: unused, deleted, long name pieces, labels
bool FAT_IsHiddenEntry(const FAT_DirectoryEntry* entry);

// "readme.txt" -> "README  TXT", false if it isn't a valid 8.3 name
bool FAT_MakeShortName(const char* name, uint16_t length, char* shortNameOut);
// "README  TXT" -> "README.TXT"
void FAT_ShortNameToString(const char* shortName, char* out);

// current date & time in FAT format (1980-01-01 00:00 on machines w/o a clock)
void FAT_GetTimestamp(uint16_t* dateOut, uint16_t* timeOut);
