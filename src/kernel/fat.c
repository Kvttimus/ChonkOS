#include "fat.h"
#include "memory.h"
#include "x86.h"

#define SECTOR_SIZE             DISK_SECTOR_SIZE
#define ENTRIES_PER_SECTOR      (SECTOR_SIZE / sizeof(FAT_DirectoryEntry))
#define NO_SECTOR               0xFFFFFFFF

#define FAT12_END_MIN           0x0FF8
#define FAT16_END_MIN           0xFFF8
#define FAT_FREE                0x0000

#pragma pack(push, 1)

typedef struct
{
    uint8_t BootJumpInstruction[3];
    uint8_t OemIdentifier[8];
    uint16_t BytesPerSector;
    uint8_t SectorsPerCluster;
    uint16_t ReservedSectors;
    uint8_t FatCount;
    uint16_t DirEntryCount;
    uint16_t TotalSectors;
    uint8_t MediaDescriptorType;
    uint16_t SectorsPerFat;
    uint16_t SectorsPerTrack;
    uint16_t Heads;
    uint32_t HiddenSectors;
    uint32_t LargeSectorCount;

    // Extended Boot Record (same layout on FAT12 & FAT16)
    uint8_t DriveNumber;
    uint8_t Reserved;
    uint8_t Signature;
    uint32_t VolumeID;
    char VolumeLabel[11];
    char SystemID[8];
} FAT_BootSector;

#pragma pack(pop)

#define EBR_SIGNATURE           0x29

// ------------------------------------------------------------------------------------------------
// sectors
// ------------------------------------------------------------------------------------------------

FS_Status FAT_ReadSector(FAT_Volume* volume, uint32_t lba)
{
    if (volume->bufferValid && volume->bufferLba == lba)
    {
        return FS_OK;
    }

    volume->bufferValid = false;
    if (!DISK_ReadSectors(&volume->disk, lba, 1, volume->buffer))
    {
        return FS_IO_ERROR;
    }
    volume->bufferLba = lba;
    volume->bufferValid = true;
    return FS_OK;
}

FS_Status FAT_WriteSector(FAT_Volume* volume, uint32_t lba)
{
    if (!DISK_WriteSectors(&volume->disk, lba, 1, volume->buffer))
    {
        volume->bufferValid = false;
        return FS_IO_ERROR;
    }
    volume->bufferLba = lba;
    volume->bufferValid = true;
    return FS_OK;
}

// ------------------------------------------------------------------------------------------------
// mounting
// ------------------------------------------------------------------------------------------------

FS_Status FAT_Mount(FAT_Volume* volume, uint8_t drive, uint32_t partitionLba)
{
    FAT_BootSector* bpb = (FAT_BootSector*)volume->buffer;
    uint16_t rootSectors;
    uint32_t totalSectors;
    uint32_t clusters;
    uint8_t i;

    memset(volume, 0, sizeof(FAT_Volume));
    volume->fatCacheSector = NO_SECTOR;
    volume->partitionLba = partitionLba;

    if (!DISK_Initialize(&volume->disk, drive))
    {
        return FS_IO_ERROR;
    }
    if (FAT_ReadSector(volume, partitionLba) != FS_OK)
    {
        return FS_IO_ERROR;
    }

    // SectorsPerFat == 0 means FAT32, which needs 32-bit cluster numbers we don't support
    if (bpb->BytesPerSector != SECTOR_SIZE || bpb->SectorsPerCluster == 0
        || (bpb->SectorsPerCluster & (bpb->SectorsPerCluster - 1)) != 0
        || bpb->FatCount == 0 || bpb->SectorsPerFat == 0 || bpb->DirEntryCount == 0)
    {
        return FS_NOT_FAT;
    }

    volume->sectorsPerCluster = bpb->SectorsPerCluster;
    volume->fatCount = bpb->FatCount;
    volume->sectorsPerFat = bpb->SectorsPerFat;
    volume->rootEntryCount = bpb->DirEntryCount;

    // disk layout: [reserved sectors][FAT copies][root directory][data clusters...]
    rootSectors = (uint16_t)(((uint32_t)bpb->DirEntryCount * sizeof(FAT_DirectoryEntry) + SECTOR_SIZE - 1) / SECTOR_SIZE);
    volume->fatLba = partitionLba + bpb->ReservedSectors;
    volume->rootLba = volume->fatLba + (uint32_t)bpb->FatCount * bpb->SectorsPerFat;
    volume->dataLba = volume->rootLba + rootSectors;

    totalSectors = (bpb->TotalSectors != 0) ? bpb->TotalSectors : bpb->LargeSectorCount;
    if (totalSectors <= volume->dataLba - partitionLba)
    {
        return FS_NOT_FAT;
    }
    clusters = (totalSectors - (volume->dataLba - partitionLba)) / bpb->SectorsPerCluster;

    // the FAT type is decided only by the cluster count, never by the label in the boot sector
    if (clusters < 4085)
    {
        volume->fatType = 12;
    }
    else if (clusters < 65525)
    {
        volume->fatType = 16;
    }
    else
    {
        return FS_NOT_FAT;
    }
    volume->clusterCount = (uint16_t)clusters;
    volume->freeHint = 2;

    // floppies are read using the geometry they were formatted with
    if (drive < 0x80 && bpb->SectorsPerTrack != 0 && bpb->Heads != 0)
    {
        volume->disk.sectors = bpb->SectorsPerTrack;
        volume->disk.heads = bpb->Heads;
        volume->disk.cylinders = (uint16_t)((totalSectors + (uint32_t)bpb->SectorsPerTrack * bpb->Heads - 1)
                                            / ((uint32_t)bpb->SectorsPerTrack * bpb->Heads));
    }

    // volume label, w/o the space padding
    if (bpb->Signature == EBR_SIGNATURE)
    {
        memcpy(volume->label, bpb->VolumeLabel, 11);
    }
    else
    {
        memcpy(volume->label, "NO NAME    ", 11);
    }
    for (i = 11; i > 0 && volume->label[i - 1] == ' '; i--);
    volume->label[i] = '\0';

    return FS_OK;
}

// ------------------------------------------------------------------------------------------------
// FAT (the cluster chains)
// ------------------------------------------------------------------------------------------------

static FS_Status FAT_FlushFatCache(FAT_Volume* volume)
{
    uint8_t copy;

    if (!volume->fatCacheDirty)
    {
        return FS_OK;
    }

    // keep every copy of the FAT the same, disk checkers (& DOS) compare them
    for (copy = 0; copy < volume->fatCount; copy++)
    {
        uint32_t lba = volume->fatLba + (uint32_t)copy * volume->sectorsPerFat + volume->fatCacheSector;
        if (!DISK_WriteSectors(&volume->disk, lba, 1, volume->fatCache))
        {
            return FS_IO_ERROR;
        }
    }

    volume->fatCacheDirty = false;
    return FS_OK;
}

FS_Status FAT_Flush(FAT_Volume* volume)
{
    return FAT_FlushFatCache(volume);
}

static FS_Status FAT_LoadFatSector(FAT_Volume* volume, uint32_t sector)
{
    FS_Status status;

    if (sector == volume->fatCacheSector)
    {
        return FS_OK;
    }

    status = FAT_FlushFatCache(volume);
    if (status != FS_OK)
    {
        return status;
    }

    volume->fatCacheSector = NO_SECTOR;
    if (!DISK_ReadSectors(&volume->disk, volume->fatLba + sector, 1, volume->fatCache))
    {
        return FS_IO_ERROR;
    }
    volume->fatCacheSector = sector;
    return FS_OK;
}

// FAT12 entries are 1.5 bytes & can straddle 2 sectors, so the FAT is accessed byte by byte
static FS_Status FAT_GetFatByte(FAT_Volume* volume, uint32_t offset, uint8_t* byteOut)
{
    FS_Status status = FAT_LoadFatSector(volume, offset / SECTOR_SIZE);
    if (status == FS_OK)
    {
        *byteOut = volume->fatCache[(uint16_t)(offset % SECTOR_SIZE)];
    }
    return status;
}

static FS_Status FAT_SetFatByte(FAT_Volume* volume, uint32_t offset, uint8_t value)
{
    FS_Status status = FAT_LoadFatSector(volume, offset / SECTOR_SIZE);
    if (status == FS_OK)
    {
        volume->fatCache[(uint16_t)(offset % SECTOR_SIZE)] = value;
        volume->fatCacheDirty = true;
    }
    return status;
}

static uint32_t FAT_EntryOffset(FAT_Volume* volume, uint16_t cluster)
{
    // FAT12 packs 2 entries into 3 bytes, FAT16 entries are 2 bytes each
    return (volume->fatType == 12) ? (uint32_t)cluster + cluster / 2 : (uint32_t)cluster * 2;
}

// raw value of a FAT entry
static FS_Status FAT_GetEntry(FAT_Volume* volume, uint16_t cluster, uint16_t* valueOut)
{
    uint32_t offset = FAT_EntryOffset(volume, cluster);
    uint8_t low, high;
    FS_Status status;

    if ((status = FAT_GetFatByte(volume, offset, &low)) != FS_OK
        || (status = FAT_GetFatByte(volume, offset + 1, &high)) != FS_OK)
    {
        return status;
    }
    *valueOut = low | ((uint16_t)high << 8);

    if (volume->fatType == 12)
    {
        // odd clusters are in the top 12 bits, even clusters in the bottom 12 bits
        *valueOut = (cluster & 1) ? (*valueOut >> 4) : (*valueOut & 0x0FFF);
    }
    return FS_OK;
}

static FS_Status FAT_SetEntry(FAT_Volume* volume, uint16_t cluster, uint16_t value)
{
    uint32_t offset = FAT_EntryOffset(volume, cluster);
    uint8_t low, high;
    FS_Status status;

    if (volume->fatType == 16)
    {
        if ((status = FAT_SetFatByte(volume, offset, (uint8_t)value)) != FS_OK)
        {
            return status;
        }
        return FAT_SetFatByte(volume, offset + 1, (uint8_t)(value >> 8));
    }

    // FAT12: keep the 4 bits that belong to the neighbouring entry
    value &= 0x0FFF;
    if ((status = FAT_GetFatByte(volume, offset, &low)) != FS_OK
        || (status = FAT_GetFatByte(volume, offset + 1, &high)) != FS_OK)
    {
        return status;
    }

    if (cluster & 1)
    {
        low = (low & 0x0F) | (uint8_t)(value << 4);
        high = (uint8_t)(value >> 4);
    }
    else
    {
        low = (uint8_t)value;
        high = (high & 0xF0) | (uint8_t)(value >> 8);
    }

    if ((status = FAT_SetFatByte(volume, offset, low)) != FS_OK)
    {
        return status;
    }
    return FAT_SetFatByte(volume, offset + 1, high);
}

static bool FAT_IsValidCluster(FAT_Volume* volume, uint16_t cluster)
{
    // data clusters are numbered from 2
    return cluster >= 2 && (uint32_t)cluster < (uint32_t)volume->clusterCount + 2;
}

uint16_t FAT_ClusterBytes(FAT_Volume* volume)
{
    return volume->sectorsPerCluster * SECTOR_SIZE;
}

uint32_t FAT_ClusterLba(FAT_Volume* volume, uint16_t cluster)
{
    return volume->dataLba + (uint32_t)(cluster - 2) * volume->sectorsPerCluster;
}

FS_Status FAT_GetNextCluster(FAT_Volume* volume, uint16_t cluster, uint16_t* nextOut)
{
    uint16_t value;
    FS_Status status = FAT_GetEntry(volume, cluster, &value);

    if (status != FS_OK)
    {
        return status;
    }

    if (value >= ((volume->fatType == 12) ? FAT12_END_MIN : FAT16_END_MIN))
    {
        *nextOut = FAT_CLUSTER_END;
        return FS_OK;
    }

    // free, bad (FF7/FFF7) or out of range clusters in the middle of a chain = broken chain
    if (!FAT_IsValidCluster(volume, value))
    {
        return FS_CORRUPT;
    }

    *nextOut = value;
    return FS_OK;
}

FS_Status FAT_AllocateCluster(FAT_Volume* volume, uint16_t previous, bool zero, uint16_t* clusterOut)
{
    uint16_t i;
    uint16_t cluster = 0;
    uint16_t value;
    FS_Status status;

    // search starts where the last allocation left off, so files mostly end up contiguous
    for (i = 0; i < volume->clusterCount; i++)
    {
        uint16_t candidate = 2 + (uint16_t)(((uint32_t)volume->freeHint - 2 + i) % volume->clusterCount);

        if ((status = FAT_GetEntry(volume, candidate, &value)) != FS_OK)
        {
            return status;
        }
        if (value == FAT_FREE)
        {
            cluster = candidate;
            break;
        }
    }

    if (cluster == 0)
    {
        return FS_DISK_FULL;
    }

    // mark it as the end of the chain first, then link it, so the chain is never left pointing
    // at a cluster that's still marked free
    if ((status = FAT_SetEntry(volume, cluster, (volume->fatType == 12) ? 0x0FFF : 0xFFFF)) != FS_OK)
    {
        return status;
    }
    if (previous != 0 && (status = FAT_SetEntry(volume, previous, cluster)) != FS_OK)
    {
        return status;
    }
    volume->freeHint = cluster + 1;

    // directories must start out empty (all entries 0 = end of directory)
    if (zero)
    {
        uint8_t s;

        memset(volume->buffer, 0, SECTOR_SIZE);
        for (s = 0; s < volume->sectorsPerCluster; s++)
        {
            if ((status = FAT_WriteSector(volume, FAT_ClusterLba(volume, cluster) + s)) != FS_OK)
            {
                return status;
            }
        }
    }

    *clusterOut = cluster;
    return FS_OK;
}

FS_Status FAT_FreeChain(FAT_Volume* volume, uint16_t first)
{
    uint16_t cluster = first;
    uint16_t freed = 0;
    FS_Status status;

    while (cluster != FAT_CLUSTER_END && cluster != 0)
    {
        uint16_t next;

        // a chain can't be longer than the disk, if it is the FAT loops back on itself
        if (!FAT_IsValidCluster(volume, cluster) || freed++ > volume->clusterCount)
        {
            return FS_CORRUPT;
        }

        if ((status = FAT_GetNextCluster(volume, cluster, &next)) != FS_OK
            || (status = FAT_SetEntry(volume, cluster, FAT_FREE)) != FS_OK)
        {
            return status;
        }

        if (cluster < volume->freeHint)
        {
            volume->freeHint = cluster;
        }
        cluster = next;
    }

    return FS_OK;
}

FS_Status FAT_CountFreeClusters(FAT_Volume* volume, uint16_t* countOut)
{
    uint16_t cluster;
    uint16_t count = 0;
    uint16_t value;
    FS_Status status;

    for (cluster = 2; (uint32_t)cluster < (uint32_t)volume->clusterCount + 2; cluster++)
    {
        if ((status = FAT_GetEntry(volume, cluster, &value)) != FS_OK)
        {
            return status;
        }
        if (value == FAT_FREE)
        {
            count++;
        }
    }

    *countOut = count;
    return FS_OK;
}

// ------------------------------------------------------------------------------------------------
// directories
// ------------------------------------------------------------------------------------------------

// finds the sector & byte offset of entry # index in a directory
static FS_Status FAT_LocateEntry(FAT_Volume* volume, uint16_t directory, uint16_t index,
                                 uint32_t* lbaOut, uint16_t* offsetOut)
{
    *offsetOut = (index % ENTRIES_PER_SECTOR) * sizeof(FAT_DirectoryEntry);

    if (directory == FAT_ROOT_DIRECTORY)
    {
        // the root directory is a fixed size area right before the data clusters
        if (index >= volume->rootEntryCount)
        {
            return FS_END;
        }
        *lbaOut = volume->rootLba + index / ENTRIES_PER_SECTOR;
    }
    else
    {
        // subdirectories are stored like files, in a cluster chain
        uint16_t entriesPerCluster = ENTRIES_PER_SECTOR * volume->sectorsPerCluster;
        uint16_t clusterIndex = index / entriesPerCluster;
        uint16_t cluster = directory;
        FS_Status status;

        while (clusterIndex-- > 0)
        {
            if ((status = FAT_GetNextCluster(volume, cluster, &cluster)) != FS_OK)
            {
                return status;
            }
            if (cluster == FAT_CLUSTER_END)
            {
                return FS_END;
            }
        }

        *lbaOut = FAT_ClusterLba(volume, cluster) + (index % entriesPerCluster) / ENTRIES_PER_SECTOR;
    }

    return FS_OK;
}

FS_Status FAT_ReadEntry(FAT_Volume* volume, uint16_t directory, uint16_t index, FAT_DirectoryEntry* entryOut)
{
    uint32_t lba;
    uint16_t offset;
    FS_Status status;

    if ((status = FAT_LocateEntry(volume, directory, index, &lba, &offset)) != FS_OK
        || (status = FAT_ReadSector(volume, lba)) != FS_OK)
    {
        return status;
    }

    memcpy(entryOut, volume->buffer + offset, sizeof(FAT_DirectoryEntry));
    return FS_OK;
}

FS_Status FAT_WriteEntry(FAT_Volume* volume, uint16_t directory, uint16_t index, const FAT_DirectoryEntry* entry)
{
    uint32_t lba;
    uint16_t offset;
    FS_Status status;

    if ((status = FAT_LocateEntry(volume, directory, index, &lba, &offset)) != FS_OK
        || (status = FAT_ReadSector(volume, lba)) != FS_OK)
    {
        return status;
    }

    memcpy(volume->buffer + offset, entry, sizeof(FAT_DirectoryEntry));
    return FAT_WriteSector(volume, lba);
}

bool FAT_IsHiddenEntry(const FAT_DirectoryEntry* entry)
{
    return (uint8_t)entry->Name[0] == FAT_ENTRY_END
        || (uint8_t)entry->Name[0] == FAT_ENTRY_DELETED
        || entry->Attributes == FAT_ATTRIBUTE_LFN
        || (entry->Attributes & FAT_ATTRIBUTE_VOLUME_ID);
}

FS_Status FAT_FindEntry(FAT_Volume* volume, uint16_t directory, const char* shortName,
                        FAT_DirectoryEntry* entryOut, uint16_t* indexOut)
{
    uint16_t index;
    FS_Status status;

    for (index = 0; ; index++)
    {
        if ((status = FAT_ReadEntry(volume, directory, index, entryOut)) != FS_OK)
        {
            return (status == FS_END) ? FS_NOT_FOUND : status;
        }

        if ((uint8_t)entryOut->Name[0] == FAT_ENTRY_END)
        {
            return FS_NOT_FOUND;                        // no more entries after this one
        }
        if (!FAT_IsHiddenEntry(entryOut) && memcmp(entryOut->Name, shortName, FAT_NAME_LENGTH) == 0)
        {
            *indexOut = index;
            return FS_OK;
        }
    }
}

FS_Status FAT_AddEntry(FAT_Volume* volume, uint16_t directory, const FAT_DirectoryEntry* entry, uint16_t* indexOut)
{
    FAT_DirectoryEntry existing;
    uint16_t index;
    FS_Status status;

    // reuse the first unused or deleted entry
    for (index = 0; ; index++)
    {
        status = FAT_ReadEntry(volume, directory, index, &existing);
        if (status == FS_END)
        {
            break;
        }
        if (status != FS_OK)
        {
            return status;
        }

        if ((uint8_t)existing.Name[0] == FAT_ENTRY_END || (uint8_t)existing.Name[0] == FAT_ENTRY_DELETED)
        {
            *indexOut = index;
            return FAT_WriteEntry(volume, directory, index, entry);
        }
    }

    // every entry is used. the root directory can't grow, subdirectories get another cluster
    if (directory == FAT_ROOT_DIRECTORY)
    {
        return FS_ROOT_FULL;
    }
    else
    {
        uint16_t last = directory;
        uint16_t next;
        uint16_t added;

        while ((status = FAT_GetNextCluster(volume, last, &next)) == FS_OK && next != FAT_CLUSTER_END)
        {
            last = next;
        }
        if (status != FS_OK
            || (status = FAT_AllocateCluster(volume, last, true, &added)) != FS_OK)
        {
            return status;
        }

        // index is now the first entry of the new cluster
        *indexOut = index;
        return FAT_WriteEntry(volume, directory, index, entry);
    }
}

FS_Status FAT_DeleteLongName(FAT_Volume* volume, uint16_t directory, uint16_t index)
{
    FAT_DirectoryEntry entry;
    FS_Status status;

    // the long file name pieces for an entry come right before it
    while (index > 0)
    {
        index--;
        if (FAT_ReadEntry(volume, directory, index, &entry) != FS_OK
            || entry.Attributes != FAT_ATTRIBUTE_LFN
            || (uint8_t)entry.Name[0] == FAT_ENTRY_DELETED)
        {
            break;
        }

        entry.Name[0] = (char)FAT_ENTRY_DELETED;
        if ((status = FAT_WriteEntry(volume, directory, index, &entry)) != FS_OK)
        {
            return status;
        }
    }
    return FS_OK;
}

FS_Status FAT_DeleteEntry(FAT_Volume* volume, uint16_t directory, uint16_t index)
{
    FAT_DirectoryEntry entry;
    FS_Status status;

    if ((status = FAT_ReadEntry(volume, directory, index, &entry)) != FS_OK)
    {
        return status;
    }

    entry.Name[0] = (char)FAT_ENTRY_DELETED;
    if ((status = FAT_WriteEntry(volume, directory, index, &entry)) != FS_OK)
    {
        return status;
    }

    // delete the long name too so its pieces don't get left behind w/o a file
    return FAT_DeleteLongName(volume, directory, index);
}

// ------------------------------------------------------------------------------------------------
// names & time
// ------------------------------------------------------------------------------------------------

static bool FAT_IsValidNameChar(char c)
{
    static const char allowed[] = "!#$%&'()-@^_`{}~";
    const char* a;

    if ((c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || (uint8_t)c >= 0x80)
    {
        return true;
    }
    for (a = allowed; *a; a++)
    {
        if (c == *a)
        {
            return true;
        }
    }
    return false;
}

bool FAT_MakeShortName(const char* name, uint16_t length, char* shortNameOut)
{
    uint16_t i;
    uint8_t baseLength = 0, extensionLength = 0;
    bool inExtension = false;

    memset(shortNameOut, ' ', FAT_NAME_LENGTH);

    // "." & ".." are real entries in every subdirectory
    if ((length == 1 || length == 2) && name[0] == '.' && name[length - 1] == '.')
    {
        memcpy(shortNameOut, name, length);
        return true;
    }

    for (i = 0; i < length; i++)
    {
        char c = name[i];

        if (c >= 'a' && c <= 'z')
        {
            c = c - 'a' + 'A';
        }

        if (c == '.')
        {
            if (inExtension || baseLength == 0)
            {
                return false;                           // 2 dots, or nothing before the dot
            }
            inExtension = true;
        }
        else if (!FAT_IsValidNameChar(c))
        {
            return false;
        }
        else if (inExtension)
        {
            if (extensionLength == 3)
            {
                return false;
            }
            shortNameOut[8 + extensionLength++] = c;
        }
        else
        {
            if (baseLength == 8)
            {
                return false;
            }
            shortNameOut[baseLength++] = c;
        }
    }

    // a first byte of E5 means deleted, the format stores a real E5 as 05
    if ((uint8_t)shortNameOut[0] == FAT_ENTRY_DELETED)
    {
        shortNameOut[0] = 0x05;
    }

    return baseLength > 0;
}

void FAT_ShortNameToString(const char* shortName, char* out)
{
    uint8_t i;

    for (i = 0; i < 8 && shortName[i] != ' '; i++)
    {
        *out++ = (i == 0 && shortName[0] == 0x05) ? (char)0xE5 : shortName[i];
    }
    if (shortName[8] != ' ')
    {
        *out++ = '.';
        for (i = 8; i < FAT_NAME_LENGTH && shortName[i] != ' '; i++)
        {
            *out++ = shortName[i];
        }
    }
    *out = '\0';
}

static uint8_t FAT_FromBcd(uint8_t bcd)
{
    return (bcd >> 4) * 10 + (bcd & 0x0F);
}

void FAT_GetTimestamp(uint16_t* dateOut, uint16_t* timeOut)
{
    uint32_t date = x86_RTC_GetDate();
    uint32_t time = x86_RTC_GetTime();
    uint16_t year;

    // FAT dates count years from 1980, which is also what we use on a PC/XT w/o a clock
    *dateOut = (0 << 9) | (1 << 5) | 1;
    *timeOut = 0;

    if (date != 0xFFFFFFFF)
    {
        year = FAT_FromBcd((uint8_t)(date >> 24)) * 100 + FAT_FromBcd((uint8_t)(date >> 16));
        if (year >= 1980 && year <= 2107)
        {
            *dateOut = ((year - 1980) << 9)
                     | ((uint16_t)FAT_FromBcd((uint8_t)(date >> 8)) << 5)
                     | FAT_FromBcd((uint8_t)date);
        }
    }

    if (time != 0xFFFFFFFF)
    {
        // FAT stores seconds / 2
        *timeOut = ((uint16_t)FAT_FromBcd((uint8_t)(time >> 16)) << 11)
                 | ((uint16_t)FAT_FromBcd((uint8_t)(time >> 8)) << 5)
                 | (FAT_FromBcd((uint8_t)time) / 2);
    }
}
