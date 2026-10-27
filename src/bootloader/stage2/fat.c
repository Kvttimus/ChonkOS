#include "fat.h"
#include "memory.h"
#include "stdio.h"

#define SECTOR_SIZE         DISK_SECTOR_SIZE
#define FAT_CLUSTER_END     0xFFFF          // returned by FAT_NextCluster at the end of a chain
#define FAT_CLUSTER_ERROR   0x0000          // returned by FAT_NextCluster if the FAT can't be read

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
    uint8_t VolumeLabel[11];
    uint8_t SystemID[8];
} FAT_BootSector;

#pragma pack(pop)

static DISK* g_Disk;
static FAT_BootSector g_BootSector;
static uint8_t g_FatType;               // 12 or 16
static uint16_t g_ClusterCount;
static uint32_t g_FatLba;
static uint32_t g_RootDirLba;
static uint16_t g_RootDirSectors;
static uint32_t g_DataLba;

// the FAT is too big to keep in memory on FAT16 (up to 128KiB), so cache 1 sector of it at a time
static uint8_t g_SectorBuffer[SECTOR_SIZE];
static uint8_t g_FatCache[SECTOR_SIZE];
static uint32_t g_FatCacheLba;

bool FAT_Initialize(DISK* disk, uint32_t partitionLba)
{
    uint32_t totalSectors;
    uint32_t clusters;

    g_Disk = disk;
    g_FatCacheLba = 0xFFFFFFFF;

    if (!DISK_ReadSectors(disk, partitionLba, 1, g_SectorBuffer))
    {
        puts("FAT: could not read boot sector\n");
        return false;
    }
    memcpy(&g_BootSector, g_SectorBuffer, sizeof(g_BootSector));

    if (g_BootSector.BytesPerSector != SECTOR_SIZE || g_BootSector.SectorsPerCluster == 0
        || g_BootSector.FatCount == 0 || g_BootSector.SectorsPerFat == 0)
    {
        // SectorsPerFat == 0 means FAT32, which we don't support
        puts("FAT: unsupported boot sector\n");
        return false;
    }

    // floppies are read using the geometry they were formatted with
    if (disk->id < 0x80 && g_BootSector.SectorsPerTrack != 0 && g_BootSector.Heads != 0)
    {
        disk->sectors = g_BootSector.SectorsPerTrack;
        disk->heads = g_BootSector.Heads;
    }

    // disk layout: [reserved sectors][FAT copies][root directory][data clusters...]
    g_FatLba = partitionLba + g_BootSector.ReservedSectors;
    g_RootDirLba = g_FatLba + (uint32_t)g_BootSector.FatCount * g_BootSector.SectorsPerFat;
    g_RootDirSectors = (uint16_t)(((uint32_t)g_BootSector.DirEntryCount * sizeof(FAT_DirectoryEntry)
                                   + SECTOR_SIZE - 1) / SECTOR_SIZE);
    g_DataLba = g_RootDirLba + g_RootDirSectors;

    totalSectors = (g_BootSector.TotalSectors != 0) ? g_BootSector.TotalSectors : g_BootSector.LargeSectorCount;
    clusters = (totalSectors - (g_DataLba - partitionLba)) / g_BootSector.SectorsPerCluster;

    // the FAT type is decided only by the cluster count, never by the label in the boot sector
    if (clusters < 4085)
    {
        g_FatType = 12;
    }
    else if (clusters < 65525)
    {
        g_FatType = 16;
    }
    else
    {
        puts("FAT: FAT32 is not supported\n");
        return false;
    }

    g_ClusterCount = (uint16_t)clusters;
    return true;
}

uint8_t FAT_GetType()
{
    return g_FatType;
}

uint16_t FAT_GetClusterCount()
{
    return g_ClusterCount;
}

static bool FAT_ReadFatByte(uint32_t offset, uint8_t* byteOut)
{
    uint32_t lba = g_FatLba + offset / SECTOR_SIZE;

    if (lba != g_FatCacheLba)
    {
        if (!DISK_ReadSectors(g_Disk, lba, 1, g_FatCache))
        {
            return false;
        }
        g_FatCacheLba = lba;
    }

    *byteOut = g_FatCache[(uint16_t)(offset % SECTOR_SIZE)];
    return true;
}

static uint16_t FAT_NextCluster(uint16_t cluster)
{
    uint32_t offset;
    uint8_t low, high;
    uint16_t value;

    // FAT12 packs 2 entries into 3 bytes, FAT16 entries are 2 bytes each
    if (g_FatType == 12)
    {
        offset = (uint32_t)cluster + cluster / 2;
    }
    else
    {
        offset = (uint32_t)cluster * 2;
    }

    // read byte by byte, a FAT12 entry can straddle 2 sectors
    if (!FAT_ReadFatByte(offset, &low) || !FAT_ReadFatByte(offset + 1, &high))
    {
        return FAT_CLUSTER_ERROR;
    }
    value = low | ((uint16_t)high << 8);

    if (g_FatType == 12)
    {
        // odd clusters are in the top 12 bits, even clusters in the bottom 12 bits
        value = (cluster & 1) ? (value >> 4) : (value & 0x0FFF);
        return (value >= 0x0FF8) ? FAT_CLUSTER_END : value;
    }

    return (value >= 0xFFF8) ? FAT_CLUSTER_END : value;
}

// "kernel.bin" -> "KERNEL  BIN"
static void FAT_ToShortName(const char* name, char shortName[11])
{
    uint8_t i;

    memset(shortName, ' ', 11);

    for (i = 0; *name && *name != '.' && i < 8; i++, name++)
    {
        shortName[i] = (*name >= 'a' && *name <= 'z') ? *name - 'a' + 'A' : *name;
    }

    // skip anything past 8 chars, up to the extension
    while (*name && *name != '.')
    {
        name++;
    }
    if (*name == '.')
    {
        name++;
    }

    for (i = 8; *name && i < 11; i++, name++)
    {
        shortName[i] = (*name >= 'a' && *name <= 'z') ? *name - 'a' + 'A' : *name;
    }
}

bool FAT_FindFile(const char* name, FAT_DirectoryEntry* entryOut)
{
    char shortName[11];
    FAT_DirectoryEntry* entries = (FAT_DirectoryEntry*)g_SectorBuffer;
    uint16_t sector;
    uint16_t i;

    FAT_ToShortName(name, shortName);

    // the root directory is a fixed size area on FAT12/16, read it a sector at a time
    for (sector = 0; sector < g_RootDirSectors; sector++)
    {
        if (!DISK_ReadSectors(g_Disk, g_RootDirLba + sector, 1, g_SectorBuffer))
        {
            puts("FAT: could not read root directory\n");
            return false;
        }

        for (i = 0; i < SECTOR_SIZE / sizeof(FAT_DirectoryEntry); i++)
        {
            if (entries[i].Name[0] == 0x00)
            {
                return false;                           // no more entries after this one
            }
            if (entries[i].Name[0] == 0xE5)
            {
                continue;                               // deleted file
            }
            if (entries[i].Attributes & (FAT_ATTRIBUTE_VOLUME_ID | FAT_ATTRIBUTE_DIRECTORY))
            {
                continue;                               // volume label, long file name or directory
            }

            if (memcmp(entries[i].Name, shortName, 11) == 0)
            {
                memcpy(entryOut, &entries[i], sizeof(FAT_DirectoryEntry));
                return true;
            }
        }
    }

    return false;
}

bool FAT_ReadFile(const FAT_DirectoryEntry* entry, void __far* buffer, uint32_t bufferSize)
{
    uint16_t cluster = entry->FirstClusterLow;
    uint8_t sectorsPerCluster = g_BootSector.SectorsPerCluster;
    uint32_t clusterBytes = (uint32_t)sectorsPerCluster * SECTOR_SIZE;
    uint32_t clustersLeft = (entry->Size + clusterBytes - 1) / clusterBytes;

    if (clustersLeft * clusterBytes > bufferSize)
    {
        puts("FAT: file is too big for the buffer\n");
        return false;
    }

    // read exactly as many clusters as the file size needs, so a corrupt FAT that loops
    // back on itself can't keep us reading forever
    for (; clustersLeft > 0; clustersLeft--)
    {
        if (cluster < 2 || cluster >= g_ClusterCount + 2)
        {
            puts("FAT: bad cluster chain\n");
            return false;
        }

        // first sector of a cluster = data start + (cluster - 2) * sectors per cluster
        if (!DISK_ReadSectors(g_Disk, g_DataLba + (uint32_t)(cluster - 2) * sectorsPerCluster,
                              sectorsPerCluster, buffer))
        {
            puts("FAT: could not read file\n");
            return false;
        }
        buffer = MK_FP(FP_SEG(buffer) + sectorsPerCluster * (SECTOR_SIZE / 16), FP_OFF(buffer));

        cluster = FAT_NextCluster(cluster);
    }

    return true;
}
