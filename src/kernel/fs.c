#include "fs.h"
#include "fat.h"
#include "disk.h"
#include "memory.h"
#include "stdio.h"
#include "string.h"

#define FS_MAX_DRIVES       4               // A: B: C: D:
#define SECTOR_SIZE         DISK_SECTOR_SIZE

// MBR partition table
#define MBR_PARTITION_TABLE 0x1BE
#define MBR_SIGNATURE       0xAA55
#define PARTITION_FAT12     0x01
#define PARTITION_FAT16_S   0x04            // FAT16 < 32MiB
#define PARTITION_FAT16     0x06
#define PARTITION_FAT16_LBA 0x0E

#pragma pack(push, 1)

typedef struct
{
    uint8_t Status;
    uint8_t ChsFirst[3];
    uint8_t Type;
    uint8_t ChsLast[3];
    uint32_t LbaFirst;
    uint32_t SectorCount;
} MBR_Partition;

#pragma pack(pop)

typedef struct
{
    bool mounted;
    FAT_Volume volume;
} FS_Drive;

typedef struct
{
    bool used;
    uint8_t flags;
    uint8_t drive;

    // where the file's directory entry is, to update its size & first cluster
    uint16_t directory;
    uint16_t entryIndex;

    uint16_t firstCluster;
    uint32_t size;
    uint32_t position;

    // last cluster we looked up, so reading/writing in order doesn't walk the chain from the
    // start every time
    uint16_t cachedCluster;
    uint16_t cachedClusterIndex;
} FS_File;

// result of looking up a path
typedef struct
{
    uint8_t drive;
    uint16_t parent;                        // directory the last part of the path is in
    char name[FAT_NAME_LENGTH];             // last part of the path, as a short name
    bool found;
    bool isRoot;                            // path is the root directory itself (has no entry)
    FAT_DirectoryEntry entry;
    uint16_t index;                         // entry index in parent
} FS_Path;

static FS_Drive g_Drives[FS_MAX_DRIVES];
static FS_File g_Files[FS_MAX_HANDLES];
static uint8_t g_DefaultDrive;

// current directory of each drive, w/o the drive letter: "\" or "\DOCS\SUB"
static char g_CurrentDirectory[FS_MAX_DRIVES][FS_PATH_MAX];

const char* FS_StatusString(FS_Status status)
{
    switch (status)
    {
        case FS_OK:                     return "OK";
        case FS_END:                    return "No more entries";
        case FS_NOT_FOUND:              return "File not found";
        case FS_PATH_NOT_FOUND:         return "Path not found";
        case FS_EXISTS:                 return "File already exists";
        case FS_NOT_DIRECTORY:          return "Not a directory";
        case FS_IS_DIRECTORY:           return "Is a directory";
        case FS_DIRECTORY_NOT_EMPTY:    return "Directory not empty";
        case FS_DISK_FULL:              return "Disk full";
        case FS_ROOT_FULL:              return "Root directory full";
        case FS_BAD_NAME:               return "Invalid file name";
        case FS_INVALID_DRIVE:          return "Invalid drive";
        case FS_TOO_MANY_OPEN:          return "Too many open files";
        case FS_BAD_HANDLE:             return "Invalid file handle";
        case FS_ACCESS_DENIED:          return "Access denied";
        case FS_IN_USE:                 return "File is in use";
        case FS_IO_ERROR:               return "Disk error";
        case FS_CORRUPT:                return "File system is corrupted";
        case FS_NOT_FAT:                return "Not a FAT12/FAT16 disk";
        case FS_CURRENT_DIRECTORY:      return "Can't remove the current directory";
    }
    return "Unknown error";
}

// ------------------------------------------------------------------------------------------------
// drives
// ------------------------------------------------------------------------------------------------

static FS_Status FS_MountHardDisk(uint8_t slot, uint8_t biosDrive)
{
    FAT_Volume* volume = &g_Drives[slot].volume;
    MBR_Partition partitions[4];
    uint8_t i;
    FS_Status status;

    // read the MBR through the volume's buffer
    if ((status = FAT_Mount(volume, biosDrive, 0)) == FS_OK)
    {
        return FS_OK;                               // no partition table, FAT right at sector 0
    }
    if (status == FS_IO_ERROR || FAT_ReadSector(volume, 0) != FS_OK)
    {
        return FS_IO_ERROR;
    }
    if (*(uint16_t*)(volume->buffer + 510) != MBR_SIGNATURE)
    {
        return FS_NOT_FAT;
    }
    memcpy(partitions, volume->buffer + MBR_PARTITION_TABLE, sizeof(partitions));

    // first FAT12/FAT16 partition
    for (i = 0; i < 4; i++)
    {
        uint8_t type = partitions[i].Type;

        if (type == PARTITION_FAT12 || type == PARTITION_FAT16_S || type == PARTITION_FAT16 || type == PARTITION_FAT16_LBA)
        {
            return FAT_Mount(volume, biosDrive, partitions[i].LbaFirst);
        }
    }
    return FS_NOT_FAT;
}

void FS_Initialize(uint8_t bootDrive)
{
    uint8_t hardDisks = DISK_GetHardDiskCount();
    uint8_t i;
    FS_Status status;

    memset(g_Drives, 0, sizeof(g_Drives));
    memset(g_Files, 0, sizeof(g_Files));
    g_DefaultDrive = 0;
    for (i = 0; i < FS_MAX_DRIVES; i++)
    {
        strcpy(g_CurrentDirectory[i], "\\");
    }

    // A: = the floppy we booted from
    if (bootDrive < 0x80)
    {
        status = FAT_Mount(&g_Drives[0].volume, bootDrive, 0);
        g_Drives[0].mounted = (status == FS_OK);
        if (status != FS_OK)
        {
            printf("A: (floppy %02xh) - %s\n", bootDrive, FS_StatusString(status));
        }
    }

    // C:, D: = hard disks
    if (hardDisks == 0)
    {
        printf("C: - no hard disk found\n");
    }
    for (i = 0; i < hardDisks && 2 + i < FS_MAX_DRIVES; i++)
    {
        status = FS_MountHardDisk(2 + i, 0x80 + i);
        g_Drives[2 + i].mounted = (status == FS_OK);
        if (status != FS_OK)
        {
            printf("%c: (hard disk %02xh) - %s\n", 'C' + i, 0x80 + i, FS_StatusString(status));
        }
    }
}

static int8_t FS_DriveIndex(char letter)
{
    if (letter >= 'a' && letter <= 'z')
    {
        letter = letter - 'a' + 'A';
    }
    if (letter < 'A' || letter >= 'A' + FS_MAX_DRIVES || !g_Drives[letter - 'A'].mounted)
    {
        return -1;
    }
    return letter - 'A';
}

bool FS_GetDriveInfo(char letter, FS_DriveInfo* infoOut)
{
    int8_t drive = FS_DriveIndex(letter);
    FAT_Volume* volume;

    if (drive < 0)
    {
        return false;
    }

    volume = &g_Drives[drive].volume;
    infoOut->letter = 'A' + drive;
    memcpy(infoOut->label, volume->label, sizeof(infoOut->label));
    infoOut->fatType = volume->fatType;
    infoOut->biosDrive = volume->disk.id;
    infoOut->totalBytes = (uint32_t)volume->clusterCount * FAT_ClusterBytes(volume);
    return true;
}

FS_Status FS_GetFreeSpace(char letter, uint32_t* freeBytesOut)
{
    int8_t drive = FS_DriveIndex(letter);
    uint16_t freeClusters;
    FS_Status status;

    if (drive < 0)
    {
        return FS_INVALID_DRIVE;
    }

    status = FAT_CountFreeClusters(&g_Drives[drive].volume, &freeClusters);
    if (status == FS_OK)
    {
        *freeBytesOut = (uint32_t)freeClusters * FAT_ClusterBytes(&g_Drives[drive].volume);
    }
    return status;
}

char FS_GetDefaultDrive()
{
    return 'A' + g_DefaultDrive;
}

FS_Status FS_SetDefaultDrive(char letter)
{
    int8_t drive = FS_DriveIndex(letter);

    if (drive < 0)
    {
        return FS_INVALID_DRIVE;
    }
    g_DefaultDrive = drive;
    return FS_OK;
}

// ------------------------------------------------------------------------------------------------
// paths
// ------------------------------------------------------------------------------------------------

static bool FS_IsSeparator(char c)
{
    return c == '\\' || c == '/';
}

// adds the directories in path to parts, handling "." & "..". false if the path is too long
static bool FS_SplitPath(const char* path, char parts[FS_PATH_DEPTH][FS_NAME_LENGTH], uint8_t* count)
{
    while (*path)
    {
        const char* start;
        uint8_t length, i;

        while (FS_IsSeparator(*path))
        {
            path++;
        }
        if (*path == '\0')
        {
            break;
        }

        start = path;
        while (*path && !FS_IsSeparator(*path))
        {
            path++;
        }
        length = (uint8_t)(path - start);

        if (length == 1 && start[0] == '.')
        {
            continue;
        }
        if (length == 2 && start[0] == '.' && start[1] == '.')
        {
            if (*count > 0)
            {
                (*count)--;
            }
            continue;
        }
        if (length >= FS_NAME_LENGTH || *count == FS_PATH_DEPTH)
        {
            return false;
        }

        for (i = 0; i < length; i++)
        {
            parts[*count][i] = toupper(start[i]);
        }
        parts[*count][length] = '\0';
        (*count)++;
    }
    return true;
}

bool FS_MakeFullPath(const char* path, char* out)
{
    char parts[FS_PATH_DEPTH][FS_NAME_LENGTH];
    uint8_t count = 0;
    uint8_t i;
    char drive;

    if (path[0] != '\0' && path[1] == ':')
    {
        drive = toupper(path[0]);
        path += 2;
    }
    else
    {
        drive = FS_GetDefaultDrive();
    }
    if (drive < 'A' || drive >= 'A' + FS_MAX_DRIVES)
    {
        return false;
    }

    // relative paths start from the drive's current directory
    if (!FS_IsSeparator(path[0]) && !FS_SplitPath(g_CurrentDirectory[drive - 'A'], parts, &count))
    {
        return false;
    }
    if (!FS_SplitPath(path, parts, &count))
    {
        return false;
    }

    out[0] = drive;
    out[1] = ':';
    out[2] = '\\';
    out[3] = '\0';
    for (i = 0; i < count; i++)
    {
        if (strlen(out) + strlen(parts[i]) + 2 > FS_PATH_MAX)
        {
            return false;
        }
        if (i > 0)
        {
            strcat(out, "\\");
        }
        strcat(out, parts[i]);
    }
    return true;
}

const char* FS_GetCurrentDirectory(char letter)
{
    int8_t drive = FS_DriveIndex(letter);
    return (drive < 0) ? 0 : g_CurrentDirectory[drive];
}

// true if path (a full path) is a drive's current directory, or a directory it's inside of
static bool FS_IsInCurrentDirectory(const char* path)
{
    const char* current = g_CurrentDirectory[path[0] - 'A'];
    uint16_t length = strlen(path + 2);

    return memcmp(current, path + 2, length) == 0 && (current[length] == '\0' || current[length] == '\\');
}

static FS_Status FS_Resolve(const char* inputPath, FS_Path* out)
{
    char fullPath[FS_PATH_MAX];
    const char* path = fullPath;
    FAT_Volume* volume;
    uint16_t directory = FAT_ROOT_DIRECTORY;
    bool anyParts = false;
    FS_Status status;

    memset(out, 0, sizeof(FS_Path));

    // relative paths -> "C:\FULL\PATH", so the walk below always starts at a root
    if (!FS_MakeFullPath(inputPath, fullPath))
    {
        return FS_BAD_NAME;
    }

    // drive
    if (path[0] != '\0' && path[1] == ':')
    {
        int8_t drive = FS_DriveIndex(path[0]);
        if (drive < 0)
        {
            return FS_INVALID_DRIVE;
        }
        out->drive = drive;
        path += 2;
    }
    else
    {
        out->drive = g_DefaultDrive;
        if (!g_Drives[out->drive].mounted)
        {
            return FS_INVALID_DRIVE;
        }
    }
    volume = &g_Drives[out->drive].volume;

    // walk the path 1 directory at a time, starting at the root
    for (;;)
    {
        const char* part;
        uint16_t length;

        while (FS_IsSeparator(*path))
        {
            path++;
        }
        if (*path == '\0')
        {
            // no parts at all = the root directory
            if (!anyParts)
            {
                out->isRoot = true;
                out->found = true;
                out->parent = FAT_ROOT_DIRECTORY;
                out->entry.Attributes = FAT_ATTRIBUTE_DIRECTORY;
                out->entry.FirstCluster = FAT_ROOT_DIRECTORY;
            }
            return FS_OK;
        }

        // the previous part has more after it, so it must be a directory to look in
        if (out->found && !out->isRoot)
        {
            if (!(out->entry.Attributes & FAT_ATTRIBUTE_DIRECTORY))
            {
                return FS_PATH_NOT_FOUND;
            }
            directory = out->entry.FirstCluster;        // ".." in a top level dir is 0 = root
        }
        else if (anyParts && !out->found)
        {
            return FS_PATH_NOT_FOUND;                   // previous part doesn't exist
        }

        anyParts = true;
        part = path;
        while (*path != '\0' && !FS_IsSeparator(*path))
        {
            path++;
        }
        length = (uint16_t)(path - part);

        if (!FAT_MakeShortName(part, length, out->name))
        {
            return FS_BAD_NAME;
        }

        out->parent = directory;
        out->isRoot = false;

        // the root directory doesn't have "." & ".." entries, they'd mean the root itself
        if (directory == FAT_ROOT_DIRECTORY && part[0] == '.')
        {
            out->found = true;
            out->isRoot = true;
            out->entry.Attributes = FAT_ATTRIBUTE_DIRECTORY;
            out->entry.FirstCluster = FAT_ROOT_DIRECTORY;
            continue;
        }

        status = FAT_FindEntry(volume, directory, out->name, &out->entry, &out->index);
        if (status == FS_OK)
        {
            out->found = true;
        }
        else if (status == FS_NOT_FOUND)
        {
            out->found = false;
        }
        else
        {
            return status;
        }
    }
}

// ------------------------------------------------------------------------------------------------
// files
// ------------------------------------------------------------------------------------------------

static FS_File* FS_GetFile(FS_Handle handle)
{
    if (handle < 0 || handle >= FS_MAX_HANDLES || !g_Files[handle].used)
    {
        return 0;
    }
    return &g_Files[handle];
}

static bool FS_IsOpen(uint8_t drive, uint16_t directory, uint16_t index)
{
    uint8_t i;

    for (i = 0; i < FS_MAX_HANDLES; i++)
    {
        if (g_Files[i].used && g_Files[i].drive == drive && g_Files[i].directory == directory
            && g_Files[i].entryIndex == index)
        {
            return true;
        }
    }
    return false;
}

static FS_Status FS_UpdateEntry(FS_File* file)
{
    FAT_Volume* volume = &g_Drives[file->drive].volume;
    FAT_DirectoryEntry entry;
    FS_Status status;

    if ((status = FAT_ReadEntry(volume, file->directory, file->entryIndex, &entry)) != FS_OK)
    {
        return status;
    }

    entry.FirstCluster = file->firstCluster;
    entry.Size = file->size;
    entry.Attributes |= FAT_ATTRIBUTE_ARCHIVE;          // "changed since the last backup"
    FAT_GetTimestamp(&entry.ModifiedDate, &entry.ModifiedTime);
    entry.AccessedDate = entry.ModifiedDate;

    if ((status = FAT_WriteEntry(volume, file->directory, file->entryIndex, &entry)) != FS_OK)
    {
        return status;
    }
    return FAT_Flush(volume);
}

FS_Status FS_Open(const char* path, uint8_t flags, FS_Handle* handleOut)
{
    FS_Path resolved;
    FAT_Volume* volume;
    FS_File* file = 0;
    FS_Handle handle;
    FS_Status status;

    for (handle = 0; handle < FS_MAX_HANDLES; handle++)
    {
        if (!g_Files[handle].used)
        {
            file = &g_Files[handle];
            break;
        }
    }
    if (file == 0)
    {
        return FS_TOO_MANY_OPEN;
    }

    if ((status = FS_Resolve(path, &resolved)) != FS_OK)
    {
        return status;
    }
    volume = &g_Drives[resolved.drive].volume;

    if (resolved.found)
    {
        if (resolved.entry.Attributes & FAT_ATTRIBUTE_DIRECTORY)
        {
            return FS_IS_DIRECTORY;
        }
        if ((flags & FS_WRITE) && (resolved.entry.Attributes & FAT_ATTRIBUTE_READ_ONLY))
        {
            return FS_ACCESS_DENIED;
        }

        // only 1 handle per file, so 2 handles can't fight over the size & clusters
        if (FS_IsOpen(resolved.drive, resolved.parent, resolved.index))
        {
            return FS_IN_USE;
        }

        if ((flags & FS_WRITE) && (flags & FS_TRUNCATE) && resolved.entry.FirstCluster != 0)
        {
            if ((status = FAT_FreeChain(volume, resolved.entry.FirstCluster)) != FS_OK)
            {
                return status;
            }
            resolved.entry.FirstCluster = 0;
            resolved.entry.Size = 0;
            if ((status = FAT_WriteEntry(volume, resolved.parent, resolved.index, &resolved.entry)) != FS_OK
                || (status = FAT_Flush(volume)) != FS_OK)
            {
                return status;
            }
        }
    }
    else
    {
        FAT_DirectoryEntry entry;

        if (!(flags & FS_CREATE) || !(flags & FS_WRITE))
        {
            return FS_NOT_FOUND;
        }

        // new empty file, it gets clusters when something is written to it
        memset(&entry, 0, sizeof(entry));
        memcpy(entry.Name, resolved.name, FAT_NAME_LENGTH);
        entry.Attributes = FAT_ATTRIBUTE_ARCHIVE;
        FAT_GetTimestamp(&entry.CreatedDate, &entry.CreatedTime);
        entry.ModifiedDate = entry.AccessedDate = entry.CreatedDate;
        entry.ModifiedTime = entry.CreatedTime;

        // (adding an entry can grow a subdirectory by a cluster, so flush the FAT too)
        if ((status = FAT_AddEntry(volume, resolved.parent, &entry, &resolved.index)) != FS_OK
            || (status = FAT_Flush(volume)) != FS_OK)
        {
            return status;
        }
        resolved.entry = entry;
    }

    memset(file, 0, sizeof(FS_File));
    file->used = true;
    file->flags = flags;
    file->drive = resolved.drive;
    file->directory = resolved.parent;
    file->entryIndex = resolved.index;
    file->firstCluster = resolved.entry.FirstCluster;
    file->size = resolved.entry.Size;

    *handleOut = handle;
    return FS_OK;
}

// finds cluster # clusterIndex of the file, adding clusters to the end if allocate is set
static FS_Status FS_GetCluster(FS_File* file, uint16_t clusterIndex, bool allocate, uint16_t* clusterOut)
{
    FAT_Volume* volume = &g_Drives[file->drive].volume;
    FS_Status status;

    if (file->firstCluster == 0)
    {
        if (!allocate)
        {
            return FS_CORRUPT;
        }
        if ((status = FAT_AllocateCluster(volume, 0, false, &file->firstCluster)) != FS_OK)
        {
            return status;
        }
        file->cachedCluster = 0;
    }

    // start from the cached spot if it's not past where we want to go
    if (file->cachedCluster == 0 || file->cachedClusterIndex > clusterIndex)
    {
        file->cachedCluster = file->firstCluster;
        file->cachedClusterIndex = 0;
    }

    while (file->cachedClusterIndex < clusterIndex)
    {
        uint16_t next;

        if ((status = FAT_GetNextCluster(volume, file->cachedCluster, &next)) != FS_OK)
        {
            return status;
        }
        if (next == FAT_CLUSTER_END)
        {
            // the chain is shorter than the file size says
            if (!allocate)
            {
                return FS_CORRUPT;
            }
            if ((status = FAT_AllocateCluster(volume, file->cachedCluster, false, &next)) != FS_OK)
            {
                return status;
            }
        }

        file->cachedCluster = next;
        file->cachedClusterIndex++;
    }

    *clusterOut = file->cachedCluster;
    return FS_OK;
}

// moves a far pointer forward, keeping the offset small so it never wraps past 64KiB
static void __far* FS_Advance(const void __far* pointer, uint16_t bytes)
{
    uint32_t linear = (uint32_t)FP_SEG(pointer) * 16 + FP_OFF(pointer) + bytes;
    return MK_FP((uint16_t)(linear >> 4), (uint16_t)(linear & 0x0F));
}

FS_Status FS_Read(FS_Handle handle, void __far* buffer, uint16_t count, uint16_t* readOut)
{
    FS_File* file = FS_GetFile(handle);
    FAT_Volume* volume;
    uint16_t clusterBytes;
    FS_Status status;

    *readOut = 0;
    if (file == 0)
    {
        return FS_BAD_HANDLE;
    }
    if (!(file->flags & FS_READ))
    {
        return FS_ACCESS_DENIED;
    }

    volume = &g_Drives[file->drive].volume;
    clusterBytes = FAT_ClusterBytes(volume);

    if ((uint32_t)count > file->size - file->position)
    {
        count = (uint16_t)(file->size - file->position);
    }

    buffer = FS_Advance(buffer, 0);
    while (count > 0)
    {
        uint16_t offsetInCluster = (uint16_t)(file->position % clusterBytes);
        uint16_t offsetInSector = offsetInCluster % SECTOR_SIZE;
        uint16_t chunk = SECTOR_SIZE - offsetInSector;
        uint16_t cluster;

        if (chunk > count)
        {
            chunk = count;
        }

        if ((status = FS_GetCluster(file, (uint16_t)(file->position / clusterBytes), false, &cluster)) != FS_OK
            || (status = FAT_ReadSector(volume, FAT_ClusterLba(volume, cluster) + offsetInCluster / SECTOR_SIZE)) != FS_OK)
        {
            return status;
        }

        memcpy(buffer, volume->buffer + offsetInSector, chunk);
        buffer = FS_Advance(buffer, chunk);
        file->position += chunk;
        *readOut += chunk;
        count -= chunk;
    }

    return FS_OK;
}

FS_Status FS_Write(FS_Handle handle, const void __far* buffer, uint16_t count, uint16_t* writtenOut)
{
    FS_File* file = FS_GetFile(handle);
    FAT_Volume* volume;
    uint16_t clusterBytes;
    FS_Status status = FS_OK;

    *writtenOut = 0;
    if (file == 0)
    {
        return FS_BAD_HANDLE;
    }
    if (!(file->flags & FS_WRITE))
    {
        return FS_ACCESS_DENIED;
    }

    volume = &g_Drives[file->drive].volume;
    clusterBytes = FAT_ClusterBytes(volume);

    buffer = FS_Advance(buffer, 0);
    while (count > 0)
    {
        uint16_t offsetInCluster = (uint16_t)(file->position % clusterBytes);
        uint16_t offsetInSector = offsetInCluster % SECTOR_SIZE;
        uint16_t chunk = SECTOR_SIZE - offsetInSector;
        uint32_t sectorStart = file->position - offsetInSector;
        uint32_t lba;
        uint16_t cluster;

        if (chunk > count)
        {
            chunk = count;
        }

        if ((status = FS_GetCluster(file, (uint16_t)(file->position / clusterBytes), true, &cluster)) != FS_OK)
        {
            break;
        }
        lba = FAT_ClusterLba(volume, cluster) + offsetInCluster / SECTOR_SIZE;

        // writing part of a sector: keep the rest of it if it has file data in it
        if (chunk < SECTOR_SIZE)
        {
            if (sectorStart < file->size)
            {
                if ((status = FAT_ReadSector(volume, lba)) != FS_OK)
                {
                    break;
                }
            }
            else
            {
                memset(volume->buffer, 0, SECTOR_SIZE);
            }
        }

        memcpy(volume->buffer + offsetInSector, buffer, chunk);
        if ((status = FAT_WriteSector(volume, lba)) != FS_OK)
        {
            break;
        }

        buffer = FS_Advance(buffer, chunk);
        file->position += chunk;
        if (file->position > file->size)
        {
            file->size = file->position;
        }
        *writtenOut += chunk;
        count -= chunk;
    }

    // save the new size & clusters right away, so the file is consistent even if the machine
    // is switched off w/o closing it. Keep the first error if there was one
    {
        FS_Status updateStatus = FS_UpdateEntry(file);
        return (status != FS_OK) ? status : updateStatus;
    }
}

FS_Status FS_Seek(FS_Handle handle, uint32_t position)
{
    FS_File* file = FS_GetFile(handle);

    if (file == 0)
    {
        return FS_BAD_HANDLE;
    }

    file->position = (position > file->size) ? file->size : position;
    return FS_OK;
}

FS_Status FS_GetPosition(FS_Handle handle, uint32_t* positionOut)
{
    FS_File* file = FS_GetFile(handle);

    if (file == 0)
    {
        return FS_BAD_HANDLE;
    }

    *positionOut = file->position;
    return FS_OK;
}

FS_Status FS_GetSize(FS_Handle handle, uint32_t* sizeOut)
{
    FS_File* file = FS_GetFile(handle);

    if (file == 0)
    {
        return FS_BAD_HANDLE;
    }

    *sizeOut = file->size;
    return FS_OK;
}

FS_Status FS_Close(FS_Handle handle)
{
    FS_File* file = FS_GetFile(handle);

    if (file == 0)
    {
        return FS_BAD_HANDLE;
    }

    file->used = false;
    return FS_OK;
}

// ------------------------------------------------------------------------------------------------
// directories
// ------------------------------------------------------------------------------------------------

FS_Status FS_Delete(const char* path)
{
    FS_Path resolved;
    FAT_Volume* volume;
    FS_Status status;

    if ((status = FS_Resolve(path, &resolved)) != FS_OK)
    {
        return status;
    }
    if (!resolved.found)
    {
        return FS_NOT_FOUND;
    }
    if (resolved.entry.Attributes & FAT_ATTRIBUTE_DIRECTORY)
    {
        return FS_IS_DIRECTORY;
    }
    if (resolved.entry.Attributes & (FAT_ATTRIBUTE_READ_ONLY | FAT_ATTRIBUTE_SYSTEM))
    {
        return FS_ACCESS_DENIED;
    }
    if (FS_IsOpen(resolved.drive, resolved.parent, resolved.index))
    {
        return FS_IN_USE;
    }

    volume = &g_Drives[resolved.drive].volume;
    if ((status = FAT_FreeChain(volume, resolved.entry.FirstCluster)) != FS_OK
        || (status = FAT_DeleteEntry(volume, resolved.parent, resolved.index)) != FS_OK)
    {
        return status;
    }
    return FAT_Flush(volume);
}

FS_Status FS_Rename(const char* path, const char* newName)
{
    FS_Path resolved;
    FAT_Volume* volume;
    FAT_DirectoryEntry existing;
    char shortName[FAT_NAME_LENGTH];
    uint16_t existingIndex;
    FS_Status status;

    if ((status = FS_Resolve(path, &resolved)) != FS_OK)
    {
        return status;
    }
    if (!resolved.found)
    {
        return FS_NOT_FOUND;
    }
    if (resolved.isRoot || resolved.name[0] == '.')
    {
        return FS_ACCESS_DENIED;
    }

    // the new name is just a name, renaming doesn't move files between directories
    if (!FAT_MakeShortName(newName, strlen(newName), shortName) || shortName[0] == '.')
    {
        return FS_BAD_NAME;
    }
    if (FS_IsOpen(resolved.drive, resolved.parent, resolved.index))
    {
        return FS_IN_USE;
    }

    volume = &g_Drives[resolved.drive].volume;
    status = FAT_FindEntry(volume, resolved.parent, shortName, &existing, &existingIndex);
    if (status == FS_OK)
    {
        return FS_EXISTS;
    }
    if (status != FS_NOT_FOUND)
    {
        return status;
    }

    // the old long name would still describe the old name, drop it
    memcpy(resolved.entry.Name, shortName, FAT_NAME_LENGTH);
    if ((status = FAT_WriteEntry(volume, resolved.parent, resolved.index, &resolved.entry)) != FS_OK)
    {
        return status;
    }
    return FAT_DeleteLongName(volume, resolved.parent, resolved.index);
}

FS_Status FS_MakeDirectory(const char* path)
{
    FS_Path resolved;
    FAT_Volume* volume;
    FAT_DirectoryEntry entry;
    uint16_t cluster;
    uint16_t index;
    FS_Status status;

    if ((status = FS_Resolve(path, &resolved)) != FS_OK)
    {
        return status;
    }
    if (resolved.found)
    {
        return FS_EXISTS;
    }
    volume = &g_Drives[resolved.drive].volume;

    // a new directory is 1 cluster holding just "." (itself) & ".." (its parent)
    if ((status = FAT_AllocateCluster(volume, 0, true, &cluster)) != FS_OK)
    {
        return status;
    }

    memset(&entry, 0, sizeof(entry));
    memset(entry.Name, ' ', FAT_NAME_LENGTH);
    entry.Attributes = FAT_ATTRIBUTE_DIRECTORY;
    FAT_GetTimestamp(&entry.CreatedDate, &entry.CreatedTime);
    entry.ModifiedDate = entry.AccessedDate = entry.CreatedDate;
    entry.ModifiedTime = entry.CreatedTime;

    entry.Name[0] = '.';
    entry.FirstCluster = cluster;
    if ((status = FAT_WriteEntry(volume, cluster, 0, &entry)) != FS_OK)
    {
        return status;
    }

    entry.Name[1] = '.';
    entry.FirstCluster = resolved.parent;               // 0 when the parent is the root
    if ((status = FAT_WriteEntry(volume, cluster, 1, &entry)) != FS_OK)
    {
        return status;
    }

    // & the entry for it in the parent directory
    memcpy(entry.Name, resolved.name, FAT_NAME_LENGTH);
    entry.FirstCluster = cluster;
    if ((status = FAT_AddEntry(volume, resolved.parent, &entry, &index)) != FS_OK)
    {
        FAT_FreeChain(volume, cluster);
        FAT_Flush(volume);
        return status;
    }

    return FAT_Flush(volume);
}

FS_Status FS_ChangeDirectory(const char* path)
{
    char fullPath[FS_PATH_MAX];
    FS_Directory directory;
    FS_Status status;

    if (!FS_MakeFullPath(path, fullPath))
    {
        return FS_BAD_NAME;
    }
    if ((status = FS_OpenDirectory(fullPath, &directory)) != FS_OK)
    {
        return status;
    }

    // like DOS, "cd C:\X" changes C:'s current directory but not the current drive
    strcpy(g_CurrentDirectory[fullPath[0] - 'A'], fullPath + 2);
    return FS_OK;
}

FS_Status FS_RemoveDirectory(const char* path)
{
    char fullPath[FS_PATH_MAX];
    FS_Path resolved;
    FAT_Volume* volume;
    FAT_DirectoryEntry entry;
    uint16_t directory;
    uint16_t index;
    FS_Status status;

    if ((status = FS_Resolve(path, &resolved)) != FS_OK)
    {
        return status;
    }
    if (!resolved.found)
    {
        return FS_NOT_FOUND;
    }
    if (!(resolved.entry.Attributes & FAT_ATTRIBUTE_DIRECTORY))
    {
        return FS_NOT_DIRECTORY;
    }
    if (resolved.isRoot || resolved.name[0] == '.')
    {
        return FS_ACCESS_DENIED;                        // can't remove the root, "." or ".."
    }

    // can't pull the current directory (or one it's inside of) out from under the shell or a program
    FS_MakeFullPath(path, fullPath);
    if (FS_IsInCurrentDirectory(fullPath))
    {
        return FS_CURRENT_DIRECTORY;
    }

    volume = &g_Drives[resolved.drive].volume;
    directory = resolved.entry.FirstCluster;

    // only "." & ".." may be left in it
    for (index = 2; ; index++)
    {
        status = FAT_ReadEntry(volume, directory, index, &entry);
        if (status == FS_END || (status == FS_OK && (uint8_t)entry.Name[0] == FAT_ENTRY_END))
        {
            break;
        }
        if (status != FS_OK)
        {
            return status;
        }
        if ((uint8_t)entry.Name[0] != FAT_ENTRY_DELETED && entry.Attributes != FAT_ATTRIBUTE_LFN)
        {
            return FS_DIRECTORY_NOT_EMPTY;
        }
    }

    if ((status = FAT_FreeChain(volume, directory)) != FS_OK
        || (status = FAT_DeleteEntry(volume, resolved.parent, resolved.index)) != FS_OK)
    {
        return status;
    }
    return FAT_Flush(volume);
}

FS_Status FS_OpenDirectory(const char* path, FS_Directory* directoryOut)
{
    FS_Path resolved;
    FS_Status status;

    if ((status = FS_Resolve(path, &resolved)) != FS_OK)
    {
        return status;
    }
    if (!resolved.found)
    {
        return FS_PATH_NOT_FOUND;
    }
    if (!(resolved.entry.Attributes & FAT_ATTRIBUTE_DIRECTORY))
    {
        return FS_NOT_DIRECTORY;
    }

    directoryOut->drive = resolved.drive;
    directoryOut->cluster = resolved.entry.FirstCluster;
    directoryOut->index = 0;
    return FS_OK;
}

FS_Status FS_ReadDirectory(FS_Directory* directory, FS_DirectoryInfo* infoOut)
{
    FAT_Volume* volume = &g_Drives[directory->drive].volume;
    FAT_DirectoryEntry entry;
    FS_Status status;

    for (;;)
    {
        if ((status = FAT_ReadEntry(volume, directory->cluster, directory->index, &entry)) != FS_OK)
        {
            return status;
        }
        if ((uint8_t)entry.Name[0] == FAT_ENTRY_END)
        {
            return FS_END;
        }
        directory->index++;

        if (!FAT_IsHiddenEntry(&entry))
        {
            FAT_ShortNameToString(entry.Name, infoOut->name);
            infoOut->attributes = entry.Attributes;
            infoOut->size = entry.Size;
            infoOut->date = entry.ModifiedDate;
            infoOut->time = entry.ModifiedTime;
            return FS_OK;
        }
    }
}
