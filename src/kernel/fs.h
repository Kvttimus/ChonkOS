#pragma once
#include "stdint.h"
#include "stdbool.h"
#include "fsstatus.h"

#define FS_MAX_HANDLES      16
#define FS_NAME_LENGTH      13              // "FILENAME.EXT" + null
#define FS_PATH_MAX         128             // "C:\DIR\...\FILE.EXT" + null
#define FS_PATH_DEPTH       16              // max directories in a path

// FS_Open flags
#define FS_READ             0x01
#define FS_WRITE            0x02
#define FS_CREATE           0x04            // create the file if it doesn't exist
#define FS_TRUNCATE         0x08            // empty the file when opening it

typedef int16_t FS_Handle;

typedef struct
{
    char name[FS_NAME_LENGTH];
    uint8_t attributes;                     // FAT_ATTRIBUTE_*
    uint32_t size;
    uint16_t date;                          // FAT format: bits 15-9 year - 1980, 8-5 month, 4-0 day
    uint16_t time;                          // FAT format: bits 15-11 hours, 10-5 minutes, 4-0 seconds / 2
} FS_DirectoryInfo;

// directory listing position, from FS_OpenDirectory
typedef struct
{
    uint8_t drive;
    uint16_t cluster;
    uint16_t index;
} FS_Directory;

typedef struct
{
    char letter;
    char label[12];
    uint8_t fatType;
    uint8_t biosDrive;
    uint32_t totalBytes;
} FS_DriveInfo;

// Mounts the boot floppy as A: & the first FAT partition of each hard disk as C:, D:
void FS_Initialize(uint8_t bootDrive);

bool FS_GetDriveInfo(char letter, FS_DriveInfo* infoOut);
FS_Status FS_GetFreeSpace(char letter, uint32_t* freeBytesOut);

// paths look like "C:\DIR\FILE.TXT" or "C:/dir/file.txt". w/o a drive letter the default drive is
// used, & paths that don't start w/ "\" are relative to that drive's current directory
char FS_GetDefaultDrive();
FS_Status FS_SetDefaultDrive(char letter);

// "file.txt" -> "C:\DOCS\FILE.TXT", "..\x" -> "C:\X". false if the path is too long or bad
bool FS_MakeFullPath(const char* path, char* out);
FS_Status FS_ChangeDirectory(const char* path);
// "\" or "\DOCS\SUB" (no drive letter), or 0 if the drive isn't mounted
const char* FS_GetCurrentDirectory(char letter);

FS_Status FS_Open(const char* path, uint8_t flags, FS_Handle* handleOut);
FS_Status FS_Read(FS_Handle handle, void __far* buffer, uint16_t count, uint16_t* readOut);
FS_Status FS_Write(FS_Handle handle, const void __far* buffer, uint16_t count, uint16_t* writtenOut);
FS_Status FS_Seek(FS_Handle handle, uint32_t position);
FS_Status FS_GetPosition(FS_Handle handle, uint32_t* positionOut);
FS_Status FS_GetSize(FS_Handle handle, uint32_t* sizeOut);
FS_Status FS_Close(FS_Handle handle);

FS_Status FS_Delete(const char* path);
// renames a file or directory in place, newName is just a name ("NEW.TXT"), not a path
FS_Status FS_Rename(const char* path, const char* newName);
FS_Status FS_MakeDirectory(const char* path);
FS_Status FS_RemoveDirectory(const char* path);

// FS_ReadDirectory returns FS_END after the last entry
FS_Status FS_OpenDirectory(const char* path, FS_Directory* directoryOut);
FS_Status FS_ReadDirectory(FS_Directory* directory, FS_DirectoryInfo* infoOut);
