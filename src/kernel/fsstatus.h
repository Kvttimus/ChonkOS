#pragma once

// result of every file system call
typedef enum
{
    FS_OK = 0,
    FS_END,                     // no more directory entries
    FS_NOT_FOUND,
    FS_PATH_NOT_FOUND,          // a directory in the middle of the path doesn't exist
    FS_EXISTS,
    FS_NOT_DIRECTORY,
    FS_IS_DIRECTORY,
    FS_DIRECTORY_NOT_EMPTY,
    FS_DISK_FULL,
    FS_ROOT_FULL,               // FAT12/16 root directories have a fixed # of entries
    FS_BAD_NAME,
    FS_INVALID_DRIVE,
    FS_TOO_MANY_OPEN,
    FS_BAD_HANDLE,
    FS_ACCESS_DENIED,
    FS_IN_USE,
    FS_IO_ERROR,
    FS_CORRUPT,
    FS_NOT_FAT,
    FS_CURRENT_DIRECTORY,       // can't remove a drive's current directory
} FS_Status;

const char* FS_StatusString(FS_Status status);
