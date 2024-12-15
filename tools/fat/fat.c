#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef uint8_t bool;
#define true 1
#define false 0

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

    // Extended Boot Record
    uint8_t DriveNumber;
    uint8_t Reserved;
    uint8_t Signature;
    uint32_t VolumeID;              // serial number, value does not matter
    uint8_t VolumeLabel[11];        // 11 bytes, padded w/ spaces
    uint8_t SystemID[8];              
} __attribute__((packed)) BootSector;  
// modern compilers may add padding bytes to data structs to align to 4/8 bytes
// - used __attribute__ to disable this functionality to ensure that the boot sector 
//   structure matches with what's on the disk


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
} __attribute__((packed)) DirectoryEntry;


// Global variables
BootSector g_BootSector;
uint8_t* g_Fat = NULL;
DirectoryEntry* g_RootDirectory = NULL;
uint32_t g_RootDirectoryEnd;


// Reads data from the disk & stores data in a global variable
// return 1 = successfully read
// return 0 = unsuccessfully read
bool readBootSector(FILE* disk)
{
    return fread(&g_BootSector, sizeof(g_BootSector), 1, disk) > 0;
}

/*
Parameters
- disk - file name
- lba - sector number
- count - # of sectors to read
- bufferOut - pointer to where to store the data
*/
bool readSectors(FILE* disk, uint32_t lba, uint32_t count, void* bufferOut)
{
    bool ok = true;
    ok = ok && (fseek(disk, lba * g_BootSector.BytesPerSector, SEEK_SET) == 0);
    ok = ok && (fread(bufferOut, g_BootSector.BytesPerSector, count, disk) == count);
    return ok;
}

// Reads FAT into memory
bool readFat(FILE* disk)
{
    g_Fat = (uint8_t*) malloc(g_BootSector.SectorsPerFat * g_BootSector.BytesPerSector);
    return readSectors(disk, g_BootSector.ReservedSectors, g_BootSector.SectorsPerFat, g_Fat);
}


// Reads root directory
bool readRootDirectory(FILE* disk)
{
    uint32_t lba = g_BootSector.ReservedSectors + g_BootSector.SectorsPerFat * g_BootSector.FatCount;
    uint32_t size = sizeof(DirectoryEntry) * g_BootSector.DirEntryCount;
    uint32_t sectors = (size / g_BootSector.BytesPerSector);
    if (size % g_BootSector.BytesPerSector > 0)
    {
        sectors++;
    }

    g_RootDirectoryEnd = lba + sectors; 
    g_RootDirectory = (DirectoryEntry*) malloc(sectors * g_BootSector.BytesPerSector);
    return readSectors(disk, lba, sectors, g_RootDirectory);
}


// Find the file in the root directory
DirectoryEntry* findFile(const char* name)
{
    for (uint32_t i=0; i < g_BootSector.DirEntryCount; i++)
    {
        if (memcmp(name, g_RootDirectory[i].Name, 11) == 0)
        {
            return &g_RootDirectory[i];
        }
    }

    return NULL;
}


// Pass fileEntry as an input from the findFile function
bool readFile(DirectoryEntry* fileEntry, FILE* disk, uint8_t* outputBuffer)
{
    bool ok = true;
    uint16_t currentCluster = fileEntry->FirstClusterLow;

    do {
        // convert from cluster to a sector
        uint32_t lba = g_RootDirectoryEnd + (currentCluster - 2) * g_BootSector.SectorsPerCluster;
        ok = ok && readSectors(disk, lba, g_BootSector.SectorsPerCluster, outputBuffer);
        outputBuffer += g_BootSector.SectorsPerCluster * g_BootSector.BytesPerSector;

        // determine what the next cluster is
        uint32_t fatIndex = currentCluster * 3 / 2;
        if (currentCluster % 2 == 0)
        {
            currentCluster = (*(uint16_t*)(g_Fat + fatIndex)) & 0x0FFF;
        }
        else
        {
            currentCluster = (*(uint16_t*)(g_Fat + fatIndex)) >> 4;
        }
    } while (ok && currentCluster < 0xFF8);

    return ok;
}


int main(int argc, char** argv) 
{
    if (argc < 3)
    {
        printf("Syntax %s <disk image> <file name>\n", argv[0]);
        return -1;
    }

    // Read the boot sector
    FILE* disk = fopen(argv[1], "rb");
    if (!disk) 
    {
        fprintf(stderr, "Cannot open disk image %s! :(\n", argv[1]);
        return -1;
    }

    if (!readBootSector(disk)) 
    {
        fprintf(stderr, "Could not read boot sector! :(\n");
        return -2;
    }

    if (!readFat(disk))
    {
        fprintf(stderr, "Coult not read FAT! :(\n");
        free(g_Fat);
        return -3;
    }

    if (!readRootDirectory(disk))
    {
        fprintf(stderr, "Could not read root directory! :(\n");
        free(g_Fat);
        free(g_RootDirectory);
        return -4;
    }

    DirectoryEntry* fileEntry = findFile(argv[2]);
    if (!fileEntry)
    {
        fprintf(stderr, "Could not find file %s! :(\n", argv[2]);
        free(g_Fat);
        free(g_RootDirectory);
        return -5;
    }

    uint8_t* buffer = (uint8_t*) malloc(fileEntry->Size + g_BootSector.BytesPerSector);
    if (!readFile(fileEntry, disk, buffer))
    {
        fprintf(stderr, "Could not read file %s! :(\n", argv[2]);
        free(g_Fat);
        free(g_RootDirectory);
        free(buffer);
        return -6;
    }

    for (size_t i=0; i < fileEntry->Size; i++)
    {
        if (isprint(buffer[i])) fputc(buffer[i], stdout);
        else printf("<%02x>", buffer[i]);
    }
    printf("\n");

    free(buffer);
    free(g_Fat);
    free(g_RootDirectory);
    return 0;
}