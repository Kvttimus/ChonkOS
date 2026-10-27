#include "disk.h"
#include "memory.h"
#include "x86.h"

#define DISK_READ_RETRIES   3

bool DISK_Initialize(DISK* disk, uint8_t driveNumber)
{
    uint8_t driveType;
    uint16_t cylinders, sectors, heads;

    disk->id = driveNumber;

    if (!x86_Disk_GetDriveParams(driveNumber, &driveType, &cylinders, &sectors, &heads))
    {
        // the original IBM PC BIOS doesn't support this for floppies
        if (driveNumber >= 0x80)
        {
            return false;
        }

        // assume 1.44MB geometry, good enough to read the boot sector (LBA 0 is CHS 0/0/1 on
        // every floppy), FAT_Initialize then replaces it w/ the geometry stored in the BPB
        cylinders = 80;
        sectors = 18;
        heads = 2;
    }

    disk->cylinders = cylinders;
    disk->sectors = sectors;
    disk->heads = heads;
    return true;
}

static void DISK_LbaToChs(DISK* disk, uint32_t lba, uint16_t* cylinderOut, uint16_t* sectorOut, uint16_t* headOut)
{
    // sector = (LBA % sectors per track) + 1
    *sectorOut = (uint16_t)(lba % disk->sectors) + 1;

    // cylinder = (LBA / sectors per track) / heads
    *cylinderOut = (uint16_t)((lba / disk->sectors) / disk->heads);

    // head = (LBA / sectors per track) % heads
    *headOut = (uint16_t)((lba / disk->sectors) % disk->heads);
}

bool DISK_ReadSectors(DISK* disk, uint32_t lba, uint8_t count, void __far* dataOut)
{
    uint16_t cylinder, sector, head;
    uint8_t retry;

    for (; count > 0; count--, lba++)
    {
        DISK_LbaToChs(disk, lba, &cylinder, &sector, &head);

        for (retry = 0; ; retry++)
        {
            if (x86_Disk_Read(disk->id, cylinder, sector, head, 1, dataOut))
            {
                break;
            }

            if (retry == DISK_READ_RETRIES - 1)
            {
                return false;
            }

            // floppy motors take a moment to spin up, reset & try again
            x86_Disk_Reset(disk->id);
        }

        // move forward 1 sector by bumping the segment, so the offset never wraps past 64KiB
        dataOut = MK_FP(FP_SEG(dataOut) + DISK_SECTOR_SIZE / 16, FP_OFF(dataOut));
    }

    return true;
}
