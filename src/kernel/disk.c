#include "disk.h"
#include "memory.h"
#include "x86.h"

#define DISK_RETRIES                3
#define BDA_HARD_DISK_COUNT         MK_FP(0x0040, 0x0075)

// 2 sectors, so there's always a whole sector inside it that doesn't cross a 64KiB boundary
static uint8_t g_BounceArea[DISK_SECTOR_SIZE * 2];
static uint8_t __far* g_Bounce;

static void DISK_SetupBounceBuffer()
{
    uint32_t linear = (uint32_t)FP_SEG(g_BounceArea) * 16 + FP_OFF(g_BounceArea);
    uint16_t skip = 0;

    // if a sector starting here would cross into the next 64KiB, start at that boundary instead
    if ((linear & 0xFFFF) + DISK_SECTOR_SIZE > 0x10000)
    {
        skip = (uint16_t)(0x10000 - (linear & 0xFFFF));
    }
    g_Bounce = (uint8_t __far*)g_BounceArea + skip;
}

bool DISK_Initialize(DISK* disk, uint8_t driveNumber)
{
    uint8_t driveType;
    uint16_t cylinders, sectors, heads;

    if (g_Bounce == 0)
    {
        DISK_SetupBounceBuffer();
    }

    disk->id = driveNumber;

    if (!x86_Disk_GetDriveParams(driveNumber, &driveType, &cylinders, &sectors, &heads))
    {
        // the original IBM PC BIOS doesn't support this for floppies
        if (driveNumber >= 0x80)
        {
            return false;
        }

        // assume 1.44MB geometry, good enough to read the boot sector (LBA 0 is CHS 0/0/1 on
        // every floppy), the FAT driver then replaces it w/ the geometry stored in the BPB
        cylinders = 80;
        sectors = 18;
        heads = 2;
    }

    disk->cylinders = cylinders;
    disk->sectors = sectors;
    disk->heads = heads;
    return true;
}

uint8_t DISK_GetHardDiskCount()
{
    return *(uint8_t __far*)BDA_HARD_DISK_COUNT;
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

static bool DISK_Transfer(DISK* disk, uint32_t lba, bool write)
{
    uint16_t cylinder, sector, head;
    uint8_t retry;

    DISK_LbaToChs(disk, lba, &cylinder, &sector, &head);

    // BIOS CHS calls can only reach the first 1024 cylinders (~504MB)
    if (cylinder >= 1024 || cylinder >= disk->cylinders)
    {
        return false;
    }

    for (retry = 0; retry < DISK_RETRIES; retry++)
    {
        bool ok = write ? x86_Disk_Write(disk->id, cylinder, sector, head, 1, g_Bounce)
                        : x86_Disk_Read(disk->id, cylinder, sector, head, 1, g_Bounce);
        if (ok)
        {
            return true;
        }

        // floppy motors take a moment to spin up, reset & try again
        x86_Disk_Reset(disk->id);
    }

    return false;
}

bool DISK_ReadSectors(DISK* disk, uint32_t lba, uint16_t count, void __far* data)
{
    for (; count > 0; count--, lba++)
    {
        if (!DISK_Transfer(disk, lba, false))
        {
            return false;
        }
        memcpy(data, g_Bounce, DISK_SECTOR_SIZE);

        // bump the segment, so the offset never wraps past 64KiB
        data = MK_FP(FP_SEG(data) + DISK_SECTOR_SIZE / 16, FP_OFF(data));
    }
    return true;
}

bool DISK_WriteSectors(DISK* disk, uint32_t lba, uint16_t count, const void __far* data)
{
    for (; count > 0; count--, lba++)
    {
        memcpy(g_Bounce, data, DISK_SECTOR_SIZE);
        if (!DISK_Transfer(disk, lba, true))
        {
            return false;
        }

        data = MK_FP(FP_SEG(data) + DISK_SECTOR_SIZE / 16, FP_OFF(data));
    }
    return true;
}
