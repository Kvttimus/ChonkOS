#include "stdint.h"
#include "stdio.h"
#include "disk.h"
#include "fat.h"
#include "memory.h"
#include "mz.h"
#include "x86.h"

#define KERNEL_FILE_NAME        "KERNEL.EXE"
#define KERNEL_LOAD_SEGMENT     0x3000      // physical 0x30000, right after stage2's 64KiB segment
                                            // 64KiB aligned so sector reads stay DMA safe

void _cdecl cstart_(uint16_t bootDrive)
{
    DISK disk;
    FAT_DirectoryEntry kernel;
    MZ_StartInfo start;
    uint16_t memoryKb = x86_GetMemorySize();
    uint32_t memoryTop = (uint32_t)memoryKb * 1024;
    uint32_t kernelStart = (uint32_t)KERNEL_LOAD_SEGMENT * 16;

    printf("ChonkOS stage2 - boot drive %02xh, %u KiB memory\n", bootDrive, memoryKb);

    if (memoryTop <= kernelStart)
    {
        printf("Not enough memory! Need more than %lu KiB\n", kernelStart / 1024);
        goto end;
    }

    if (!DISK_Initialize(&disk, (uint8_t)bootDrive))
    {
        puts("Disk init error!\n");
        goto end;
    }

    if (!FAT_Initialize(&disk, 0))
    {
        puts("FAT init error!\n");
        goto end;
    }
    printf("FAT%u volume, %u clusters\n", FAT_GetType(), FAT_GetClusterCount());

    if (!FAT_FindFile(KERNEL_FILE_NAME, &kernel))
    {
        printf("%s not found!\n", KERNEL_FILE_NAME);
        goto end;
    }

    if (!FAT_ReadFile(&kernel, MK_FP(KERNEL_LOAD_SEGMENT, 0), memoryTop - kernelStart))
    {
        printf("Could not load %s!\n", KERNEL_FILE_NAME);
        goto end;
    }

    if (!MZ_Prepare(KERNEL_LOAD_SEGMENT, kernel.Size, memoryTop - kernelStart, &start))
    {
        printf("Could not prepare %s!\n", KERNEL_FILE_NAME);
        goto end;
    }
    printf("Loaded %s (%lu bytes), starting kernel at %04x:%04x...\n", KERNEL_FILE_NAME, kernel.Size, start.cs, start.ip);

    x86_StartKernel(start.cs, start.ip, start.ss, start.sp, (uint8_t)bootDrive);

end:
    for (;;);
}
