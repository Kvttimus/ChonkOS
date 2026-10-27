#include "stdint.h"
#include "stdio.h"
#include "alloc.h"
#include "fs.h"
#include "keyboard.h"
#include "panic.h"
#include "program.h"
#include "shell.h"
#include "timer.h"
#include "x86.h"

void _cdecl kmain(uint16_t bootDrive)
{
    uint32_t freeBytes, largestBytes;
    uint16_t blocks;

    clrscr();
    printf("ChonkOS %s\n\n", CHONKOS_VERSION);

    PANIC_Initialize();
    TIMER_Initialize();
    KBD_Initialize();

    MEM_Initialize();
    MEM_GetStats(&freeBytes, &largestBytes, &blocks);
    printf("Memory: %u KiB, %lu KiB free\n", x86_GetMemorySize(), freeBytes / 1024);

    FS_Initialize((uint8_t)bootDrive);
    SHELL_PrintDrives();

    PROGRAM_Initialize();

    printf("\nType \"help\" for a list of commands.\n\n");
    SHELL_Run();
}
