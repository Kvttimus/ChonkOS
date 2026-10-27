#pragma once
#include "stdint.h"
#include "stdbool.h"

void _cdecl x86_Video_WriteCharTeletype(char c, uint8_t page);

uint16_t _cdecl x86_GetMemorySize();

bool _cdecl x86_Disk_Reset(uint8_t drive);
bool _cdecl x86_Disk_Read(uint8_t drive, uint16_t cylinder, uint16_t sector, uint16_t head,
                          uint8_t count, void __far* dataOut);
bool _cdecl x86_Disk_GetDriveParams(uint8_t drive, uint8_t* driveTypeOut, uint16_t* cylindersOut,
                                    uint16_t* sectorsOut, uint16_t* headsOut);

void _cdecl x86_StartKernel(uint16_t cs, uint16_t ip, uint16_t ss, uint16_t sp, uint8_t bootDrive);
