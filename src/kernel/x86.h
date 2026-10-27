#pragma once
#include "stdint.h"
#include "stdbool.h"

void _cdecl x86_Video_WriteCharTeletype(char c, uint8_t page);
uint8_t _cdecl x86_Video_GetMode();
void _cdecl x86_Video_SetMode(uint8_t mode);
void _cdecl x86_Video_GetCursor(uint8_t* rowOut, uint8_t* columnOut);
void _cdecl x86_Video_SetCursor(uint8_t row, uint8_t column);

uint16_t _cdecl x86_GetMemorySize();

bool _cdecl x86_Disk_Reset(uint8_t drive);
bool _cdecl x86_Disk_Read(uint8_t drive, uint16_t cylinder, uint16_t sector, uint16_t head,
                          uint8_t count, void __far* data);
bool _cdecl x86_Disk_Write(uint8_t drive, uint16_t cylinder, uint16_t sector, uint16_t head,
                           uint8_t count, const void __far* data);
bool _cdecl x86_Disk_GetDriveParams(uint8_t drive, uint8_t* driveTypeOut, uint16_t* cylindersOut,
                                    uint16_t* sectorsOut, uint16_t* headsOut);

// BCD encoded, 0xFFFFFFFF if the machine has no clock chip (PC/XT)
uint32_t _cdecl x86_RTC_GetTime();          // 0x00HHMMSS
uint32_t _cdecl x86_RTC_GetDate();          // 0xCCYYMMDD

void _cdecl x86_Halt();
void _cdecl x86_HaltForever();
void _cdecl x86_DisableInterrupts();
void _cdecl x86_EnableInterrupts();

uint8_t _cdecl x86_InByte(uint16_t port);
void _cdecl x86_OutByte(uint16_t port, uint8_t value);

void _cdecl x86_Reboot();
