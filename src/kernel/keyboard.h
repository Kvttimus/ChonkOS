#pragma once
#include "stdint.h"
#include "stdbool.h"

// Keys are reported like the BIOS does: low byte = ASCII character (0 for keys that don't have
// one, like arrows & F keys), high byte = scancode of the key
#define KEY_ASCII(key)      ((char)((key) & 0xFF))
#define KEY_SCANCODE(key)   ((uint8_t)((key) >> 8))

// scancodes (set 1, which every PC keyboard sends to the CPU) of keys w/o an ASCII character
#define KEY_F1              0x3B
#define KEY_F10             0x44
#define KEY_HOME            0x47
#define KEY_UP              0x48
#define KEY_PAGE_UP         0x49
#define KEY_LEFT            0x4B
#define KEY_RIGHT           0x4D
#define KEY_END             0x4F
#define KEY_DOWN            0x50
#define KEY_PAGE_DOWN       0x51
#define KEY_INSERT          0x52
#define KEY_DELETE          0x53

// called from the keyboard interrupt when Ctrl+Break is pressed, it doesn't have to return
typedef void (*KBD_BreakHandler)();

// also takes over the BIOS keyboard service (int 16h) for programs that use it
void KBD_Initialize();
void KBD_SetBreakHandler(KBD_BreakHandler handler);

bool KBD_HasKey();

// waits for a key press
uint16_t KBD_GetKey();

// next key w/o removing it, 0 if there isn't one
uint16_t KBD_PeekKey();

// BIOS format: bit 1 shift, 2 ctrl, 3 alt, 5 num lock, 6 caps lock
uint8_t KBD_GetShiftFlags();
