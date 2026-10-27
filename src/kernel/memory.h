#pragma once
#include "stdint.h"

// far pointers are segment:offset, stored as (segment << 16) | offset
#define MK_FP(segment, offset)  ((void __far*)(((uint32_t)(segment) << 16) | (uint16_t)(offset)))
#define FP_SEG(farPointer)      ((uint16_t)((uint32_t)(void __far*)(farPointer) >> 16))
#define FP_OFF(farPointer)      ((uint16_t)(uint32_t)(void __far*)(farPointer))

// large memory model => all pointers here are far
void* memcpy(void* dst, const void* src, uint16_t count);
void* memset(void* dst, int value, uint16_t count);
int memcmp(const void* a, const void* b, uint16_t count);
