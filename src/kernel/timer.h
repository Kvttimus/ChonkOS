#pragma once
#include "stdint.h"

// The BIOS programs the PIT to fire IRQ0 about 18.2 times per second (every ~55ms)
#define TIMER_TICKS_PER_10_SECONDS  182

void TIMER_Initialize();

// ticks since the kernel started
uint32_t TIMER_GetTicks();

// waits at least ms milliseconds (in steps of ~55ms)
void TIMER_Sleep(uint16_t ms);
