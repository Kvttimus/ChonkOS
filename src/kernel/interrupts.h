#pragma once
#include "stdint.h"

// Interrupt handlers are written in C w/ Watcom's __interrupt keyword: the compiler saves every
// register, loads ds = DGROUP, and returns w/ iret
typedef void (__interrupt __far* ISR)();

// Registers as saved by an __interrupt function, when it takes this as its parameter.
// Order matches what Watcom pushes (checked against its generated code)
typedef struct
{
    uint16_t gs, fs;                    // placeholders, 8086 doesn't have these registers
    uint16_t es, ds, di, si, bp, sp, bx, dx, cx, ax;
    uint16_t ip, cs, flags;             // pushed by the CPU when the interrupt happened
} ISR_Registers;

// The 8259 PIC: hardware IRQ 0-7 arrive as interrupt vectors 08h-0Fh (set up by the BIOS)
#define IRQ_TIMER           0
#define IRQ_KEYBOARD        1
#define IRQ_TO_VECTOR(irq)  (0x08 + (irq))

// interrupt vectors the CPU uses for its own errors
#define VECTOR_DIVIDE_ERROR 0x00

// software interrupts
#define VECTOR_BIOS_KEYBOARD    0x16
#define VECTOR_DOS_TERMINATE    0x20
#define VECTOR_DOS_SYSCALL      0x21

// bits in the flags register
#define FLAG_CARRY          0x0001
#define FLAG_ZERO           0x0040

ISR IVT_GetVector(uint8_t vector);
void IVT_SetVector(uint8_t vector, ISR handler);

// Tells the PIC we're done handling an IRQ, so it can send us the next one
void PIC_SendEndOfInterrupt();
