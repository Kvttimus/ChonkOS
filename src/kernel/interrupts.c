#include "interrupts.h"
#include "memory.h"
#include "x86.h"

#define PIC_COMMAND_PORT    0x20
#define PIC_EOI             0x20

// The interrupt vector table (IVT) sits at 0000:0000, 256 far pointers (segment:offset) one
// per interrupt number. The CPU looks up the handler there whenever an interrupt happens
static ISR __far* const g_IVT = (ISR __far*)MK_FP(0, 0);

ISR IVT_GetVector(uint8_t vector)
{
    return g_IVT[vector];
}

void IVT_SetVector(uint8_t vector, ISR handler)
{
    // a vector is 2 words, an interrupt in between writing them would jump to a half updated
    // address, so keep interrupts off while it changes
    x86_DisableInterrupts();
    g_IVT[vector] = handler;
    x86_EnableInterrupts();
}

void PIC_SendEndOfInterrupt()
{
    x86_OutByte(PIC_COMMAND_PORT, PIC_EOI);
}
