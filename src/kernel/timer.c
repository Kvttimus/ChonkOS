#include "timer.h"
#include "interrupts.h"
#include "x86.h"

static volatile uint32_t g_Ticks;
static ISR g_BiosTimerHandler;

static void __interrupt __far TIMER_Handler()
{
    g_Ticks++;

    // The BIOS handler also turns the floppy motor off after a disk read, keeps the time of
    // day & sends the PIC end of interrupt, so we always pass the tick on to it
    g_BiosTimerHandler();
}

void TIMER_Initialize()
{
    g_Ticks = 0;
    g_BiosTimerHandler = IVT_GetVector(IRQ_TO_VECTOR(IRQ_TIMER));
    IVT_SetVector(IRQ_TO_VECTOR(IRQ_TIMER), TIMER_Handler);
}

uint32_t TIMER_GetTicks()
{
    uint32_t a, b;

    // a 32-bit read is 2 instructions on the 8086, the timer could fire in between. reading
    // until 2 reads match means we didn't catch it half updated
    do
    {
        a = g_Ticks;
        b = g_Ticks;
    } while (a != b);

    return a;
}

void TIMER_Sleep(uint16_t ms)
{
    // ticks = ms * 18.2 / 1000, rounded up so we never sleep too little
    uint32_t ticks = ((uint32_t)ms * TIMER_TICKS_PER_10_SECONDS + 9999) / 10000;
    uint32_t start = TIMER_GetTicks();

    while (TIMER_GetTicks() - start < ticks)
    {
        x86_Halt();
    }
}
