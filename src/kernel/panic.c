#include "panic.h"
#include "interrupts.h"
#include "program.h"
#include "stdio.h"
#include "x86.h"

void panic(const char* message)
{
    printf("\n*** KERNEL PANIC: %s ***\nSystem halted.", message);
    x86_HaltForever();
}

static void __interrupt __far PANIC_DivideErrorHandler(ISR_Registers registers)
{
    // on an 8086 cs:ip points right after the div instruction, on a 286+ at the div itself
    printf("\nDivide error at %04x:%04x (ax=%04x dx=%04x)", registers.cs, registers.ip, registers.ax, registers.dx);

    // a program's own bug only ends the program, the kernel is fine
    if (PROGRAM_IsRunning() && !PROGRAM_InSyscall())
    {
        printf(" - program stopped\n");
        x86_EnableInterrupts();
        PROGRAM_Terminate(PROGRAM_EXIT_CRASH << 8);
    }
    panic("divide by zero");
}

void PANIC_Initialize()
{
    IVT_SetVector(VECTOR_DIVIDE_ERROR, (ISR)PANIC_DivideErrorHandler);
}
