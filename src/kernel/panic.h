#pragma once

// installs handlers for the CPU's own errors (divide by zero)
void PANIC_Initialize();

// prints the message & stops the machine, never returns
void panic(const char* message);
