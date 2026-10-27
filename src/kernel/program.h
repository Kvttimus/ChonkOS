#pragma once
#include "stdint.h"
#include "stdbool.h"
#include "fs.h"

// exit codes are DOS style: high byte = how the program ended, low byte = its return code
#define PROGRAM_EXIT_NORMAL         0x00
#define PROGRAM_EXIT_CTRL_C         0x01    // Ctrl+C or Ctrl+Break
#define PROGRAM_EXIT_CRASH          0x02    // divide error (DOS uses 2 for critical errors)
#define PROGRAM_EXIT_TYPE(code)     ((uint8_t)((code) >> 8))
#define PROGRAM_EXIT_CODE(code)     ((uint8_t)(code))

// DOS programs can have up to 20 open files (handles 0-4 are stdin, stdout, stderr, aux, printer)
#define PROGRAM_MAX_FILES           20

void PROGRAM_Initialize();

// Loads a .COM or .EXE & runs it, returns once it exits. path must be a full path.
// Prints its own error messages, returns false if the program couldn't be started
bool PROGRAM_Run(const char* path, const char* arguments, uint16_t* exitCodeOut);

bool PROGRAM_IsRunning();
uint16_t PROGRAM_GetPsp();
uint16_t PROGRAM_GetLastExitCode();

// ends the running program right away (from a syscall or an interrupt handler), never returns
void PROGRAM_Terminate(uint16_t exitCode);

// true while the kernel is handling a syscall for the program
bool PROGRAM_InSyscall();

// ends the program if Ctrl+Break was pressed during a syscall, syscalls call this when it's safe
void PROGRAM_CheckBreak();

// file handle table of the running program: DOS handle -> file system handle (-1 = closed)
FS_Handle* PROGRAM_GetFiles();
