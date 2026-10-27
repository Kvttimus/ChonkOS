#pragma once

#define CHONKOS_VERSION "0.1"

// the command line, never returns
void SHELL_Run();

// lists the mounted drives (also used at boot)
void SHELL_PrintDrives();
