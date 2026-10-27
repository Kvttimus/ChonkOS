# ChonkOS

ChonkOS is a small but complete operating system for the original IBM PC, written from scratch in C and x86 assembly. It targets 8086/8088-compatible hardware, the CPU family of the 1981 IBM PC and PC/XT, and runs entirely in 16-bit real mode. There's no protected mode, no paging and no 32-bit instructions, only the 1 MB address space, 640 KB of conventional memory and segmented addressing those machines had. The bootloader, kernel, drivers, file system, shell and program loader are all original code. The only thing ChonkOS takes from the machine is the BIOS that every PC ships with.

It boots from a floppy disk, mounts FAT12 and FAT16 drives, gives you a DOS-style command line, and runs `.COM` and `.EXE` programs through a DOS-compatible system call interface.

## What's inside

**A custom two-stage bootloader.** Stage 1 is a 512-byte boot sector written in assembly. It understands just enough FAT12 to find `STAGE2.BIN` in the floppy's root directory, follow its cluster chain and load it. Stage 2 is written in C. It has a proper disk driver and a FAT12/FAT16 reader, loads `KERNEL.EXE`, applies the executable's segment relocations, zeroes its uninitialized data and jumps into it. Both stages read one sector at a time, so they work on old BIOSes that can't read across a track and never hit the floppy DMA controller's 64 KB boundary limit.

**A C kernel built for real-mode constraints.** The kernel is compiled with Open Watcom in the large memory model, so its code and data can each grow past 64 KB, and it's linked as a DOS MZ executable so it can be loaded at any address. Watcom has no runtime library here, so the 32-bit multiply and divide helpers the compiler needs on a 16-bit CPU are written by hand in assembly.

**DOS-style memory management.** Conventional memory is managed in 16-byte paragraphs as a chain of memory control blocks, in the same layout DOS uses. The allocator reclaims the memory the bootloader used, splits and merges blocks, resizes them in place, tracks which program owns each one, and checks the chain for corruption before handing out memory.

**Its own interrupt handlers and drivers.** ChonkOS installs its own handlers in the interrupt vector table:
- A timer that counts ticks and passes each one on to the BIOS, which still needs it to turn off the floppy motor.
- A keyboard driver that decodes scancodes into characters itself. It handles Shift, Ctrl, Alt, Caps Lock, Num Lock and the extra keys on 101-key keyboards. It also detects whether it's running on a PC/XT or an AT, because the two acknowledge keystrokes differently.
- A replacement for the BIOS keyboard service (`int 16h`) backed by the kernel's own key buffer.
- A divide-by-zero handler. A crash inside a program ends only that program.

**A read/write FAT12 and FAT16 file system.**
- **Drives:** the boot floppy is `A:`, and the first FAT partition on each hard disk (found by reading the partition table) is `C:` and `D:`.
- **Files and directories:** full subdirectory support, `.`/`..`, relative paths and a current directory per drive. Files can be created, read, written, appended, seeked, renamed and deleted, and directories made and removed. Subdirectories grow as they fill up.
- **Interoperability with other systems:** both copies of the FAT stay in sync, long-filename entries left by Linux or Windows are removed when ChonkOS deletes or renames a file, and new files get timestamps from the real-time clock.
- **Testing:** the driver has been stress-tested against Linux's `fsck.fat`, including filling a disk completely, and files copied through ChonkOS come back byte-identical.

**A DOS-like shell.** The command line has the commands you'd expect (`dir`, `cd`, `type`, `copy`, `del`, `ren`, `md`, `rd`, `echo` with `>`/`>>` redirection, `mem`, `time` and more). It also has a line editor with cursor keys, Home/End, Delete, command history, Esc to clear and Ctrl+C to cancel. Long listings pause with `-- More --`.

**Programs and a DOS-compatible system call API.**
- **Loading:** ChonkOS loads and runs both `.COM` programs and relocatable `.EXE` programs. Each gets a DOS-style Program Segment Prefix with its command-line arguments, an environment block and its own memory.
- **System calls:** programs talk to the kernel through `int 21h`, implementing around 40 of the most commonly used MS-DOS functions. These cover console input and output, files and directories, memory allocation, interrupt vectors, date and time, and exit codes. ChonkOS reports itself as DOS 3.30, so simple real DOS programs that stick to those functions can run unchanged. System calls switch to their own kernel stack, so a program with a tiny stack can't crash the kernel.
- **Cleanup when a program exits:** ChonkOS cleans up after it, however it exits. It restores every interrupt vector the program changed, closes its open files and frees every block of memory it owned.
- **Stopping and crashing:** Ctrl+Break stops a program stuck in a loop, and a divide by zero only kills the program that caused it.

## Why the 8086?

Targeting the original IBM PC means building everything under the same constraints its first operating systems had. The CPU only has 16-bit registers, so even 32-bit arithmetic needs helper routines. Memory is addressed through 64 KB segments, and there's no memory protection, so a program can overwrite anything, including the kernel. Disks are reached through the BIOS one cylinder/head/sector address at a time, and the floppy controller's DMA can't cross a 64 KB physical boundary. ChonkOS is written with all of that in mind:
- Every assembly file is assembled with NASM's `cpu 8086` setting, so an instruction a real 8088 can't run fails the build.
- The C code is compiled for the 8086 instruction set.
- Details that only matter on real hardware are handled too, such as the early-8088 interrupt bug when changing stacks and machines without a real-time clock.

## Requirements

Build on Linux or WSL with:

- `nasm`
- [Open Watcom v2](https://github.com/open-watcom/open-watcom-v2), installed at `/usr/bin/watcom` (the Makefiles use `/usr/bin/watcom/binl64/wcc` and `wlink`)
- `make` and `gcc`
- `mtools` (`mcopy`), `dosfstools` (`mkfs.fat`) and `fdisk` (`sfdisk`), to build the disk images
- `qemu-system-i386` to run it (optionally `bochs` for `debug.sh`)

## Building and running

From the project folder:

```sh
make run
```

This builds everything and boots ChonkOS in QEMU, from the floppy (`A:`) with the hard disk image attached as `C:`.

The same thing in separate steps:

```sh
make
./run.sh
```

or, starting QEMU yourself:

```sh
qemu-system-i386 -drive file=build/main_floppy.img,format=raw,if=floppy -drive file=build/main_hdd.img,format=raw,if=ide -boot a
```

The build produces:

- `build/main_floppy.img`: a 1.44 MB FAT12 boot floppy with the bootloader, kernel and example programs
- `build/main_hdd.img`: a 32 MB hard disk with one FAT16 partition

> **Note:** `build/main_hdd.img` is only created if it doesn't exist, so files you save on `C:` survive rebuilds. `make clean` deletes it, along with everything else in `build/`.

## Things to try

At the `A:\>` prompt:

| Command | What it does |
|---|---|
| `help` | List the built-in commands |
| `dir`, `cd`, `md`, `rd`, `type`, `copy`, `del`, `ren` | DOS-style file commands; paths can be relative or full (`C:\DOCS\A.TXT`) |
| `echo hi > note.txt` | Write a file (`>>` adds to it) |
| `C:` / `A:` | Switch drives |
| `mem` | Show memory blocks |
| `hello`, `args one two`, `input` | Example programs |
| `exetest` | Tests the `.EXE` loader |
| `files` | Tests the file syscalls (try it on `C:`) |
| `crash`, then `mem` | A program that crashes and leaks memory and a file; ChonkOS cleans up after it |
| `spin` | Loops forever; stop it with **Ctrl+Break** (Ctrl+Pause or Ctrl+Scroll Lock in QEMU) |

The shell supports arrow keys, Home/End, Delete, Up/Down for history, Esc to clear the line and Ctrl+C to cancel it.

## Project layout

| Path | Contents |
|---|---|
| `src/bootloader/stage1` | Boot sector: finds and loads `STAGE2.BIN` from the FAT12 floppy |
| `src/bootloader/stage2` | Loads `KERNEL.EXE`, applies its relocations and starts it |
| `src/kernel` | The kernel: memory allocator, interrupts, keyboard, disk, FAT12/16, file system, shell, program loader, `int 21h` system calls |
| `src/programs` | Example programs, built by `make` and copied to the floppy |
| `tools/fat` | A small FAT12 reader for disk images on the host |

## Notes

- When running `run.sh` & `debug.sh`, make sure to give them execute (+x) perms (`chmod +x run.sh debug.sh`).
- All code is limited to 8086 instructions (`cpu 8086` in the assembly, `-0` for Watcom), so it should run on an original IBM PC/XT. QEMU emulates a newer CPU; for a more faithful test, use an emulator like 86Box with an IBM XT configuration.
