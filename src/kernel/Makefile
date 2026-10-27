BUILD_DIR?=build/
ASM?=nasm
ASMFLAGS?=-f obj
CC16?=/usr/bin/watcom/binl64/wcc  		# Integrate Watcom compiler into build
# -0 = 8086 instructions only, -ml = large memory model
# -zu = don't assume ss = DGROUP: interrupt handlers run on whatever stack was active (a program's),
#       w/o it Watcom reaches globals through ss & writes them into the program's memory
CFLAGS16?=-0 -d3 -s -wx -ml -zu -zl -zq
LD16?=/usr/bin/watcom/binl64/wlink

SOURCES_C=$(wildcard *.c)
SOURCES_ASM=$(wildcard *.asm)
HEADERS=$(wildcard *.h)
OBJECTS_C=$(patsubst %.c, $(BUILD_DIR)/kernel/c/%.obj, $(SOURCES_C))
OBJECTS_ASM=$(patsubst %.asm, $(BUILD_DIR)/kernel/asm/%.obj, $(SOURCES_ASM))

.PHONY: all kernel clean always

all: kernel

kernel: $(BUILD_DIR)/kernel.exe

# linked as a DOS MZ executable, so stage2 can relocate it (see entry.asm)
$(BUILD_DIR)/kernel.exe: $(OBJECTS_ASM) $(OBJECTS_C) linker.lnk
	$(LD16) NAME $(BUILD_DIR)/kernel.exe FILE \{ $(OBJECTS_ASM) $(OBJECTS_C) \} OPTION MAP=$(BUILD_DIR)/kernel.map @linker.lnk

$(BUILD_DIR)/kernel/c/%.obj: %.c $(HEADERS) | always
	$(CC16) $(CFLAGS16) -fo=$@ $<

$(BUILD_DIR)/kernel/asm/%.obj: %.asm | always
	$(ASM) $(ASMFLAGS) -o $@ $<

always:
	mkdir -p $(BUILD_DIR)/kernel/c
	mkdir -p $(BUILD_DIR)/kernel/asm

clean:
	rm -f $(BUILD_DIR)/kernel.exe $(BUILD_DIR)/kernel.map
	rm -rf $(BUILD_DIR)/kernel
