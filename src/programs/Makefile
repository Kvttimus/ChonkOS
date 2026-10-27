BUILD_DIR?=build/
ASM?=nasm
LD16?=/usr/bin/watcom/binl64/wlink

# .COM programs are flat binaries, .EXE programs are object files linked into a DOS MZ executable
COM_PROGRAMS=hello args files input spin crash
EXE_PROGRAMS=exetest

OUT=$(BUILD_DIR)/programs
TARGETS=$(patsubst %, $(OUT)/%.com, $(COM_PROGRAMS)) $(patsubst %, $(OUT)/%.exe, $(EXE_PROGRAMS))

.PHONY: all programs clean always

all: programs

programs: $(TARGETS)

$(OUT)/%.com: %.asm | always
	$(ASM) -f bin -o $@ $<

$(OUT)/%.exe: %.asm | always
	$(ASM) -f obj -o $(OUT)/$*.obj $<
	$(LD16) FORMAT DOS NAME $@ FILE $(OUT)/$*.obj OPTION QUIET

always:
	mkdir -p $(OUT)

clean:
	rm -rf $(OUT)
