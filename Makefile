ASM=nasm
CC=gcc
CC16=/usr/bin/watcom/binl64/wcc  		# Integrate Watcom compiler into build
LD16=/usr/bin/watcom/binl64/wlink

SRC_DIR=src
TOOLS_DIR=tools
BUILD_DIR=build

QEMU=qemu-system-i386

.PHONY: all run floppy_img hdd_img kernel bootloader programs clean always tools_fat

all: floppy_img hdd_img tools_fat

# Build everything & boot it in QEMU from the floppy, w/ the hard disk as C:
run: all
	$(QEMU) -drive file=$(BUILD_DIR)/main_floppy.img,format=raw,if=floppy \
	        -drive file=$(BUILD_DIR)/main_hdd.img,format=raw,if=ide -boot a

# Floppy Image
floppy_img: $(BUILD_DIR)/main_floppy.img

$(BUILD_DIR)/main_floppy.img: bootloader kernel programs
	dd if=/dev/zero of=$(BUILD_DIR)/main_floppy.img bs=512 count=2880
	mkfs.fat -F 12 -n "CHONKY OS" $(BUILD_DIR)/main_floppy.img
	dd if=$(BUILD_DIR)/stage1.bin of=$(BUILD_DIR)/main_floppy.img conv=notrunc
	mcopy -i $(BUILD_DIR)/main_floppy.img $(BUILD_DIR)/stage2.bin "::stage2.bin"
	mcopy -i $(BUILD_DIR)/main_floppy.img $(BUILD_DIR)/kernel.exe "::kernel.exe"
	mcopy -i $(BUILD_DIR)/main_floppy.img $(BUILD_DIR)/programs/*.com $(BUILD_DIR)/programs/*.exe "::"

#	mcopy -i $(BUILD_DIR)/main_floppy.img test.txt "::test.txt"

# Hard disk image: 32MiB w/ 1 FAT16 partition starting at sector 2048 (1MiB).
# Only created if it doesn't exist yet, so files written to it from ChonkOS survive rebuilds
hdd_img: $(BUILD_DIR)/main_hdd.img

$(BUILD_DIR)/main_hdd.img: | always
	dd if=/dev/zero of=$@ bs=1M count=32
	echo 'start=2048, type=06' | sfdisk -q $@
	mkfs.fat -F 16 --offset 2048 -n "CHONKY HDD" $@ 31744
	mcopy -i $@@@1M test.txt "::test.txt"

# Bootloader
bootloader: stage1 stage2

stage1: $(BUILD_DIR)/stage1.bin

$(BUILD_DIR)/stage1.bin: always
	$(MAKE) -C $(SRC_DIR)/bootloader/stage1 BUILD_DIR=$(abspath $(BUILD_DIR))

stage2: $(BUILD_DIR)/stage2.bin

$(BUILD_DIR)/stage2.bin: always
	$(MAKE) -C $(SRC_DIR)/bootloader/stage2 BUILD_DIR=$(abspath $(BUILD_DIR))

# Kernel
kernel: $(BUILD_DIR)/kernel.exe

$(BUILD_DIR)/kernel.exe: always
	$(MAKE) -C $(SRC_DIR)/kernel BUILD_DIR=$(abspath $(BUILD_DIR))

# Programs (.COM & .EXE files that go on the floppy)
programs: always
	$(MAKE) -C $(SRC_DIR)/programs BUILD_DIR=$(abspath $(BUILD_DIR))

# Tools
tools_fat: $(BUILD_DIR)/tools/fat
$(BUILD_DIR)/tools/fat: always $(TOOLS_DIR)/fat/fat.c
	mkdir -p $(BUILD_DIR)/tools
	$(CC) -g -o $(BUILD_DIR)/tools/fat $(TOOLS_DIR)/fat/fat.c

# Always
always:
	mkdir -p $(BUILD_DIR)

# Clean
clean:
	$(MAKE) -C $(SRC_DIR)/bootloader/stage1 BUILD_DIR=$(abspath $(BUILD_DIR)) clean
	$(MAKE) -C $(SRC_DIR)/bootloader/stage2 BUILD_DIR=$(abspath $(BUILD_DIR)) clean
	$(MAKE) -C $(SRC_DIR)/programs BUILD_DIR=$(abspath $(BUILD_DIR)) clean
	$(MAKE) -C $(SRC_DIR)/kernel BUILD_DIR=$(abspath $(BUILD_DIR)) clean
	rm -rf $(BUILD_DIR)/*
