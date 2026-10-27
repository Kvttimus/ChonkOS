make
qemu-system-i386 -drive file=build/main_floppy.img,format=raw,if=floppy -drive file=build/main_hdd.img,format=raw,if=ide -boot a
