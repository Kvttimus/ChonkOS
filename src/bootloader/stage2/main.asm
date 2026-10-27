bits 16
cpu 8086

section _ENTRY class=CODE

extern _cstart_
global entry

entry:
    cli
    mov ax, ds
    mov ss, ax
    mov sp, 0
    mov bp, sp
    sti 

    ; expect boot drive in dl, & send as arg to cstart function
    xor dh, dh
    push dx
    call _cstart_

    cli
.halt:
    hlt                         ; an NMI can wake us up from hlt, so loop back
    jmp .halt

; a raw binary normally stops before the BSS segment (uninitialized globals), leaving that memory
; full of garbage. putting this segment after it (see linker.lnk) forces the linker to write BSS
; out as zeros, so stage1 loads it already zeroed like C expects
section _IMAGEEND class=IMAGEEND
    db 0
