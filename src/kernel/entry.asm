bits 16
cpu 8086

; The kernel is linked as a DOS MZ executable (see linker.lnk). Before jumping here, stage2:
;   - applies the relocations, fixing up every segment reference for where we actually got loaded
;   - zeroes BSS (the uninitialized globals that come after the image)
;   - sets ss:sp from the MZ header, to the stack segment below
;   - puts the boot drive in dl
; The linker sets ss to the stack segment on its own, but Watcom's C code expects ss = DGROUP,
; so we switch to the same stack addressed relative to DGROUP

section _ENTRY class=CODE

extern _kmain
global entry
global _kernel_start
global _kernel_end

; _ENTRY is the first segment of the image (see linker.lnk), so this is where the kernel starts
_kernel_start:
entry:
    ; C code expects ds = es = DGROUP (where globals & constants live)
    mov ax, DGROUP
    mov ds, ax
    mov es, ax

    ; interrupts off while changing ss:sp - some early 8088s don't block interrupts after "mov ss"
    cli
    mov ss, ax
    mov sp, stack_top                   ; offset relative to DGROUP, since _STACK is in the group
    sti

    ; send boot drive as arg to kmain
    xor dh, dh
    push dx
    call (seg _kmain):_kmain            ; large memory model => far call

    cli
.halt:
    hlt                                 ; an NMI can wake us up from hlt, so loop back
    jmp .halt


section _DATA class=DATA

section _STACK stack class=STACK align=16
    resb 4096
stack_top:
_kernel_end:                            ; the stack is the last thing in the image (see linker.lnk)

; stack is in DGROUP so ss = ds, which Watcom's C code expects
group DGROUP _DATA _STACK
