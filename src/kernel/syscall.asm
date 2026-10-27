bits 16
cpu 8086

; ------------------------------------------------------------------------------------------------
; System calls (int 21h) & running programs
;
; A program calls int 21h w/ the function # in ah. The entry point saves every register on the
; program's stack in the same layout Watcom's __interrupt functions use (ISR_Registers), switches
; to a kernel stack (the program's could be tiny), and calls SYSCALL_Handle w/ a far pointer to
; the saved registers. The C code reads arguments from them & writes results into them, which
; get popped back into the real registers on the way out.
; ------------------------------------------------------------------------------------------------

section syscall_TEXT class=CODE

extern _SYSCALL_Handle

global _syscall_int21
global _syscall_int20
global _x86_RunProgram
global _x86_ExitProgram
global _syscall_depth

; int 20h - old style "terminate program", same as int 21h ah=4Ch al=0
_syscall_int20:
    mov ax, 4C00h
    ; fall through into int 21h

_syscall_int21:
    ; interrupts are off, the CPU pushed flags, cs, ip
    push ax
    push cx
    push dx
    push bx
    push sp                             ; placeholder, matches ISR_Registers
    push bp
    push si
    push di
    push ds
    push es
    push ax                             ; fs placeholder (8086 doesn't have fs/gs)
    push ax                             ; gs placeholder

    mov dx, ss                          ; dx:bx = the saved registers
    mov bx, sp

    mov ax, DGROUP
    mov ds, ax
    mov es, ax

    ; switch to the kernel's syscall stack, unless we're already on it (a syscall made while
    ; handling a syscall, e.g. by a program's own interrupt handler)
    cmp word [_syscall_depth], 0
    jne .on_kernel_stack
    mov ss, ax
    mov sp, syscall_stack_top
.on_kernel_stack:
    inc word [_syscall_depth]

    push dx                             ; remember the caller's stack
    push bx

    sti                                 ; syscalls can wait for the keyboard, they need interrupts
    cld

    push dx                             ; SYSCALL_Handle(ISR_Registers far* registers)
    push bx
    call far _SYSCALL_Handle
    add sp, 4

    cli
    dec word [_syscall_depth]

    pop bx                              ; back to the caller's stack
    pop dx
    mov ss, dx
    mov sp, bx

    pop ax                              ; gs, fs placeholders
    pop ax
    pop es
    pop ds
    pop di
    pop si
    pop bp
    pop bx                              ; sp placeholder
    pop bx
    pop dx
    pop cx
    pop ax
    iret


; Starts a program & returns when it exits (when x86_ExitProgram is called)
; void x86_RunProgram(uint16_t cs, uint16_t ip, uint16_t ss, uint16_t sp, uint16_t psp);
_x86_RunProgram:
    push bp
    mov bp, sp
    push si
    push di
    push ds
    push es
    push bx

    ; x86_ExitProgram comes back to this exact spot
    mov [kernel_ss], ss
    mov [kernel_sp], sp

    ; [bp + 6] - cs
    ; [bp + 8] - ip
    ; [bp + 10] - ss
    ; [bp + 12] - sp
    ; [bp + 14] - psp
    mov ax, [bp + 6]
    mov bx, [bp + 8]
    mov cx, [bp + 10]
    mov dx, [bp + 12]
    mov si, [bp + 14]

    ; interrupts off while changing ss:sp - some early 8088s don't block interrupts after "mov ss"
    cli
    mov ss, cx
    mov sp, dx
    sti

    push ax                             ; far return address = cs:ip
    push bx

    ; like DOS: ds = es = the program's PSP, other registers 0
    mov ds, si
    mov es, si
    xor ax, ax
    xor bx, bx
    xor cx, cx
    xor dx, dx
    xor si, si
    xor di, di
    xor bp, bp
    retf


; Abandons the running program & returns from x86_RunProgram, can be called from anywhere
; (a syscall, an interrupt handler), never returns
; void x86_ExitProgram();
_x86_ExitProgram:
    cli
    mov ax, DGROUP
    mov ds, ax
    mov word [_syscall_depth], 0
    mov ss, [kernel_ss]
    mov sp, [kernel_sp]
    sti

    pop bx
    pop es
    pop ds
    pop di
    pop si
    pop bp
    retf


section _DATA class=DATA public

_syscall_depth:     dw 0                ; > 0 while handling a syscall
kernel_ss:          dw 0
kernel_sp:          dw 0

section _BSS class=BSS public

syscall_stack:      resb 4096
syscall_stack_top:

group DGROUP _DATA _BSS
