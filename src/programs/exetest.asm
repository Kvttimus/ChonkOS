; EXETEST.EXE - tests the .EXE loader: several segments (so the loader has to relocate the
; segment values), a far call between code segments, >64KiB of data (so it's read in pieces)
; and uninitialized data (BSS) that has to be zeroed. Also tests the memory syscalls.
; Built as an object file & linked into a DOS MZ .EXE w/ wlink
;
; NOTE: the linker packs segments together, so a segment's data doesn't always start at offset 0
; of its segment value. Always use "seg label" & the label's own offset to reach data
bits 16
cpu 8086

segment code class=CODE

..start:
    ; ds = es = PSP at the start, point ds at our data
    mov ax, seg msg_hello
    mov ds, ax

    mov dx, msg_hello
    call print

    ; far call into the other code segment
    call far other_function

    ; the last byte of the 2nd big data segment must have loaded right
    mov ax, seg big_last
    mov es, ax
    cmp byte [es:big_last], 5Ah
    jne fail
    mov dx, msg_big
    call print

    ; BSS must be all zeros
    mov ax, seg bss_start
    mov es, ax
    mov di, bss_start
    mov cx, BSS_SIZE
    xor al, al
    repe scasb
    jne fail
    mov dx, msg_bss
    call print

    ; memory: shrink our block to what we use, allocate 32KiB, fill it, free it
    mov ah, 62h                         ; get PSP -> bx
    int 21h
    mov es, bx                          ; es = our block (it starts w/ the PSP)
    mov ax, seg program_end_marker      ; first paragraph after the program =
    mov dx, program_end_marker          ;   segment + (offset + 15) / 16
    add dx, 15
    mov cl, 4
    shr dx, cl
    add ax, dx
    sub ax, bx                          ; size in paragraphs = that - PSP segment
    mov bx, ax
    mov ah, 4Ah                         ; resize block es to bx paragraphs
    int 21h
    jc fail

    mov bx, 800h                        ; 32KiB
    mov ah, 48h
    int 21h
    jc fail
    mov es, ax
    xor di, di
    mov cx, 8000h
    mov al, 0AAh
    rep stosb
    mov ah, 49h                         ; free es
    int 21h
    jc fail
    mov dx, msg_memory
    call print

    mov ax, 4C2Ah                       ; exit w/ return code 42
    int 21h

fail:
    mov dx, msg_fail
    call print
    mov ax, 4C01h
    int 21h

print:
    mov ah, 09h
    int 21h
    ret

segment code2 class=CODE

other_function:
    push ds
    mov ax, seg msg_far
    mov ds, ax
    mov dx, msg_far
    mov ah, 09h
    int 21h
    pop ds
    retf

segment data class=DATA

msg_hello:  db 'Hello from EXETEST.EXE!', 13, 10, '$'
msg_far:    db '  far call into another code segment: ok', 13, 10, '$'
msg_big:    db '  >64KiB of data loaded: ok', 13, 10, '$'
msg_bss:    db '  BSS zeroed: ok', 13, 10, '$'
msg_memory: db '  resize, allocate & free memory: ok', 13, 10, '$'
msg_fail:   db '  FAILED', 13, 10, '$'

BIG_SIZE    equ 40000

segment bigdata1 class=FAR_DATA
            times BIG_SIZE db 11h

segment bigdata2 class=FAR_DATA
            times BIG_SIZE - 1 db 22h
big_last:   db 5Ah

BSS_SIZE    equ 1000

segment bss class=BSS
bss_start:  resb BSS_SIZE

segment stack stack class=STACK
            resb 1024

; marks the end of the program in memory (the linker puts it last)
segment program_end class=ZZEND
program_end_marker:
