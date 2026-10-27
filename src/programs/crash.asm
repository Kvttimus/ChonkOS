; CRASH.COM - misbehaves on purpose: hooks the timer interrupt & never unhooks it, allocates
; memory & never frees it, opens a file & never closes it, then divides by zero.
; ChonkOS should clean all of that up & keep running.
org 100h
bits 16
cpu 8086

    mov dx, msg_start
    mov ah, 09h
    int 21h

    ; hook int 08h (timer) w/ a handler in our own memory, which is freed when we exit
    mov ax, 3508h                       ; get the old vector -> es:bx
    int 21h
    mov [old_timer], bx
    mov [old_timer + 2], es
    mov dx, timer_handler
    mov ax, 2508h                       ; set vector to ds:dx
    int 21h

    ; shrink our block so there's room, then allocate 4KiB & don't free it
    mov bx, 1000h                       ; keep 64KiB
    mov ah, 4Ah                         ; es = our PSP
    push cs
    pop es
    int 21h
    mov bx, 100h
    mov ah, 48h
    int 21h

    ; open a file & leave it open
    mov dx, file_name
    mov ah, 3Ch
    xor cx, cx
    int 21h

    mov dx, msg_crash
    mov ah, 09h
    int 21h

    xor ax, ax
    xor dx, dx
    xor cx, cx
    div cx                              ; divide by zero

    ; never gets here
    ret

timer_handler:
    inc word [cs:ticks]
    jmp far [cs:old_timer]

msg_start:  db 'Hooking the timer, leaking memory & a file...', 13, 10, '$'
msg_crash:  db 'Now dividing by zero!', 13, 10, '$'
file_name:  db 'LEAK.TXT', 0
old_timer:  dd 0
ticks:      dw 0
