; ARGS.COM - prints its command line arguments, exits w/ their length as the return code
org 100h
bits 16
cpu 8086

PSP_TAIL_LENGTH     equ 80h             ; the command line after the program name
PSP_TAIL            equ 81h

    mov dx, msg_args
    mov ah, 09h
    int 21h

    ; print the command tail w/ int 21h ah=40h (write to a handle), handle 1 = stdout
    mov cl, [PSP_TAIL_LENGTH]
    xor ch, ch
    mov dx, PSP_TAIL
    mov bx, 1
    mov ah, 40h
    int 21h

    mov dx, msg_end
    mov ah, 09h
    int 21h

    mov al, [PSP_TAIL_LENGTH]           ; return code = # of chars
    mov ah, 4Ch                         ; int 21h ah=4Ch: exit w/ return code al
    int 21h

msg_args:   db 'Arguments: [$'
msg_end:    db ']', 13, 10, '$'
