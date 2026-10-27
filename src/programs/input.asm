; INPUT.COM - reads a line w/ int 21h ah=0Ah, then a key w/ the BIOS (int 16h)
org 100h
bits 16
cpu 8086

    mov dx, msg_name
    mov ah, 09h
    int 21h

    ; buffered input: [0] = max chars, [1] = chars read, then the text ending in CR
    mov dx, line
    mov ah, 0Ah
    int 21h

    mov dx, msg_hi
    mov ah, 09h
    int 21h

    mov bx, 1                           ; print the name to stdout
    mov dx, line + 2
    mov cl, [line + 1]
    xor ch, ch
    mov ah, 40h
    int 21h

    mov dx, msg_key
    mov ah, 09h
    int 21h

    ; wait for a key straight from the BIOS keyboard service
    xor ah, ah
    int 16h

    mov dl, al                          ; echo it w/ int 21h ah=02h
    mov ah, 02h
    int 21h

    mov dx, msg_bye
    mov ah, 09h
    int 21h
    ret

msg_name:   db 'What', 39, 's your name? $'
msg_hi:     db 'Hi, $'
msg_key:    db '! Press any key: $'
msg_bye:    db 13, 10, 'Bye!', 13, 10, '$'

line:       db 30, 0
            times 31 db 0
