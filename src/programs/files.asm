; FILES.COM - exercises the file syscalls: create, write, close, open, read, seek, delete
org 100h
bits 16
cpu 8086

    ; create CHONK.TXT
    mov dx, file_name
    xor cx, cx                          ; attributes
    mov ah, 3Ch
    int 21h
    jc error
    mov [handle], ax

    ; write the text to it
    mov bx, ax
    mov dx, text
    mov cx, text_length
    mov ah, 40h
    int 21h
    jc error

    mov bx, [handle]
    mov ah, 3Eh                         ; close
    int 21h

    ; open it again for reading
    mov dx, file_name
    mov al, 0                           ; read only
    mov ah, 3Dh
    int 21h
    jc error
    mov [handle], ax

    ; skip the first 6 bytes ("Hello ") w/ a seek from the start
    mov bx, ax
    xor cx, cx
    mov dx, 6
    mov ax, 4200h
    int 21h
    jc error

    ; read the rest back
    mov bx, [handle]
    mov dx, buffer
    mov cx, 100
    mov ah, 3Fh
    int 21h
    jc error
    mov [bytes_read], ax

    mov bx, [handle]
    mov ah, 3Eh
    int 21h

    ; print what was read to stdout
    mov dx, msg_read
    mov ah, 09h
    int 21h
    mov bx, 1
    mov dx, buffer
    mov cx, [bytes_read]
    mov ah, 40h
    int 21h

    ; delete it, then make sure opening it fails
    mov dx, file_name
    mov ah, 41h
    int 21h
    jc error

    mov dx, file_name
    mov ax, 3D00h
    int 21h
    jnc error                           ; should be gone
    cmp ax, 2                           ; error 2 = file not found
    jne error

    mov dx, msg_ok
    mov ah, 09h
    int 21h
    mov ax, 4C00h
    int 21h

error:
    mov dx, msg_error
    mov ah, 09h
    int 21h
    mov ax, 4C01h
    int 21h

file_name:  db 'CHONK.TXT', 0
text:       db 'Hello file syscalls!', 13, 10
text_length equ $ - text
msg_read:   db 'Read back: $'
msg_ok:     db 'Files test passed', 13, 10, '$'
msg_error:  db 'Files test FAILED', 13, 10, '$'

handle:     dw 0
bytes_read: dw 0
buffer:     times 100 db 0
