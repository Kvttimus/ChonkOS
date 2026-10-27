; HELLO.COM - the smallest ChonkOS program
; .COM programs are loaded at PSP:0100 w/ cs = ds = es = ss, so org 100h
org 100h
bits 16
cpu 8086

    mov dx, msg_hello
    mov ah, 09h                         ; int 21h ah=09h: print "$" terminated string at ds:dx
    int 21h

    ret                                 ; a .COM can just return to exit (lands on int 20h in the PSP)

msg_hello:  db 'Hello from HELLO.COM!', 13, 10, '$'
