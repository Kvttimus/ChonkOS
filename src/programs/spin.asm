; SPIN.COM - loops forever, to test stopping a stuck program w/ Ctrl+Break
org 100h
bits 16
cpu 8086

    mov dx, msg_spin
    mov ah, 09h
    int 21h

.forever:
    jmp .forever

msg_spin:   db 'Spinning forever, press Ctrl+Break to stop me', 13, 10, '$'
