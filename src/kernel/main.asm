org 0x0 
bits 16  ; tells assembler to emit 16 bit code (starting here for backwards compatability)

%define ENDL 0x0D, 0x0A

; the "puts" instruction is above main so we have to add "jmp main" to ensure
; that the program starts at main and not at "puts"
start:
    ; print msg
    mov si, msg_hello
    call puts

.halt:
    cli
    hlt


; Prints a string to the screen
; Params:
;   - ds:si points to string
puts:
    ; save registers we will modify
    push si 
    push ax 
    push bx

.loop:
    lodsb                         ; loads next char in AL register
    or al, al                     ; verify if next character is null? (or'ing value w/ self doesn't modify it)
    jz .done                      ; if character is null, then we jmp outside loop (exit it)

    mov ah, 0x0e                  ; call BIOS interrupt
    mov bh, 0
    int 0x10

    jmp .loop                     ; if not null, then loop again

.done:
    pop bx
    pop ax
    pop si
    ret 

msg_hello: db ' YAYAYAYAY FAT SUCCESSFUL', ENDL, 0