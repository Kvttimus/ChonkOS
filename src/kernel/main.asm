org 0x7C00  ; sets offset starting at 0x7c00
bits 16  ; tells assembler to emit 16 bit code (starting here for backwards compatability)

%define ENDL 0x0D, 0x0A

; the "puts" instruction is above main so we have to add "jmp main" to ensure
; that the program starts at main and not at "puts"
start:
    jmp main

; Prints a string to the screen
; Params:
;   - ds:si points to string
puts:
    ; save registers we will modify
    push si 
    push ax 

.loop:
    lodsb                         ; loads next char in AL register
    or al, al                     ; verify if next character is null? (or'ing value w/ self doesn't modify it)
    jz .done                      ; if character is null, then we jmp outside loop (exit it)

    mov ah, 0x0e                  ; call BIOS interrupt
    mov bh, 0
    int 0x10

    jmp .loop                     ; if not null, then loop again

.done:
    pop ax
    pop si
    ret 

main:                             ; mark where code begins
    ; set up data segments
    mov ax, 0                     ; can't directly write to ds/es
    mov ds, ax
    mov es, ax

    ; setup stack
    mov ss, ax
    mov sp, 0x7C00                ; stack grows downwards from where we are loaded in memory

    ; print msg
    mov si, msg_hello
    call puts

    hlt

.halt:                            ; if CPU starts again, it will enter an infinite loop (maintain more control over the program)
    jmp .halt


msg_hello: db 'Hello World!', ENDL, 0

; pad the program to 510 bytes (we will boot on 512 byte disk)
times 510-($-$$) db 0             ; $-$$ gives out the length of the program so far, measured in bytes
dw 0xAA55                         ; last 2 bytes of the program (bytes 511-512)