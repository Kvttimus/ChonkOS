bits 16
cpu 8086

section _TEXT class=CODE

; NOTE: all functions here use the _cdecl calling convention in the small memory model:
; [bp + 0] - old call frame
; [bp + 2] - return address (small memory model => 2 bytes)
; [bp + 4] - first argument, [bp + 6] - second argument, ...
; bytes are converted to words (can't push a single byte on the stack)

; int 10h ah=0Eh
; args: character, page
global _x86_Video_WriteCharTeletype
_x86_Video_WriteCharTeletype:

    ; make new call frame
    push bp                             ; save old call frame
    mov bp, sp                          ; initialize new call frame

    ; save bx
    push bx

    ; [bp + 4] - first argument (character)
    ; [bp + 6] - second argument (page)
    mov ah, 0Eh
    mov al, [bp + 4]
    mov bh, [bp + 6]

    int 10h

    ; restore bx
    pop bx

    ; restore old call frame
    mov sp, bp
    pop bp
    ret


; int 12h - size of conventional memory
; uint16_t x86_GetMemorySize();
; returns: memory size in KiB (e.g. 640)
global _x86_GetMemorySize
_x86_GetMemorySize:
    int 12h
    ret


; int 13h ah=00h - reset disk system
; bool x86_Disk_Reset(uint8_t drive);
global _x86_Disk_Reset
_x86_Disk_Reset:
    push bp
    mov bp, sp

    mov ah, 0
    mov dl, [bp + 4]                    ; dl = drive number
    stc                                 ; set carry flag, some BIOS's don't set it
    int 13h

    mov ax, 1
    sbb ax, 0                           ; ax = 1 on success (carry clear), 0 on fail (carry set)

    mov sp, bp
    pop bp
    ret


; int 13h ah=02h - read sectors (CHS)
; bool x86_Disk_Read(uint8_t drive, uint16_t cylinder, uint16_t sector, uint16_t head,
;                    uint8_t count, void far* dataOut);
global _x86_Disk_Read
_x86_Disk_Read:
    push bp
    mov bp, sp

    push bx
    push es

    ; [bp + 4] - drive
    ; [bp + 6] - cylinder
    ; [bp + 8] - sector
    ; [bp + 10] - head
    ; [bp + 12] - count
    ; [bp + 14] - dataOut offset
    ; [bp + 16] - dataOut segment
    mov dl, [bp + 4]                    ; dl = drive number

    mov ch, [bp + 6]                    ; ch = cylinder (lower 8 bits)
    mov cl, [bp + 7]                    ; cl = cylinder (bits 8-9)
    ror cl, 1                           ; move bits 0-1 up to bits 6-7
    ror cl, 1                           ; (8086 can only shift by 1 or cl)
    and cl, 0C0h

    mov al, [bp + 8]
    and al, 3Fh
    or cl, al                           ; cl bits 0-5 = sector number

    mov dh, [bp + 10]                   ; dh = head
    mov al, [bp + 12]                   ; al = # of sectors to read

    mov bx, [bp + 16]                   ; es:bx = where to store data
    mov es, bx
    mov bx, [bp + 14]

    mov ah, 02h
    stc                                 ; set carry flag, some BIOS's don't set it
    int 13h

    mov ax, 1
    sbb ax, 0                           ; ax = 1 on success (carry clear), 0 on fail (carry set)

    pop es
    pop bx

    mov sp, bp
    pop bp
    ret


; int 13h ah=08h - get drive parameters
; bool x86_Disk_GetDriveParams(uint8_t drive, uint8_t* driveTypeOut, uint16_t* cylindersOut,
;                              uint16_t* sectorsOut, uint16_t* headsOut);
; NOTE: the original IBM PC BIOS doesn't support this for floppies, callers need a fallback
global _x86_Disk_GetDriveParams
_x86_Disk_GetDriveParams:
    push bp
    mov bp, sp

    push bx
    push si
    push di
    push es

    xor di, di                          ; es:di = 0000:0000 to work around BIOS bugs
    mov es, di
    mov dl, [bp + 4]                    ; dl = drive number
    mov ah, 08h
    stc
    int 13h
    jc .error

    ; drive type is in bl
    mov si, [bp + 6]
    mov [si], bl

    ; sectors = cl bits 0-5
    mov al, cl
    and al, 3Fh
    xor ah, ah
    mov si, [bp + 10]
    mov [si], ax

    ; cylinders = (ch | (cl bits 6-7) << 2) + 1  (BIOS returns the max cylinder index)
    mov bl, ch
    mov bh, cl
    mov cl, 6
    shr bh, cl
    inc bx
    mov si, [bp + 8]
    mov [si], bx

    ; heads = dh + 1  (BIOS returns the max head index)
    mov al, dh
    xor ah, ah
    inc ax
    mov si, [bp + 12]
    mov [si], ax

    mov ax, 1
    jmp .done

.error:
    xor ax, ax

.done:
    pop es
    pop di
    pop si
    pop bx

    mov sp, bp
    pop bp
    ret


; Switches to the kernel's stack & jumps to cs:ip with dl = boot drive, never returns
; void x86_StartKernel(uint16_t cs, uint16_t ip, uint16_t ss, uint16_t sp, uint8_t bootDrive);
global _x86_StartKernel
_x86_StartKernel:
    mov bp, sp

    ; [bp + 2] - cs (no call frame pushed)
    ; [bp + 4] - ip
    ; [bp + 6] - ss
    ; [bp + 8] - sp
    ; [bp + 10] - boot drive
    mov ax, [bp + 2]
    mov bx, [bp + 4]
    mov cx, [bp + 6]
    mov si, [bp + 8]
    mov dl, [bp + 10]

    ; interrupts off while changing ss:sp - some early 8088s don't block interrupts after "mov ss"
    cli
    mov ss, cx
    mov sp, si
    sti

    push ax                             ; far return address = cs:ip
    push bx
    retf


; ------------------------------------------------------------------------------------------------
; Watcom runtime helpers - the compiler calls these for 32-bit math because the 8086 only has
; 16-bit registers. They use Watcom's register calling convention, not _cdecl.
; ------------------------------------------------------------------------------------------------

; 32-bit multiply (same result for signed & unsigned, we only keep the low 32 bits)
; in:  dx:ax = multiplicand, cx:bx = multiplier
; out: dx:ax = product
global __U4M
global __I4M
__U4M:
__I4M:
    push si
    push di

    mov si, ax                          ; si = low(a)
    mov di, dx                          ; di = high(a)

    mul cx                              ; ax = low(low(a) * high(b))
    mov cx, ax

    mov ax, di
    mul bx                              ; ax = low(high(a) * low(b))
    add cx, ax                          ; cx = sum of the cross products

    mov ax, si
    mul bx                              ; dx:ax = low(a) * low(b)
    add dx, cx                          ; cross products only affect the high word

    pop di
    pop si
    ret


; 32-bit unsigned divide
; in:  dx:ax = dividend, cx:bx = divisor
; out: dx:ax = quotient, cx:bx = remainder
global __U4D
__U4D:
    test cx, cx
    jnz .long_divisor

    ; divisor fits in 16 bits, do it as 2 hardware divides
    mov cx, ax                          ; cx = low(dividend)
    mov ax, dx
    xor dx, dx
    div bx                              ; ax = high(quotient), dx = remainder so far
    xchg ax, cx                         ; cx = high(quotient), ax = low(dividend)
    div bx                              ; ax = low(quotient), dx = remainder
    mov bx, dx                          ; cx:bx = remainder
    mov dx, cx                          ; dx:ax = quotient
    xor cx, cx
    ret

.long_divisor:
    ; shift & subtract long division, 1 quotient bit per iteration
    push si
    push di
    push bp

    xor si, si                          ; di:si = remainder
    xor di, di
    mov bp, 32

.loop:
    shl ax, 1                           ; shift next dividend bit into the remainder
    rcl dx, 1                           ; (the quotient fills in from the bottom of dx:ax)
    rcl si, 1
    rcl di, 1
    jc .subtract                        ; remainder overflowed 32 bits, it's definitely >= divisor

    cmp di, cx                          ; remainder >= divisor ?
    jb .next
    ja .subtract
    cmp si, bx
    jb .next

.subtract:
    sub si, bx
    sbb di, cx
    inc ax                              ; set quotient bit

.next:
    dec bp
    jnz .loop

    mov bx, si                          ; cx:bx = remainder
    mov cx, di

    pop bp
    pop di
    pop si
    ret
