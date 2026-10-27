bits 16
cpu 8086

section x86_TEXT class=CODE

; NOTE: all functions here use the _cdecl calling convention in the large memory model:
; [bp + 0] - old call frame
; [bp + 2] - return address (large memory model => 4 bytes, segment:offset)
; [bp + 6] - first argument, [bp + 8] - second argument, ...
; bytes are converted to words (can't push a single byte on the stack)
; every function is called w/ a far call, so it returns w/ retf

; int 10h ah=0Eh
; void x86_Video_WriteCharTeletype(char c, uint8_t page);
global _x86_Video_WriteCharTeletype
_x86_Video_WriteCharTeletype:
    push bp
    mov bp, sp
    push bx

    mov ah, 0Eh
    mov al, [bp + 6]                    ; al = character
    mov bh, [bp + 8]                    ; bh = page
    int 10h

    pop bx
    mov sp, bp
    pop bp
    retf


; int 10h ah=0Fh - get current video mode
; uint8_t x86_Video_GetMode();
; returns: video mode (3 = 80x25 color text, 7 = 80x25 monochrome text, ...)
global _x86_Video_GetMode
_x86_Video_GetMode:
    push bx                             ; BIOS returns the active page in bh

    mov ah, 0Fh
    int 10h
    xor ah, ah                          ; ah = # of columns, we only want the mode in al

    pop bx
    retf


; int 10h ah=00h - set video mode, also clears the screen
; void x86_Video_SetMode(uint8_t mode);
global _x86_Video_SetMode
_x86_Video_SetMode:
    push bp
    mov bp, sp

    mov ah, 00h
    mov al, [bp + 6]                    ; al = mode
    int 10h

    mov sp, bp
    pop bp
    retf


; int 10h ah=03h - get cursor position (page 0)
; void x86_Video_GetCursor(uint8_t* rowOut, uint8_t* columnOut);
global _x86_Video_GetCursor
_x86_Video_GetCursor:
    push bp
    mov bp, sp
    push bx
    push cx                             ; BIOS returns the cursor shape in cx
    push di
    push es

    mov ah, 03h
    xor bh, bh                          ; page 0
    int 10h

    les di, [bp + 6]
    mov [es:di], dh                     ; dh = row
    les di, [bp + 10]
    mov [es:di], dl                     ; dl = column

    pop es
    pop di
    pop cx
    pop bx
    mov sp, bp
    pop bp
    retf


; int 10h ah=02h - set cursor position (page 0)
; void x86_Video_SetCursor(uint8_t row, uint8_t column);
global _x86_Video_SetCursor
_x86_Video_SetCursor:
    push bp
    mov bp, sp
    push bx

    mov ah, 02h
    xor bh, bh                          ; page 0
    mov dh, [bp + 6]                    ; dh = row
    mov dl, [bp + 8]                    ; dl = column
    int 10h

    pop bx
    mov sp, bp
    pop bp
    retf


; int 12h - size of conventional memory
; uint16_t x86_GetMemorySize();
; returns: memory size in KiB (e.g. 640)
global _x86_GetMemorySize
_x86_GetMemorySize:
    int 12h
    retf


; int 13h ah=00h - reset disk system
; bool x86_Disk_Reset(uint8_t drive);
global _x86_Disk_Reset
_x86_Disk_Reset:
    push bp
    mov bp, sp

    mov ah, 0
    mov dl, [bp + 6]                    ; dl = drive number
    stc                                 ; set carry flag, some BIOS's don't set it
    int 13h

    mov ax, 1
    sbb ax, 0                           ; ax = 1 on success (carry clear), 0 on fail (carry set)

    mov sp, bp
    pop bp
    retf


; int 13h ah=02h (read) / ah=03h (write) - transfer sectors (CHS)
; bool x86_Disk_Read(uint8_t drive, uint16_t cylinder, uint16_t sector, uint16_t head,
;                    uint8_t count, void far* data);
; bool x86_Disk_Write(... same ...);
global _x86_Disk_Read
_x86_Disk_Read:
    mov ah, 02h
    jmp disk_transfer

global _x86_Disk_Write
_x86_Disk_Write:
    mov ah, 03h

disk_transfer:
    push bp
    mov bp, sp

    push bx
    push es

    ; [bp + 6] - drive
    ; [bp + 8] - cylinder
    ; [bp + 10] - sector
    ; [bp + 12] - head
    ; [bp + 14] - count
    ; [bp + 16] - data offset
    ; [bp + 18] - data segment
    mov dl, [bp + 6]                    ; dl = drive number

    mov ch, [bp + 8]                    ; ch = cylinder (lower 8 bits)
    mov cl, [bp + 9]                    ; cl = cylinder (bits 8-9)
    ror cl, 1                           ; move bits 0-1 up to bits 6-7
    ror cl, 1                           ; (8086 can only shift by 1 or cl)
    and cl, 0C0h

    mov al, [bp + 10]
    and al, 3Fh
    or cl, al                           ; cl bits 0-5 = sector number

    mov dh, [bp + 12]                   ; dh = head
    mov al, [bp + 14]                   ; al = # of sectors

    les bx, [bp + 16]                   ; es:bx = data

    stc                                 ; set carry flag, some BIOS's don't set it
    int 13h

    mov ax, 1
    sbb ax, 0                           ; ax = 1 on success (carry clear), 0 on fail (carry set)

    pop es
    pop bx

    mov sp, bp
    pop bp
    retf


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

    ; [bp + 6] - drive
    ; [bp + 8] - driveTypeOut (far pointer)
    ; [bp + 12] - cylindersOut
    ; [bp + 16] - sectorsOut
    ; [bp + 20] - headsOut
    xor di, di                          ; es:di = 0000:0000 to work around BIOS bugs
    mov es, di
    mov dl, [bp + 6]                    ; dl = drive number
    mov ah, 08h
    stc
    int 13h
    jc .error

    ; drive type is in bl
    les di, [bp + 8]
    mov [es:di], bl

    ; sectors = cl bits 0-5
    mov al, cl
    and al, 3Fh
    xor ah, ah
    les di, [bp + 16]
    mov [es:di], ax

    ; cylinders = (ch | (cl bits 6-7) << 2) + 1  (BIOS returns the max cylinder index)
    mov bl, ch
    mov bh, cl
    mov cl, 6
    shr bh, cl
    inc bx
    les di, [bp + 12]
    mov [es:di], bx

    ; heads = dh + 1  (BIOS returns the max head index)
    mov al, dh
    xor ah, ah
    inc ax
    les di, [bp + 20]
    mov [es:di], ax

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
    retf


; int 1Ah ah=02h - read real time clock time (AT & later, a PC/XT has no clock chip)
; uint32_t x86_RTC_GetTime();
; returns: 0x00HHMMSS in BCD, or 0xFFFFFFFF if there's no clock
global _x86_RTC_GetTime
_x86_RTC_GetTime:
    push cx
    mov ah, 02h
    stc
    int 1Ah
    jc rtc_error

    ; BIOS returns ch = hours, cl = minutes, dh = seconds
    mov al, dh                          ; dx:ax = 00 HH : MM SS
    mov ah, cl
    mov dl, ch
    xor dh, dh
    pop cx
    retf


; int 1Ah ah=04h - read real time clock date
; uint32_t x86_RTC_GetDate();
; returns: 0xCCYYMMDD in BCD (century, year, month, day), or 0xFFFFFFFF if there's no clock
global _x86_RTC_GetDate
_x86_RTC_GetDate:
    push cx
    mov ah, 04h
    stc
    int 1Ah
    jc rtc_error

    mov ax, dx                          ; ax = MM DD
    mov dx, cx                          ; dx = CC YY
    pop cx
    retf

rtc_error:
    mov ax, 0FFFFh
    mov dx, ax
    pop cx
    retf


; Waits until the next interrupt
; void x86_Halt();
global _x86_Halt
_x86_Halt:
    hlt
    retf


; Stops the CPU for good, never returns
; void x86_HaltForever();
global _x86_HaltForever
_x86_HaltForever:
    cli
.halt:
    hlt                                 ; an NMI can wake us up from hlt, so loop back
    jmp .halt


; void x86_DisableInterrupts();
global _x86_DisableInterrupts
_x86_DisableInterrupts:
    cli
    retf


; void x86_EnableInterrupts();
global _x86_EnableInterrupts
_x86_EnableInterrupts:
    sti
    retf


; uint8_t x86_InByte(uint16_t port);
global _x86_InByte
_x86_InByte:
    push bp
    mov bp, sp

    mov dx, [bp + 6]                    ; dx = port
    in al, dx
    xor ah, ah

    mov sp, bp
    pop bp
    retf


; void x86_OutByte(uint16_t port, uint8_t value);
global _x86_OutByte
_x86_OutByte:
    push bp
    mov bp, sp

    mov dx, [bp + 6]                    ; dx = port
    mov al, [bp + 8]                    ; al = value
    out dx, al

    mov sp, bp
    pop bp
    retf


; Warm reboots the machine (like Ctrl+Alt+Del), never returns
; void x86_Reboot();
global _x86_Reboot
_x86_Reboot:
    cli
    xor ax, ax
    mov ds, ax
    mov word [0472h], 1234h             ; BIOS data area flag: 1234h = warm boot, skip memory test
    jmp 0FFFFh:0                        ; jmp to beginning of BIOS


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
    retf


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
    retf

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
    retf
