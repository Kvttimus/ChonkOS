org 0x7C00  ; sets offset starting at 0x7c00
bits 16  ; tells assembler to emit 16 bit code (starting here for backwards compatability)

%define ENDL 0x0D, 0x0A

; ------------------------------------------ FAT12 HEADER ------------------------------------------
jmp short start
nop

bdb_oem:                         db 'MSWIN4.1'  ; 8 bytes
bdb_bytes_per_sector:            dw 512
bdb_sectors_per_clustor:         db 1
bdb_reserved_sectors:            dw 1
bdb_fat_count:                   db 2
bdb_dir_entries_count:           dw 0E0h         
bdb_total_sectors:               dw 2880        ; 2880 * 512 = 1.44MB
bdb_media_descriptor_type:       db 0F0h        ; F0 = 3.5" floppy disk
bdb_sectors_per_fat:             dw 9           ; 9 sectors/fat
bdb_sectors_per_track:           dw 18
bdb_heads:                       dw 2
bdb_hidden_sectors:              dd 0
bdb_large_sector_count:          dd 0


; Extended Boot Record
ebr_drive_number:                db 0           ; 0x00 = floppy, 0x80 = hdd
                                 db 0           ; reserved byte
ebr_signature:                   db 29h
ebr_volume_id:                   db 12h, 34h, 56h, 78h  ; serial number, val doesn't matter
ebr_volume_label:                db 'CHONKY OS  '       ; 11 bytes, padded w/ spaces
ebr_system_id:                   db 'FAT12   '          ; 8 bytes, padded w/ spaces



; ------------------------------------------ CODE GOES HERE ------------------------------------------

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

    ; read smth from floppy disk
    ; BIOS should set DL to drive number
    mov [ebr_drive_number], dl

    mov ax, 1                     ; LBA=1, second sector from disk
    mov cl, 1                     ; 1 sector to read
    mov bx, 0x7E00                ; data should be after the bootloader
    call disk_read

    ; print msg
    mov si, msg_hello
    call puts

    cli                           ; disable interrupts, this way CPU can't get out of "halt" state
    hlt

; Error handlers
floppy_error:
    mov si, msg_read_failed
    call puts
    jmp wait_key_and_reboot

wait_key_and_reboot:
    mov ah, 0
    int 16h                       ; wait for keypress
    jmp 0FFFFh:0                  ; jmp to beginning of BIOS, should reboot

.halt:                            
    cli                           ; disable interrupts, this way CPU can't get out of "halt" state
    hlt


; Disk Routines

; Converts an LBA address to a CHS address
; Parameters:
;   - ax: LBA Address
; Returns
;   - cx [bits 0-5]: sector number
;   - cx [bits 6-15]: cylinder
;   - dh: head
lba_to_chs:
    push ax
    push dx

    xor dx, dx                                   ; dx = 0
    div word [bdb_sectors_per_track]             ; ax = LBA / SectorsPerTrack
                                                 ; dx = LBA % SectorsPerTrack (remainder)
    inc dx                                       ; dx = (LBA % SectorsPerTrack + 1) = sector
    mov cx, dx                                   ; cx = sector

    xor dx, dx                                   ; dx = 0
    div word [bdb_heads]                         ; ax = (LBA / SectorsPerTrack) / Heads = cylinder
                                                 ; dx = (LBA / SectorsPerTrack) % Heads = head
    mov dh, dl                                   ; dh = head
    mov ch, al                                   ; ch = cylinder (lower 8 bits of cylinder address)
    shl ah, 6
    or cl, ah                                    ; put upper 2 bits of cylinder in CL

    pop ax
    mov dl, al                                   ; restore DL
    pop ax
    ret

; Reads sectors from a disk
; Parameters:
;   - ax: LBA Address
;   - cl: # of sectors to read (up to 128)
;   - dl: drive #
;   - es:bx: memory address where to store read data
disk_read: 
    push ax                                      ; save registers we will modify
    push bx 
    push cx
    push dx
    push di

    push cx                                      ; temp save CL (# of sectors to read)
    call lba_to_chs                              ; compute CHS
    pop ax                                       ; AL = # of sectors to read
    
    mov ah, 02h
    mov di, 3                                    ; retry count

.retry:
    pusha                                        ; save all registers, we don't know what BIOS modifies
    stc                                          ; set carry flag, some BIOS's don't set it
    int 13h                                      ; carry flag cleared = succeed
    jnc .done                                    ; jmp if carry not set
    
    ; read failed
    popa
    call disk_reset

    dec di
    test di, di
    jnz .retry

.fail:
    ; all attempts have been exhausted
    jmp floppy_error

.done:
    popa

    pop di
    pop dx
    pop cx
    pop bx 
    pop ax                                      ; restore registers modified
    ret

; Resets disk controller
; Parameters:
;   - dl: drive #
disk_reset:
    pusha
    mov ah, 0
    stc
    int 13h
    jc floppy_error
    popa
    ret

msg_hello:                  db 'Hello World!', ENDL, 0
msg_read_failed:            db 'Read from Disk failed!', ENDL, 0

; pad the program to 510 bytes (we will boot on 512 byte disk)
times 510-($-$$) db 0             ; $-$$ gives out the length of the program so far, measured in bytes
dw 0xAA55                         ; last 2 bytes of the program (bytes 511-512)