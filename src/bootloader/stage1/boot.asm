org 0x7C00  ; sets offset starting at 0x7c00
bits 16  ; tells assembler to emit 16 bit code (starting here for backwards compatability)
cpu 8086  ; reject any instruction a real 8086/8088 can't run (pusha, shl reg imm, push imm, ...)

%define ENDL 0x0D, 0x0A

; ------------------------------------------ FAT12 HEADER ------------------------------------------
jmp short start
nop

bdb_oem:                         db 'MSWIN4.1'  ; 8 bytes
bdb_bytes_per_sector:            dw 512
bdb_sectors_per_cluster:         db 1
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

start:
    ; set up data segments
    xor ax, ax                    ; can't directly write to ds/es
    mov ds, ax
    mov es, ax

    ; setup stack
    ; interrupts off while changing ss:sp - some early 8088s don't block interrupts after "mov ss"
    cli
    mov ss, ax
    mov sp, 0x7C00                ; stack grows downwards from where we are loaded in memory
    sti

    ; some BIOSes might start us at 07C0:0000 instead of 0000:7C00, far jump to make sure
    ; cs is what we expect
    jmp 0:.after

.after:
    ; BIOS should set DL to drive number
    mov [ebr_drive_number], dl

    ; show loading msg
    mov si, msg_loading
    call puts

    ; NOTE: drive geometry (sectors per track, heads) comes from the BPB above.
    ; int 13h ah=08h isn't supported for floppies on the original IBM PC BIOS

    ; compute LBA of root dir = reserved + fats * sectors_per_fat
    mov ax, [bdb_sectors_per_fat]
    mov bl, [bdb_fat_count]
    xor bh, bh
    mul bx                              ; dx:ax = (fats * sectors_per_fat)
    add ax, [bdb_reserved_sectors]      ; ax = LBA of root dir
    push ax

    ; compute size of root dir = (32 * number_of_entries) / bytes_per_sector
    mov ax, [bdb_dir_entries_count]
    mov cl, 5
    shl ax, cl                          ; ax *= 32
    xor dx, dx                          ; dx = 0
    div word [bdb_bytes_per_sector]     ; number of sectors we need to read

    test dx, dx                         ; if dx = 0, add 1
    jz .root_dir_after
    inc ax                              ; division remainder != 0, add 1
                                        ; this means we have a sector only partially filled w/ entries

.root_dir_after:
    ; read root directory
    mov cx, ax                          ; cl = number of sectors to read = size of root dir
    pop ax                              ; ax = LBA of root directory
    mov [data_lba], ax
    add [data_lba], cx                  ; data region starts right after the root dir
    mov dl, [ebr_drive_number]          ; dl = drive number (we saved it previously)
    mov bx, buffer                      ; es:bx = buffer
    call disk_read

    ; search for stage2.bin
    xor bx, bx
    mov di, buffer

.search_stage2:
    mov si, file_stage2_bin
    mov cx, 11                          ; compare up to 11 chars
    push di
    repe cmpsb
    pop di
    je .found_stage2

    add di, 32
    inc bx
    cmp bx, [bdb_dir_entries_count]
    jl .search_stage2

    ; stage2 not found
    jmp stage2_not_found_error

.found_stage2:
    ; di should have the address to the entry
    mov ax, [di + 26]                   ; first logical cluster field (offset 26)
    mov [stage2_cluster], ax

    ; load FAT from disk into memory
    mov ax, [bdb_reserved_sectors]
    mov bx, buffer
    mov cl, [bdb_sectors_per_fat]
    mov dl, [ebr_drive_number]
    call disk_read

    ; read stage2 and process FAT chain
    mov bx, STAGE2_LOAD_SEGMENT
    mov es, bx
    mov bx, STAGE2_LOAD_OFFSET

.load_stage2_loop:
    ; read next cluster, LBA = (cluster - 2) * sectors_per_cluster + data_lba
    mov ax, [stage2_cluster]
    dec ax
    dec ax
    mov cl, [bdb_sectors_per_cluster]
    xor ch, ch
    mul cx                              ; ax = (cluster - 2) * sectors_per_cluster (clobbers dx)
    add ax, [data_lba]
    mov dl, [ebr_drive_number]
    call disk_read                      ; advances es:bx past the data it read

    ; compute location of next cluster, FAT12 entry is at byte offset cluster * 3 / 2
    mov ax, [stage2_cluster]
    mov si, ax
    shr si, 1
    add si, ax                          ; si = cluster * 3 / 2
    mov dx, [buffer + si]               ; read entry from FAT table (ds = 0)

    test al, 1                          ; odd cluster -> entry is in the top 12 bits
    jz .even

.odd:
    mov cl, 4
    shr dx, cl
    jmp .next_cluster_after

.even:
    and dh, 0x0F                        ; even cluster -> entry is in the bottom 12 bits

.next_cluster_after:
    cmp dx, 0x0FF8                      ; end of chain
    jae .read_finish

    mov [stage2_cluster], dx
    jmp .load_stage2_loop

.read_finish:
    ; jump to stage2 w/ boot device in dl
    mov dl, [ebr_drive_number]          ; boot device in dl
    mov ax, STAGE2_LOAD_SEGMENT         ; set segment registers
    mov ds, ax
    mov es, ax

    jmp STAGE2_LOAD_SEGMENT:STAGE2_LOAD_OFFSET

; Error handlers
floppy_error:
    mov si, msg_read_failed
    jmp short error_and_reboot

stage2_not_found_error:
    mov si, msg_stage2_not_found

error_and_reboot:
    call puts
    xor ah, ah
    int 16h                       ; wait for keypress
    jmp 0FFFFh:0                  ; jmp to beginning of BIOS, should reboot

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
    ror ah, 1                                    ; move bits 0-1 of ah (upper 2 bits of cylinder)
    ror ah, 1                                    ; up to bits 6-7 (8086 can only shift by 1 or cl)
    or cl, ah                                    ; put upper 2 bits of cylinder in CL

    pop ax
    mov dl, al                                   ; restore DL
    pop ax
    ret

; Reads sectors from a disk, one sector per BIOS call
; - old BIOSes can't read across a track boundary in one call
; - with es:bx 512-aligned, a single sector never crosses a 64KiB DMA boundary
; Parameters:
;   - ax: LBA Address
;   - cl: # of sectors to read (1-255)
;   - dl: drive #
;   - es:bx: memory address where to store read data
; Returns
;   - es:bx: points just past the data that was read
disk_read:
    push ax                                      ; save registers we will modify
    push cx
    push dx
    push di

.next_sector:
    push ax                                      ; save LBA
    push cx                                      ; save # of sectors left
    call lba_to_chs                              ; compute CHS
    mov di, 3                                    ; retry count

.retry:
    mov ax, 0201h                                ; ah = 02h (read), al = 1 sector
    stc                                          ; set carry flag, some BIOS's don't set it
    int 13h                                      ; carry flag cleared = succeed
    jnc .sector_done                             ; jmp if carry not set

    ; read failed, reset disk controller & try again
    xor ah, ah
    int 13h

    dec di
    jnz .retry

    ; all attempts have been exhausted
    jmp floppy_error

.sector_done:
    pop cx
    pop ax
    inc ax                                       ; next LBA

    add bx, [bdb_bytes_per_sector]               ; next spot in memory
    jnc .no_wrap
    mov di, es                                   ; bx wrapped past 64KiB, move es up 64KiB
    add di, 1000h
    mov es, di

.no_wrap:
    dec cl
    jnz .next_sector

    pop di
    pop dx
    pop cx
    pop ax                                      ; restore registers modified
    ret

msg_loading:                db 'Loading...', ENDL, 0
msg_read_failed:            db 'Disk read failed!', ENDL, 0
msg_stage2_not_found:       db 'STAGE2.BIN not found!', ENDL, 0
file_stage2_bin:            db 'STAGE2  BIN'

STAGE2_LOAD_SEGMENT         equ 0x2000          ; 64KiB aligned so sector reads stay DMA safe
STAGE2_LOAD_OFFSET          equ 0

; variables live in free conventional memory below the stack, they don't take up boot sector space
stage2_cluster              equ 0x0500
data_lba                    equ 0x0502

; pad the program to 510 bytes (we will boot on 512 byte disk)
times 510-($-$$) db 0             ; $-$$ gives out the length of the program so far, measured in bytes
dw 0xAA55                         ; last 2 bytes of the program (bytes 511-512)

buffer:
