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
    ; set up data segments
    mov ax, 0                     ; can't directly write to ds/es
    mov ds, ax
    mov es, ax

    ; setup stack
    mov ss, ax
    mov sp, 0x7C00                ; stack grows downwards from where we are loaded in memory

    ; some BIOSes might start us at 07C0:0000 instead of 0000:07C0, make sure we are in the
    ; expected location
    push es                       ; NOTE - this has a potential memory leak as there are "push" statements w/ no "pop"
    push word .after
    retf

.after:
    ; read smth from floppy disk
    ; BIOS should set DL to drive number
    mov [ebr_drive_number], dl

    ; show loading msg
    mov si, msg_loading
    call puts

    ; read drive parameters (sectors per track and head count),
    ; instead of just relying on data on formatted disk
    push es 
    mov ah, 08h
    int 13h
    jc floppy_error
    pop es
    
    and cl, 0x3F                        ; remove top 2 bits
    xor ch, ch
    mov [bdb_sectors_per_track], cx     ; sector count

    inc dh
    mov [bdb_heads], dh                 ; head count

    ; compute LBA of root dir = reserved + fat * sectors_per_fat 
    ; note: this section can be hardcoded
    mov ax, [bdb_sectors_per_fat]
    mov bl, [bdb_fat_count]
    xor bh, bh 
    ;xor bx, bx                              ; dx:ax = (fats * sectors_per_fat)
    mul bx
    add ax, [bdb_reserved_sectors]      ; ax = LBA of root dir
    push ax

    ; compute size of root dir = (32 * number_of_entries) /  bytes_per_sector
    mov ax, [bdb_sectors_per_fat]
    shl ax, 5                           ; ax *= 32
    xor dx, dx                          ; dx = 0
    div word [bdb_bytes_per_sector]    ; number of sectors we need to read

    test dx, dx                         ; if dx = 0, add 1
    jz .root_dir_after
    inc ax                              ; division remainder != 0, add 1
                                        ; this means we have a sector only partially filled w/ entries

.root_dir_after:
    ; read root directory
    mov cl, al                          ; cl = number of sectors to read = size of root dir
    pop ax                              ; al = LBA of root directory
    mov dl, [ebr_drive_number]          ; dl = drive number (we saved it previously)
    mov bx, buffer                      ; es:bx = buffer
    call disk_read

    ; search for kernel.bin
    xor bx, bx
    mov di, buffer

.search_kernel:
    mov si, file_stage2_bin
    mov cx, 11                          ; compare up to 11 chars
    push di
    repe cmpsb
    pop di
    je .found_kernel

    add di, 32
    inc bx
    cmp bx, [bdb_dir_entries_count]
    jl .search_kernel

    ; kernel not found
    jmp kernel_not_found_error

.found_kernel:
    ; di should have the address to the entry
    mov ax, [di + 26]                   ; first logical cluster field (offset 26)
    mov [stage2_cluster], ax

    ; load FAT from disk into memory
    mov ax, [bdb_reserved_sectors]
    mov bx, buffer
    mov cl, [bdb_sectors_per_fat]
    mov dl, [ebr_drive_number]
    call disk_read

    ; read kernel and process FAT chain
    mov bx, KERNEL_LOAD_SEGMENT
    mov es, bx
    mov bx, KERNEL_LOAD_OFFSET

.load_kernel_loop:
    ; read next cluster
    mov ax, [stage2_cluster]
    add ax, 31                          ; first cluster = (kernel_cluster - 2) * sectors_per_cluster + start_sector
                                        ; start sector = reserved + fats + root dir size = 1 + 18 + 134 = 33
    mov cl, 1
    mov dl, [ebr_drive_number]
    call disk_read

    add bx, [bdb_bytes_per_sector]      ; overflows if kernel.bin file size > 64 kib

    ; compute location of next cluster
    mov ax, [stage2_cluster]
    mov cx, 3
    mul cx
    mov cx, 2
    div cx                              ; ax = index of entry in FAT, dx = cluster % 2

    mov si, buffer
    add si, ax
    mov ax, [ds:si]                     ; read entry from FAT table at index ax

    or dx, dx
    jz .even 

.odd:
    shr ax, 4
    jmp .next_cluster_after

.even:
    and ax, 0x0FFF

.next_cluster_after:
    cmp ax, 0x0FF8                      ; end of chain
    jae .read_finish

    mov [stage2_cluster], ax
    jmp .load_kernel_loop

.read_finish:
    ; jump to our boot device in kernel
    mov dl, [ebr_drive_number]          ; boot device in dl
    mov ax, KERNEL_LOAD_SEGMENT         ; set segment registers
    mov ds, ax
    mov es, ax

    jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET

    jmp wait_key_and_reboot             ; should never happen

    cli                                 ; disable interrupts, this way CPU can't get out of "halt" state
    hlt

; Error handlers
floppy_error:
    mov si, msg_read_failed
    call puts
    jmp wait_key_and_reboot

kernel_not_found_error:
    mov si, msg_stage2_not_found
    call puts
    jmp wait_key_and_reboot

wait_key_and_reboot:
    mov ah, 0
    int 16h                       ; wait for keypress
    jmp 0FFFFh:0                  ; jmp to beginning of BIOS, should reboot

.halt:                            
    cli                           ; disable interrupts, this way CPU can't get out of "halt" state
    hlt

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

msg_loading:                db 'Loading...', ENDL, 0
msg_read_failed:            db 'Read from Disk failed!', ENDL, 0
msg_stage2_not_found:       db 'STAGE2.BIN file not found!', ENDL, 0   
file_stage2_bin:            db 'STAGE2  BIN'
stage2_cluster:             dw 0

KERNEL_LOAD_SEGMENT         equ 0x2000
KERNEL_LOAD_OFFSET          equ 0

; pad the program to 510 bytes (we will boot on 512 byte disk)
times 510-($-$$) db 0             ; $-$$ gives out the length of the program so far, measured in bytes
dw 0xAA55                         ; last 2 bytes of the program (bytes 511-512)

buffer: