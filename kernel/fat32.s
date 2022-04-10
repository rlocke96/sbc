;----------------------------------------------------------------------
; fat32.s
; by Robert Locke
; Started: 03/20/2022
; reference:
; 	https://www.ntfs.com/partition-table.htm
;	https://www.ntfs.com/fat_systems.htm
;	https://www.adrian.idv.hk/2009-11-15-fat32/
;----------------------------------------------------------------------


  .include "lib.inc"
  .include "bios.inc"
  .include "sdcard.inc"
  .include "fat32.inc"
  .include "regs.inc"

;  .import sector_buffer, sector_buffer_end, sector_lba
  .import filename_char_ucs2_to_internal, filename_char_internal_to_ucs2
  .import filename_cp437_to_internal, filename_char_internal_to_cp437
  .import match_name, match_type

FLAG_IN_USE	= 1<<0	; Context in use
FLAG_DIRTY	= 1<<1	; Buffer is dirty
FLAG_DIRENT	= 1<<2	; Directory entry needs to be updated on close

.segment "BSS"

_fat32_bss_start:

fat32_time_year:     .byte 0
fat32_time_month:    .byte 0
fat32_time_day:      .byte 0
fat32_time_hours:    .byte 0
fat32_time_minutes:  .byte 0
fat32_time_seconds:  .byte 0

; Temp
bytecnt:             .word 0       ; Used by fat32_write
tmp_buf:             .res 4        ; Used by save_sector_buffer, fat32_rename
next_sector_arg:     .byte 0       ; Used by next_sector to store argument
tmp_bufptr:          .word 0       ; Used by next_sector
tmp_sector_lba:      .dword 0      ; Used by next_sector
name_offset:         .byte 0
tmp_dir_cluster:     .dword 0
tmp_attrib:          .byte 0       ; temporary: attribute when creating a dir entry
tmp_dirent_flag:     .byte 0
shortname_buf:       .res 11       ; Used for shortname creation
tmp_timestamp:       .byte 0
tmp_filetype:        .byte 0       ; Used to match file type in find_dirent

; Temp - LFN
lfn_index:           .byte 0       ; counter when collecting/decoding LFN entries
lfn_count:           .byte 0       ; number of LFN dir entries when reading/creating
lfn_checksum:        .byte 0       ; created or expected LFN checksum
lfn_char_count:      .byte 0       ; counter when decoding LFN characters
lfn_name_index:      .byte 0       ; counter when decoding LFN characters
tmp_sfn_case:        .byte 0       ; flags when decoding SFN characters
free_entry_count:    .byte 0       ; counter when looking for contig. free dir entries
marked_entry_lba:    .res 4        ; mark/rewind data for directory entries
marked_entry_cluster:.res 4
marked_entry_offset: .res 2
tmp_entry:           .res 21       ; SFN entry fields except name, saved during rename
lfn_buf:             .res 20*32    ; create/collect LFN; 20 dirents (13c * 20 > 255c)

; API arguments and return data
fat32_dirent:        .tag dirent   ; Buffer containing decoded directory entry
fat32_size:          .res 4        ; Used for fat32_read, fat32_write, fat32_get_offset, fat32_get_free_space
fat32_errno:         .byte 0       ; Last error
fat32_readonly:      .byte 0       ; User-accessible read-only flag

.struct context
flags		.byte	; Flag bits
start_cluster	.dword	; Start cluster
cluster		.dword	; Current cluster
lba		.dword	; Sector of current cluster
cluster_sector	.byte	; Sector index within current cluster
bufptr		.word	; Pointer within sector_buffer
file_size	.dword	; Size of current file
file_offset	.dword	; Offset in current file
dirent_lba	.dword	; Sector containing directory entry for this file
dirent_bufptr	.word	; Offset to start of directory entry
eof		.byte	; =$ff: EOF has been reached
.endstruct

CONTEXT_SIZE = 32

.if CONTEXT_SIZE * FAT32_CONTEXTS > 256
.error "Too many FAT32_CONTEXTS to fix into 256 bytes!"
.endif

.if .sizeof(context) > CONTEXT_SIZE
.error ".struct context > CONTEXT_SIZE"
.endif

.struct fs
mounted			.byte	; Flag to indicate the volume is mounted
rootdir_cluster		.dword	; Cluster of root directory
sectors_per_cluster	.byte	; Sectors per cluster
cluster_shift		.byte	; Log2 of Sectors Per Cluster
lba_partition		.dword	; Start sector of FAT#@ partition
fat_size		.dword	; Size in sectors of each FAT table
lba_fat			.dword	; Start sector of first FAT Table
lba_data		.dword	; Start sector of first DATA Cluster
cluster_count		.dword	; Total number of cluster on volume
lba_fsinfo		.dword	; Sector number of FS info
; Variables
free_clusters		.dword	; Number of free clusters
free_cluster		.dword	; Cluster to start search for free clusters
cwd_cluster		.dword	; Cluster of current directory
.endstruct

FS_SIZE		=	64

.if FS_SIZE * FAT32_VOLUMES > 256
.error "Too many FAT32_VOLUMES to fit into 256 bytes!"
.endif

.if .sizeof(fs) > FS_SIZE
.error ".struct fs too big!"
.endif

; Contexts
context_inx:		.byte 0 	; Index of current context
cur_context:		.tag context 	; Current file descriptor state
contexts_inuse:		.res FAT32_CONTEXTS
.if ::FAT32_VOLUMES > 1
volume_for_context:	.res FAT32_CONTEXTS
.endif

; Volumes
volume_idx:	.byte 0	; Index of current filesystem
cur_volume:	.tag fs

.if FAT32_CONTEXTS > 1
contexts:            .res CONTEXT_SIZE * FAT32_CONTEXTS
.endif

.if FAT32_VOLUMES > 1
volumes:             .res FS_SIZE * FAT32_VOLUMES
.endif
; fat32_time

; Partition Table Offsets
ptable		= $1be
ptable_size	= $10
ptable_type	= $04
ptable_start	= $08
ptable_length	= $0c
ptable_state	= $00

; Fat32 Table Offsets
BPB_BytsPerSec  	= $0B ; 2  Bytes
BPB_SecPerClus  	= $0D ; 1  Bytes
BPB_RsvdSecCnt  	= $0E ; 2  Bytes
BPB_NumFATs         	= $10 ; 1  Bytes
BPB_RootEntries		= $11 ; 2  Bytes
BPB_FATSz32         	= $24 ; 4  Bytes
BPB_RootClus      	= $2C ; 4  Bytes
BPB_Name        	= $47 ; 11 Bytes

_fat32_bss_end:

.segment "CODE"

;----------------------------------------------------------------------
; load_mbr_sector
;
;----------------------------------------------------------------------
load_mbr_sector:
  set32_val cur_context + context::lba, 0
  jmp load_sector_buffer

;----------------------------------------------------------------------
; load_sector_buffer
;
; * c=0: failure; sets errno
;----------------------------------------------------------------------
load_sector_buffer:
  ; Check if sector is already loaded
  cmp32_ne cur_context + context::lba, sector_lba, @do_load
  sec
  rts

@do_load:
  set32 sector_lba, cur_context + context::lba
  jsr sdcard_read_sector
  bcc @1
  rts

@1:
  lda #ERRNO_READ
  jmp set_errno

;-----------------------------------------------------------------------------
; set_errno
;
; Only set errno if it wasn't already set.
; If a read error causes a file not found error, it's still a read error.
;-----------------------------------------------------------------------------
set_errno:
  clc
  pha
  lda fat32_errno
  bne @1
  pla
  sta fat32_errno
  rts

@1:
  pla
  rts  

;-----------------------------------------------------------------------------
; fat32_get_ptable_entry
;
; Returns a given partition table entry
;
; In:	A	Index
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
fat32_get_ptable_entry:
  stz fat32_errno

  cmp #$4	; Only 4 partition volumes allowed
  bcs @error 	; end of list

  asl
  asl
  asl
  asl 	; Multiple A by 16. Part Table is 16 length 

  pha	
  jsr load_mbr_sector
  pla
  bcs @1
@error:
  clc
  rts

@1:
  ; Start LBA
  tax
  phx
  ldy #0
@2:
  lda sector_buffer + $1be + 8, x
  sta fat32_dirent + dirent::start, y
  inx
  iny
  cpy #4
  bne @2
  plx

  ; size
  phx
  ldy #0
@3:
  lda sector_buffer + $1be + 12, x
  sta fat32_dirent + dirent::size, y
  inx
  iny
  cpy #4
  bne @3
  plx

  ; type
  lda sector_buffer + $1be + 4, x
  sta fat32_dirent + dirent::attributes
  stz fat32_dirent + dirent::name

  cmp #$0b
  beq @read_name
  cmp #$0c
  bne @done

@read_name:
  ; Read first sector of partition
  set32 cur_context + context::lba, fat32_dirent + dirent::start
  jsr load_sector_buffer
  bcc @error

  set16_val fat32_bufptr, (sector_buffer + $47)
  jsr decode_volume_label

@done:
  sec
  rts

;-----------------------------------------------------------------------------
; decode_volume_label
;-----------------------------------------------------------------------------
decode_volume_label:
  ldy #0
@1:
  lda (fat32_bufptr), y
  sta fat32_dirent + dirent::name, y
  iny
  cpy #11
  bne @1
  dey
  lda #$20
@2:
  cmp fat32_dirent + dirent::name, y
  bne @3
  dey
  bpl @2
@3:
  iny
  lda #0
  sta fat32_dirent + dirent::name, y
  rts

;-----------------------------------------------------------------------------
; fat32_init
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
fat32_init:
  ; Clear FAT32 BSS
  set16_val fat32_bufptr, _fat32_bss_start
  lda #0
@1:
  sta (fat32_bufptr)
  inc fat32_bufptr + 0
  bne @2
  inc fat32_bufptr + 1
@2:
  ldx fat32_bufptr + 0
  cpx #<_fat32_bss_end
  bne @1
  ldx fat32_bufptr + 1
  cpx #>_fat32_bss_end
  bne @1

  ; Make sure sector_lba is non-zero
  ; (was overwritten by sdcard_init)
  lda #$FF
  sta sector_lba

  ; No current volume
  sta volume_idx

  ; No time set up
  sta fat32_time_year

  sec
  rts

;-----------------------------------------------------------------------------
; fat32_alloc_context
;
; In:  a     volume
; Out: a     context
;      c     =0: failure
;      errno =ERRNO_OUT_OF_RESOURCES: all contexts in use
;            =ERRNO_READ            : error mounting volume
;            =ERRNO_WRITE           : error mounting volume
;            =ERRNO_NO_FS           : error mounting volume
;            =ERRNO_FS_INCONSISTENT : error mounting volume
;-----------------------------------------------------------------------------
fat32_alloc_context:
        stz fat32_errno

.if FAT32_VOLUMES > 1
        tay ; volume
.endif
        ldx #0
@1:     lda contexts_inuse, x
        beq @found_free
        inx
        cpx #FAT32_CONTEXTS
        bne @1

        lda #ERRNO_OUT_OF_RESOURCES
        jmp set_errno

@found_free:
        lda #1
        sta contexts_inuse, x

.if FAT32_VOLUMES > 1
        tya
        sta volume_for_context, x
        phx
        cmp #$ff
        sec
        beq @2
        clc
        jsr set_volume
@2:     pla
        bcs @rts
        jsr fat32_free_context
        clc
        rts
.else
        txa
        sec
.endif
@rts:
        rts

;-----------------------------------------------------------------------------
; fat32_free_context
;
; In:  a     context
; Out: c     =0: failure
;-----------------------------------------------------------------------------
fat32_free_context:
        cmp #FAT32_CONTEXTS
        bcc @1
@fail:  clc
        rts
@1:
        tax
        lda contexts_inuse, x
        beq @fail
        stz contexts_inuse, x
        sec
        rts

;-----------------------------------------------------------------------------
; set_volume
;
; In:  a  volume
;      c  =1: don't mount
;
; * c=0: failure
;-----------------------------------------------------------------------------
set_volume:
        php ; mount flag

        ; Already selected?
        cmp volume_idx
        bne @0
        plp
        sec
        rts

@0:
        ; Valid volume index?
        cmp #FAT32_VOLUMES
        bcc @ok

        plp
        lda #ERRNO_NO_FS
        jmp set_errno

@ok:
.if ::FAT32_VOLUMES > 1
        ; Save new volume index
        pha

        .assert FS_SIZE = 64, error
        ; Copy current volume back
        lda volume_idx
        bmi @dont_write_back ; < 0 = no current volume
        asl ; X=A*64
        asl
        asl
        asl
        asl
        asl
        tax

        ldy #0
@1:     lda cur_volume, y
        sta volumes, x
        inx
        iny
        cpy #(.sizeof(fs))
        bne @1

@dont_write_back:
        ; Copy new volume to current
        pla              ; Get new volume idx
        pha
        asl ; X=A*64
        asl
        asl
        asl
        asl
        asl
        tax

        ldy #0
@2:     lda volumes, x
        sta cur_volume, y
        inx
        iny
        cpy #(.sizeof(fs))
        bne @2

        pla
.endif

        sta volume_idx

        plp
        bcs @done ; don't mount
        bit cur_volume + fs::mounted
        bmi @done
        lda volume_idx
        jmp mount
@done:
        sec
        rts

;-----------------------------------------------------------------------------
; mount
;
; In:  a  partition number (0+)
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
mount:
        pha ; partition number

        jsr load_mbr_sector
        pla
        bcs @2a
@error: clc
        rts

@2a:    asl ; *16
        asl
        asl
        asl
        tax

        ; Check partition type
        lda sector_buffer + $1BE + 4, x
        cmp #$0B
        beq @3
        cmp #$0C
        beq @3
        lda #ERRNO_NO_FS
        jmp set_errno

@3:
        ; Get LBA of partition
        lda sector_buffer + $1BE + 8 + 0, x
        sta cur_volume + fs::lba_partition + 0
        lda sector_buffer + $1BE + 8 + 1, x
        sta cur_volume + fs::lba_partition + 1
        lda sector_buffer + $1BE + 8 + 2, x
        sta cur_volume + fs::lba_partition + 2
        lda sector_buffer + $1BE + 8 + 3, x
        sta cur_volume + fs::lba_partition + 3

        ; Read first sector of partition
        set32 cur_context + context::lba, cur_volume + fs::lba_partition
        jsr load_sector_buffer
        bcc @error

        ; Some sanity checks
        lda sector_buffer + 510 ; Check signature
        cmp #$55
        beq :+
@fs_inconsistent:
        lda #ERRNO_FS_INCONSISTENT
        jmp set_errno
:       lda sector_buffer + 511
        cmp #$AA
        bne @fs_inconsistent
        lda sector_buffer + 16 ; # of FATs should be 2
        cmp #2
        bne @fs_inconsistent
        lda sector_buffer + 17 ; Root entry count = 0 for FAT32
        bne @fs_inconsistent
        lda sector_buffer + 18
        bne @fs_inconsistent

        ; Get sectors per cluster
        lda sector_buffer + 13
        sta cur_volume + fs::sectors_per_cluster
        beq @fs_inconsistent

        ; Calculate shift amount based on sectors per cluster
        ; cluster_shift already 0
@4:     lsr
        beq @5
        inc cur_volume + fs::cluster_shift
        bra @4
@5:
        ; FAT size in sectors
        set32 cur_volume + fs::fat_size, sector_buffer + 36

        ; Root cluster
        set32 cur_volume + fs::rootdir_cluster, sector_buffer + 44

        ; Calculate LBA of first FAT
        add32_16 cur_volume + fs::lba_fat, cur_volume + fs::lba_partition, sector_buffer + 14

        ; Calculate LBA of first data sector
        add32 cur_volume + fs::lba_data, cur_volume + fs::lba_fat, cur_volume + fs::fat_size
        add32 cur_volume + fs::lba_data, cur_volume + fs::lba_data, cur_volume + fs::fat_size

        ; Calculate number of clusters on volume: (total_sectors - lba_data) >> cluster_shift
        set32 cur_volume + fs::cluster_count, sector_buffer + 32
        sub32 cur_volume + fs::cluster_count, cur_volume + fs::cluster_count, cur_volume + fs::lba_data
        ldy cur_volume + fs::cluster_shift
        beq @7
@6:     shr32 cur_volume + fs::cluster_count
        dey
        bne @6
@7:
        ; Get FS info sector
        add32_16 cur_volume + fs::lba_fsinfo, cur_volume + fs::lba_partition, sector_buffer + 48

        ; Load FS info sector
        set32 cur_context + context::lba, cur_volume + fs::lba_fsinfo
        jsr load_sector_buffer
        bcs @8
        rts
@8:
        ; Get number of free clusters
        set32 cur_volume + fs::free_clusters, sector_buffer + 488

        ; Set initial start point for free cluster search
        set32_val cur_volume + fs::free_cluster, 2

        ; Success
        lda #$80
        sta cur_volume + fs::mounted
        sec
        rts

;-----------------------------------------------------------------------------
; fat32_open
;
; Open file specified in string pointed to by fat32_ptr
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
fat32_open:
        stz fat32_errno

        ; Check if context is free
        lda cur_context + context::flags
        bne @error

        ; Find file
        jsr find_file
        bcs @1
        lda #ERRNO_FILE_NOT_FOUND
        jmp set_errno

@1:
        ; Open file
        stz cur_context + context::eof
        set32_val cur_context + context::file_offset, 0
        set32 cur_context + context::file_size, fat32_dirent + dirent::size
        set32 cur_context + context::start_cluster, fat32_dirent + dirent::start
        set32 cur_context + context::cluster, fat32_dirent + dirent::start
        jsr open_cluster
        bcc @error

        ; Set context as in-use
        lda #FLAG_IN_USE
        sta cur_context + context::flags

        ; Success
        sec
        rts

@error: clc
        rts

;-----------------------------------------------------------------------------
; find_file
;
; Same as find_dirent, but with file type check
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
find_file:
        lda #$80 ; files only
        jmp find_dirent

;-----------------------------------------------------------------------------
; find_dir
;
; Same as find_dirent, but with directory type check
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
find_dir:
        lda #$40 ; directories only
        jmp find_dirent


;-----------------------------------------------------------------------------
; find_dirent
;
; Find directory entry with path specified in string pointed to by fat32_ptr
;
; In:  a  =$00 allow files and directories
;         =$80 only allow files
;         =$40 only allow directories
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
find_dirent:
        sta tmp_filetype
        stz name_offset

        ; If path starts with a slash, use root directory as base,
        ; otherwise the current directory.
        lda (fat32_ptr)
        cmp #'/'
        bne @use_current
        set32_val cur_context + context::cluster, 0
        inc name_offset

        ; Does path only consists of a slash?
        ldy name_offset
        lda (fat32_ptr), y
        bne @open

        ; Fake a directory entry for the root directory
        lda #'/'
        sta fat32_dirent + dirent::name
        stz fat32_dirent + dirent::name + 1
        lda #$10
        sta fat32_dirent + dirent::attributes
        .assert dirent::start < dirent::size, error ; must be next to each other
        ldx #0
@clr:   stz fat32_dirent + dirent::start, x
        inx
        cpx #8
        bne @clr

        sec
        rts

@use_current:
        set32 cur_context + context::cluster, cur_volume + fs::cwd_cluster

@open:  set32 tmp_dir_cluster, cur_context + context::cluster

        jsr open_cluster
        bcc @error

@next:  ; Read entry
        jsr fat32_read_dirent
        bcc @error

        ldy name_offset
        jsr match_name
        bcc @next

        ; Check for '/'
        lda (fat32_ptr), y
        cmp #'/'
        beq @chdir

        lda fat32_dirent + dirent::attributes
        bit #$10
        bne @is_dir
        ; is file
        bit tmp_filetype
        bvs @next
        bra @ok
@is_dir:
        bit tmp_filetype
        bmi @next
@ok:    jsr match_type
        bcc @next

@found: ; Found
        sec
        rts

@error: clc
        rts

@chdir: iny
        lda (fat32_ptr), y
        beq @found

        ; Is this a directory?
        lda fat32_dirent + dirent::attributes
        bit #$10
        beq @error

        sty name_offset

        set32 cur_context + context::cluster, fat32_dirent + dirent::start
        set32 tmp_dir_cluster, fat32_dirent + dirent::start
        jmp @open

;-----------------------------------------------------------------------------
; fat32_read_dirent
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
fat32_read_dirent:
        stz fat32_errno

        sec
        jmp read_dirent

;-----------------------------------------------------------------------------
; open_cluster
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
open_cluster:
        ; Check if cluster == 0 -> modify into root dir
        lda cur_context + context::cluster + 0
        ora cur_context + context::cluster + 1
        ora cur_context + context::cluster + 2
        ora cur_context + context::cluster + 3
        bne readsector

open_rootdir:
        set32 cur_context + context::cluster, cur_volume + fs::rootdir_cluster

readsector:
        ; Read first sector of cluster
        jsr calc_cluster_lba
        jsr load_sector_buffer
        bcc @done

        ; Reset buffer pointer
        set16_val fat32_bufptr, sector_buffer

        sec
@done:  rts

;-----------------------------------------------------------------------------
; calc_cluster_lba
;-----------------------------------------------------------------------------
calc_cluster_lba:
        ; lba = lba_data + ((cluster - 2) << cluster_shift)
        sub32_val cur_context + context::lba, cur_context + context::cluster, 2
        ldy cur_volume + fs::cluster_shift
        beq @shift_done
@1:     shl32 cur_context + context::lba
        dey
        bne @1
@shift_done:

        add32 cur_context + context::lba, cur_context + context::lba, cur_volume + fs::lba_data
        stz cur_context + context::cluster_sector
        rts

;-----------------------------------------------------------------------------
; read_dirent
;
; In:   c=1: return next file entry
;       c=0: return next volume label entry
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
read_dirent:
        ror
        sta tmp_dirent_flag
        stz lfn_index
        stz lfn_count

@fat32_read_dirent_loop:
        ; Load next sector if at end of buffer
        cmp16_val_ne fat32_bufptr, sector_buffer_end, @1
        lda #0
        jsr next_sector
        bcs @1
@error: clc     ; Indicate error
        rts
@1:
        ; Last entry?
        lda (fat32_bufptr)
        beq @error

        ; Skip empty entries
        cmp #$E5
        bne @3
        jmp @next_entry_clear_lfn_buffer
@3:

        ; Volume label entry?
        ldy #11
        lda (fat32_bufptr), y
        sta fat32_dirent + dirent::attributes
        cmp #8
        bne @2
        bit tmp_dirent_flag
        bpl @2b
@2a:
        jmp @next_entry
@2:
        bit tmp_dirent_flag
        bpl @2a
@2b:
        ; check for LFN entry
        lda fat32_dirent + dirent::attributes
        cmp #$0f
        beq @lfn_entry
        bra @short_entry

@lfn_entry:

        ; does it have the right index?
        jsr check_lfn_index
        bcs @index_ok
        jmp @next_entry_clear_lfn_buffer
@index_ok:

        ; first LFN entry?
        lda lfn_index
        bne @not_first_lfn_entry

; first LFN entry
        ; init buffer
        set16_val fat32_lfn_bufptr, lfn_buf

        ; save checksum
        ldy #13
        lda (fat32_bufptr), y
        sta lfn_checksum

        ; prepare expected index
        lda (fat32_bufptr)
        and #$1f
        sta lfn_index
        sta lfn_count

        ; add entry to buffer
        jsr add_lfn_entry

        ; remember dir entry
        jsr mark_dir_entry

        ; continue with next entry
        jmp @next_entry
; followup LFN entry
@not_first_lfn_entry:

        ; compare checksum
        ldy #13
        lda (fat32_bufptr), y
        cmp lfn_checksum
        beq @checksum_ok
        jmp @next_entry_clear_lfn_buffer

@checksum_ok:
        dec lfn_index

        ; add entry to buffer
        jsr add_lfn_entry

        ; continue with next entry
        jmp @next_entry


;*******
@short_entry:
        ; is there a LFN?
        lda lfn_index
        cmp #1
        bne @is_short

        ; Compare checksum
        lda #0
        tay
@checksum_loop:
        tax
        lsr
        txa
        ror
        clc
        adc (fat32_bufptr), y
        iny
        cpy #11
        bne @checksum_loop

        cmp lfn_checksum
        bne @is_short

        lda lfn_count
        sta lfn_index

        ldx #0
@decode_lfn_loop:
        sub16_val fat32_lfn_bufptr, fat32_lfn_bufptr, 32

        ldy #1
        lda #5
        jsr decode_lfn_chars
        bcc @name_done2
        ldy #14
        lda #6
        jsr decode_lfn_chars
        bcc @name_done2
        ldy #28
        lda #2
        jsr decode_lfn_chars
        dec lfn_index
        bne @decode_lfn_loop
@name_done2:
        bra @name_done ; yes, we need to zero terminate!

@is_short:
        ; Volume label decoding
        bit tmp_dirent_flag
        bmi @4b
        jsr decode_volume_label
        bra @name_done_z

@4b:    ; get upper/lower case flags
        ldy #12
        lda (fat32_bufptr), y
        asl
        asl
        asl ; bits 7 and 6
        sta tmp_sfn_case

        ; Copy first part of file name
        ldy #0
@4:     lda (fat32_bufptr), y
        cmp #' '
        beq @skip_spaces
        cmp #$05 ; $05 at first character translates into $E5
        bne @n05
        cpy #0
        bne @n05
        lda #$E5
@n05:   bit tmp_sfn_case
        bvc @ucase1
        jsr to_lower
@ucase1:
        jsr filename_cp437_to_internal
        sta fat32_dirent + dirent::name, y
        iny
        cpy #8
        bne @4

        ; Skip any following spaces
@skip_spaces:
        tya
        tax
@5:     cpy #8
        beq @6
        lda (fat32_bufptr), y
        iny
        cmp #' '
        beq @5
@6:
        ; If extension starts with a space, we're done
        lda (fat32_bufptr), y
        cmp #' '
        beq @name_done

        ; Add dot to output
        lda #'.'
        sta fat32_dirent + dirent::name, x
        inx

        ; Copy extension part of file name
@7:     lda (fat32_bufptr), y
        cmp #' '
        beq @name_done
        bit tmp_sfn_case
        bpl @ucase2
        jsr to_lower
@ucase2:
        phx
        jsr filename_cp437_to_internal
        plx
        sta fat32_dirent + dirent::name, x
        iny
        inx
        cpy #11
        bne @7

@name_done:
        ; Add zero-termination to output
        stz fat32_dirent + dirent::name, x

@name_done_z:
        ; Decode mtime timestamp
        ldy #$16
        lda (fat32_bufptr), y
        iny
        ora (fat32_bufptr), y
        iny
        ora (fat32_bufptr), y
        iny
        ora (fat32_bufptr), y
        bne @ts1
        stz fat32_dirent + dirent::mtime_seconds
        stz fat32_dirent + dirent::mtime_minutes
        stz fat32_dirent + dirent::mtime_hours
        stz fat32_dirent + dirent::mtime_day
        lda #$ff ; year 2235 signals "no date"
        bra @ts2
@ts1:   ldy #$16
        lda (fat32_bufptr), y
        sta tmp_timestamp
        and #31
        asl
        sta fat32_dirent + dirent::mtime_seconds
        iny
        lda (fat32_bufptr), y
        asl tmp_timestamp
        rol
        asl tmp_timestamp
        rol
        asl tmp_timestamp
        rol
        and #63
        sta fat32_dirent + dirent::mtime_minutes
        lda (fat32_bufptr), y
        lsr
        lsr
        lsr
        sta fat32_dirent + dirent::mtime_hours
        iny
        lda (fat32_bufptr), y
        tax
        and #31
        sta fat32_dirent + dirent::mtime_day
        iny
        lda (fat32_bufptr), y
        sta tmp_timestamp
        txa
        lsr tmp_timestamp
        ror
        lsr
        lsr
        lsr
        lsr
        sta fat32_dirent + dirent::mtime_month
        lda (fat32_bufptr), y
        lsr
@ts2:   sta fat32_dirent + dirent::mtime_year

        ; Copy file size
        ldy #28
        ldx #0
@8:     lda (fat32_bufptr), y
        sta fat32_dirent + dirent::size, x
        iny
        inx
        cpx #4
        bne @8

        ; Copy cluster
        ldy #26
        lda (fat32_bufptr), y
        sta fat32_dirent + dirent::start + 0
        iny
        lda (fat32_bufptr), y
        sta fat32_dirent + dirent::start + 1
        ldy #20
        lda (fat32_bufptr), y
        sta fat32_dirent + dirent::start + 2
        iny
        lda (fat32_bufptr), y
        sta fat32_dirent + dirent::start + 3

        ; Save lba + fat32_bufptr
        set32 cur_context + context::dirent_lba,    cur_context + context::lba
        set16 cur_context + context::dirent_bufptr, fat32_bufptr

        ; Increment buffer pointer to next entry
        add16_val fat32_bufptr, fat32_bufptr, 32

        sec
        rts

@next_entry_clear_lfn_buffer:
        stz lfn_index

@next_entry:
        add16_val fat32_bufptr, fat32_bufptr, 32
        jmp @fat32_read_dirent_loop

;-----------------------------------------------------------------------------
; add_lfn_entry
;-----------------------------------------------------------------------------
add_lfn_entry:
        ldy #31
:       lda (fat32_bufptr), y
        sta (fat32_lfn_bufptr), y
        dey
        bpl :-
        add16_val fat32_lfn_bufptr, fat32_lfn_bufptr, 32
        rts

;-----------------------------------------------------------------------------
; check_lfn_index
;
; * c=1: ok
;-----------------------------------------------------------------------------
check_lfn_index:
        lda lfn_index
        beq @expect_start

        lda lfn_index
        dec
        cmp (fat32_bufptr)
        beq @ok

        stz lfn_index
@expect_start:
        lda (fat32_bufptr)
        asl
        asl ; bit #6 -> C
        rts

@ok:
        sec
        rts

;-----------------------------------------------------------------------------
; decode_lfn_chars
;
; Convert 16 bit UCS-2-encoded LFN characters to private 8 bit encoding.
;
; In:   a  number of characters
;       x  target index (offset in fat32_dirent + dirent::name)
;       y  source index (offset in (fat32_lfn_bufptr))
; Out:  x  updated target index
;       y  updated source index
;       c  =0: terminating 0 character encountered
;-----------------------------------------------------------------------------
decode_lfn_chars:
        stx lfn_name_index
        sta lfn_char_count
@loop:
        lda (fat32_lfn_bufptr), y
        iny
        pha
        pha
        lda (fat32_lfn_bufptr), y
        iny
        plx
        pha
        jsr filename_char_ucs2_to_internal
        ldx lfn_name_index
        sta fat32_dirent + dirent::name, x
        inc lfn_name_index
        pla
        plx
        bne @cont
        tax
        beq @end
@cont:
        dec lfn_char_count
        bne @loop
        ldx lfn_name_index
        sec
        rts
@end:   ldx lfn_name_index
        clc
        rts

;-----------------------------------------------------------------------------
; mark_dir_entry
;
; Save current cluster, LBA and directory entry index.
;-----------------------------------------------------------------------------
mark_dir_entry:
        ; save cluster
        lda cur_context + context::cluster + 0
        sta marked_entry_cluster + 0
        lda cur_context + context::cluster + 1
        sta marked_entry_cluster + 1
        lda cur_context + context::cluster + 2
        sta marked_entry_cluster + 2
        lda cur_context + context::cluster + 3
        sta marked_entry_cluster + 3
        ; save LBA
        lda cur_context + context::lba + 0
        sta marked_entry_lba + 0
        lda cur_context + context::lba + 1
        sta marked_entry_lba + 1
        lda cur_context + context::lba + 2
        sta marked_entry_lba + 2
        lda cur_context + context::lba + 3
        sta marked_entry_lba + 3
        ; save offset
        lda fat32_bufptr + 0
        sta marked_entry_offset + 0
        lda fat32_bufptr + 1
        sta marked_entry_offset + 1
        rts

;-----------------------------------------------------------------------------
; next_sector
; A: bit0 - allocate cluster if at end of cluster chain
;    bit1 - clear allocated cluster
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
next_sector:
        ; Save argument
        sta next_sector_arg

        ; Last sector of cluster?
        lda cur_context + context::cluster_sector
        inc
        cmp cur_volume + fs::sectors_per_cluster
        beq @end_of_cluster
        sta cur_context + context::cluster_sector

        ; Load next sector
        inc32 cur_context + context::lba
@read_sector:
        jsr load_sector_buffer
        bcc @error
        set16_val fat32_bufptr, sector_buffer
        sec
        rts

@end_of_cluster:
        jsr next_cluster
        bcc @error
        jsr is_end_of_cluster_chain
        bcs @end_of_chain
@read_cluster:
        jsr calc_cluster_lba
        bra @read_sector

@end_of_chain:
        ; Request to allocate new cluster?
        lda next_sector_arg
        bit #$01
        beq @error

        ; Save location of cluster entry in FAT
        set16 tmp_bufptr, fat32_bufptr
        set32 tmp_sector_lba, sector_lba

        ; Allocate a new cluster
        jsr allocate_cluster
        bcc @error

        ; Load back the cluster sector
        set32 cur_context + context::lba, tmp_sector_lba
        jsr load_sector_buffer
        bcs @1
@error: clc
        rts
@1:
        set16 fat32_bufptr, tmp_bufptr

        ; Write allocated cluster number in FAT
        ldy #0
@2:     lda cur_volume + fs::free_cluster, y
        sta (fat32_bufptr), y
        iny
        cpy #4
        bne @2

        ; Save FAT sector
        jsr save_sector_buffer
        bcc @error

        ; Set allocated cluster as current
        set32 cur_context + context::cluster, cur_volume + fs::free_cluster

        ; Request to clear new cluster?
        lda next_sector_arg
        bit #$02
        beq @wrdone
        jsr clear_cluster
        bcc @error

@wrdone:
        ; Retry
        jmp @read_cluster

;-----------------------------------------------------------------------------
; allocate_cluster
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
allocate_cluster:
        ; Find free entry
        jsr find_free_cluster
        bcs @1
        rts
@1:
        ; Set cluster as end-of-chain
        ldy #0
        lda #$FF
        sta (fat32_bufptr), y
        iny
        sta (fat32_bufptr), y
        iny
        sta (fat32_bufptr), y
        iny
        lda (fat32_bufptr), y
        ora #$0F        ; Preserve upper 4 bits
        sta (fat32_bufptr), y

        ; Save FAT sector
        jsr save_sector_buffer
        bcs @2
        rts
@2:
        ; Decrement free clusters and update FS info
        dec32 cur_volume + fs::free_clusters
        jmp update_fs_info

;-----------------------------------------------------------------------------
; clear_cluster
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
clear_cluster:
        ; Fill sector buffer with 0
        jsr clear_buffer

        ; Write sectors
        jsr calc_cluster_lba
@2:     set32 sector_lba, cur_context + context::lba
        jsr write_sector
        bcs @3
        rts
@3:     lda cur_context + context::cluster_sector
        inc
        cmp cur_volume + fs::sectors_per_cluster
        beq @wrdone
        sta cur_context + context::cluster_sector
        inc32 cur_context + context::lba
        bra @2

@wrdone:
        sec
        rts

;-----------------------------------------------------------------------------
; clear_buffer
;-----------------------------------------------------------------------------
clear_buffer:
        ldy #0
        tya
@1:     sta sector_buffer, y
        sta sector_buffer + 256, y
        iny
        bne @1
        rts

;-----------------------------------------------------------------------------
; find_free_cluster
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
find_free_cluster:
        ; Start search at free_cluster
        set32 cur_context + context::cluster, cur_volume + fs::free_cluster
        jsr load_fat_sector_for_cluster
        bcs @next
        rts

@next:  ; Check for free entry
        ldy #3
        lda (fat32_bufptr), y
        and #$0F        ; Ignore upper 4 bits of 32-bit entry
        dey
        ora (fat32_bufptr), y
        dey
        ora (fat32_bufptr), y
        dey
        ora (fat32_bufptr), y
        bne @not_free

        ; Return found free cluster
        set32 cur_volume + fs::free_cluster, cur_context + context::cluster
        sec
        rts

@not_free:
        ; fat32_bufptr += 4
        add16_val fat32_bufptr, fat32_bufptr, 4

        ; cluster += 1
        inc32 cur_context + context::cluster

        ; Check if at end of FAT table
        cmp32_ne cur_context + context::cluster, cur_volume + fs::cluster_count, @1
        clc
        rts
@1:
        ; Load next FAT sector if at end of buffer
        cmp16_val_ne fat32_bufptr, sector_buffer_end, @next
        inc32 cur_context + context::lba
        jsr load_sector_buffer
        bcs @2
        rts
@2:     set16_val fat32_bufptr, sector_buffer
        jmp @next

;-----------------------------------------------------------------------------
; is_end_of_cluster_chain
;-----------------------------------------------------------------------------
is_end_of_cluster_chain:
        ; Check if this is the end of cluster chain (entry >= 0x0FFFFFF8)
        lda cur_context + context::cluster + 3
        and #$0F        ; Ignore upper 4 bits
        cmp #$0F
        bne @no
        lda cur_context + context::cluster + 2
        cmp #$FF
        bne @no
        lda cur_context + context::cluster + 1
        cmp #$FF
        bne @no
        lda cur_context + context::cluster + 0
        cmp #$F8
        bcs @yes
@no:    clc
@yes:   rts

;-----------------------------------------------------------------------------
; load_fat_sector_for_cluster
;
; Load sector that hold cluster entry for cur_context.cluster
; On return fat32_bufptr points to cluster entry in sector_buffer.
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
load_fat_sector_for_cluster:
        ; Calculate sector where cluster entry is located

        ; lba = lba_fat + (cluster / 128)
        lda cur_context + context::cluster + 1
        sta cur_context + context::lba + 0
        lda cur_context + context::cluster + 2
        sta cur_context + context::lba + 1
        lda cur_context + context::cluster + 3
        sta cur_context + context::lba + 2
        stz cur_context + context::lba + 3
        lda cur_context + context::cluster + 0
        asl     ; upper bit in C
        rol cur_context + context::lba + 0
        rol cur_context + context::lba + 1
        rol cur_context + context::lba + 2
        rol cur_context + context::lba + 3
        add32 cur_context + context::lba, cur_context + context::lba, cur_volume + fs::lba_fat

        ; Read FAT sector
        jsr load_sector_buffer
        bcs @1
        rts     ; Failure
@1:
        ; fat32_bufptr = sector_buffer + (cluster & 127) * 4
        lda cur_context + context::cluster
        asl
        asl
        sta fat32_bufptr + 0
        lda #0
        bcc @2
        lda #1
@2:     sta fat32_bufptr + 1
        add16_val fat32_bufptr, fat32_bufptr, sector_buffer

        ; Success
        sec
        rts


;-----------------------------------------------------------------------------
; next_cluster
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
next_cluster:
        ; End of cluster chain?
        jsr is_end_of_cluster_chain
        bcs @error

        ; Load correct FAT sector
        jsr load_fat_sector_for_cluster
        bcc @error

        ; Copy next cluster from FAT
        ldy #0
@1:     lda (fat32_bufptr), y
        sta cur_context + context::cluster, y
        iny
        cpy #4
        bne @1

        sec
        rts

@error: clc
        rts

;-----------------------------------------------------------------------------
; save_sector_buffer
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
save_sector_buffer:
        ; Determine if this is FAT area write (sector_lba - lba_fat < fat_size)
        sub32 tmp_buf, sector_lba, cur_volume + fs::lba_fat
        lda tmp_buf + 2
        ora tmp_buf + 3
        bne @normal
        sec
        lda tmp_buf + 0
        sbc cur_volume + fs::fat_size + 0
        lda tmp_buf + 1
        sbc cur_volume + fs::fat_size + 1
        bcs @normal

        ; Write second FAT
        set32 tmp_buf, sector_lba
        add32 sector_lba, sector_lba, cur_volume + fs::fat_size
        jsr write_sector
        php
        set32 sector_lba, tmp_buf
        plp
        bcc @error_write

@normal:
        jsr write_sector
        bcc @error_write

        ; Clear dirty bit
        lda cur_context + context::flags
        and #(FLAG_DIRTY ^ $FF)
        sta cur_context + context::flags

        sec
        rts

@error_write:
        lda #ERRNO_WRITE
        jmp set_errno


;-----------------------------------------------------------------------------
; update_fs_info
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
update_fs_info:
        ; Load FS info sector
        set32 cur_context + context::lba, cur_volume + fs::lba_fsinfo
        jsr load_sector_buffer
        bcs @1
        rts
@1:
        ; Get number of free clusters
        set32 sector_buffer + 488, cur_volume + fs::free_clusters

        ; Save sector
        jmp save_sector_buffer

;-----------------------------------------------------------------------------
; write_sector
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
write_sector:
        lda fat32_readonly
        bne @error
        jmp sdcard_write_sector

@error: lda #ERRNO_WRITE_PROTECT_ON
        jmp set_errno

;-----------------------------------------------------------------------------
; fat32_read_byte
;
; Out:  a      byte
;       x      =$ff: EOF after this byte
;       c      =0: success
;              =1: failure (includes reading past EOF)
;       errno  =0: no error, or reading past EOF
;              =ERRNO_READ: read error
;-----------------------------------------------------------------------------
fat32_read_byte:
        stz fat32_errno

        ; Bytes remaining?
        bit cur_context + context::eof
        bmi @error

        ; At end of buffer?
        cmp16_val_ne fat32_bufptr, sector_buffer_end, @2
        lda #0
        jsr next_sector
        bcc @error
@2:
        ; Increment offset within file
        inc32 cur_context + context::file_offset

        ldx #0   ; no EOF
        cmp32_ne cur_context + context::file_offset, cur_context + context::file_size, @3
        ldx #$ff ; EOF
        stx cur_context + context::eof
@3:
        ; Get byte from buffer
        lda (fat32_bufptr)
        inc16 fat32_bufptr

        sec     ; Indicate success
        rts

@error: clc
        rts

;-----------------------------------------------------------------------------
; fat32_close
;
; Close current file
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
fat32_close:
        stz fat32_errno

        lda cur_context + context::flags
        bne :+
        jmp @done
:
        ; Write current sector if dirty
        jsr sync_sector_buffer
        bcs :+
        jmp error_clear_context
:
        ; Update directory entry with new size and mdate if needed
        lda cur_context + context::flags
        bit #FLAG_DIRENT
        bne :+
        jmp @done
:       and #(FLAG_DIRENT ^ $FF)        ; Clear bit
        sta cur_context + context::flags

        ; Load sector of directory entry
        set32 cur_context + context::lba, cur_context + context::dirent_lba
        jsr load_sector_buffer
        bcs :+
        jmp error_clear_context
:
        ; Write size to directory entry
        set16 fat32_bufptr, cur_context + context::dirent_bufptr
        ldy #28
        lda cur_context + context::file_size + 0
        sta (fat32_bufptr), y
        iny
        lda cur_context + context::file_size + 1
        sta (fat32_bufptr), y
        iny
        lda cur_context + context::file_size + 2
        sta (fat32_bufptr), y
        iny
        lda cur_context + context::file_size + 3
        sta (fat32_bufptr), y

        ; Encode mtime timestamp
@ts1:   lda fat32_time_year
        inc
        bne @ts3
        ; no time set up
        lda #0
        ldy #$16
        sta (fat32_bufptr), y
        iny
        sta (fat32_bufptr), y
        iny
        sta (fat32_bufptr), y
        iny
        sta (fat32_bufptr), y
        bra @ts2

@ts3:   ldy #$16
        lda fat32_time_minutes
        tax
        asl
        asl
        asl
        asl
        asl
        sta (fat32_bufptr), y
        lda fat32_time_seconds
        lsr
        ora (fat32_bufptr), y
        sta (fat32_bufptr), y
        iny
        txa
        lsr
        lsr
        lsr
        sta (fat32_bufptr), y
        lda fat32_time_hours
        asl
        asl
        asl
        ora (fat32_bufptr), y
        sta (fat32_bufptr), y
        iny
        lda fat32_time_month
        tax
        asl
        asl
        asl
        asl
        asl
        ora fat32_time_day
        sta (fat32_bufptr), y
        iny
        txa
        lsr
        lsr
        lsr
        sta (fat32_bufptr), y
        lda fat32_time_year
        asl
        ora (fat32_bufptr), y
        sta (fat32_bufptr), y
@ts2:

        ; Fill creation date if empty
        ldy #$0e
        lda (fat32_bufptr), y
        iny
        ora (fat32_bufptr), y
        iny
        ora (fat32_bufptr), y
        iny
        ora (fat32_bufptr), y
        bne @ts4
        ldy #$16
        lda (fat32_bufptr), y
        ldy #$0e
        sta (fat32_bufptr), y
        ldy #$17
        lda (fat32_bufptr), y
        ldy #$0f
        sta (fat32_bufptr), y
        ldy #$18
        lda (fat32_bufptr), y
        ldy #$10
        sta (fat32_bufptr), y
        ldy #$19
        lda (fat32_bufptr), y
        ldy #$11
        sta (fat32_bufptr), y
@ts4:

        ; Write directory sector
        jsr save_sector_buffer
        bcc error_clear_context
@done:
        clear_bytes cur_context, .sizeof(context)

        sec
        rts

;-----------------------------------------------------------------------------
; error_clear_context
;
; Call this instead of fat32_close if there has been an error to avoid cached
; writes and possible further inconsistencies.
;-----------------------------------------------------------------------------
error_clear_context:
        clear_bytes cur_context, .sizeof(context)
        clc
        rts


;-----------------------------------------------------------------------------
; sync_sector_buffer
;
; * c=0: failure; sets errno
;-----------------------------------------------------------------------------
sync_sector_buffer:
        ; Write back sector buffer if dirty
        lda cur_context + context::flags
        bit #FLAG_DIRTY
        beq @done
        jmp save_sector_buffer

@done:  sec
        rts

