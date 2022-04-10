;-----------------------------------------------------------------------------
; Kernel variables
;-----------------------------------------------------------------------------
.struct dirent           ; FILE             PARTITION
name          .res 256   ; file name        partition name (if FAT32, else empty)
attributes    .byte      ; file attributes  partition type
start         .dword     ; start cluster    start LBA
size          .dword     ; size in bytes    size in sectors
mtime_year    .byte
mtime_month   .byte
mtime_day     .byte
mtime_hours   .byte
mtime_minutes .byte
mtime_seconds .byte
.endstruct

.importzp fat32_bufptr, fat32_lfn_bufptr, fat32_ptr, fat32_ptr2
