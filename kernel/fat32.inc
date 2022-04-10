;--------------------------------------------------------------------------------------------------
; fat32.inc
;
;--------------------------------------------------------------------------------------------------


;--------------------------------------------------------------------------------------------------
; Functions
;--------------------------------------------------------------------------------------------------

; Global operations
.global fat32_init
.global fat32_alloc_context
.global fat32_free_context

; Partition table operations
.global fat32_get_ptable_entry	; Get partition table entry in A (0+) into fat32_dirent
.global fat32_dirent		; 
.global fat32_read_byte

; File operations
.global fat32_open
.global fat32_close

.global sync_sector_buffer

.global fat32_errno

; Maximum number of open files
FAT32_CONTEXTS = 4

; Maximum number of partitions
FAT32_VOLUMES = 4

;----------------------------------------------------------------------
; errno
;----------------------------------------------------------------------
ERRNO_OK                = 0
ERRNO_READ              = 1
ERRNO_WRITE             = 2
ERRNO_ILLEGAL_FILENAME  = 3
ERRNO_FILE_EXISTS       = 4
ERRNO_FILE_NOT_FOUND    = 5
ERRNO_FILE_READ_ONLY    = 6
ERRNO_DIR_NOT_EMPTY     = 7
ERRNO_NO_MEDIA          = 8
ERRNO_NO_FS             = 9
ERRNO_FS_INCONSISTENT   = 10
ERRNO_WRITE_PROTECT_ON  = 11
ERRNO_OUT_OF_RESOURCES  = 12

