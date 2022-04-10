RTC = $8060
PORTB = $8030
PORTA = $8031
DDRB = $8032
DDRA = $8033
VIA                 = $8030
VIA_ORB             = VIA
VIA_IRB             = VIA
VIA_ORA             = VIA + 1
VIA_IRA             = VIA + 1
VIA_DDRB            = VIA + 2
VIA_DDRA            = VIA + 3
VIA_T1CL            = VIA + 4
VIA_T1CH            = VIA + 5
VIA_T1LL            = VIA + 6
VIA_T1LH            = VIA + 7
VIA_T2CL            = VIA + 8
VIA_T2CH            = VIA + 9
VIA_SR              = VIA + 10
VIA_ACR             = VIA + 11
VIA_PCR             = VIA + 12
VIA_IFR             = VIA + 13
VIA_IER             = VIA + 14



; 6551
ACIADATA    = $8000
ACIASTATUS  = $8001
ACIACMD     = $8002
ACIACTRL    = $8003



MemoryStart = $200
IRQvec = MemoryStart    ; 2 Bytes
NMIvec = IRQvec + 2     ; 2 Bytes

SD_CMD      = NMIvec + 2     ; 1 Bytes
SD_Address  = SD_CMD + 1 ; 4 Bytes
SD_CRC      = SD_Address + 4 ; 1 Byte
P0_STATE    = SD_CRC + 1   ; 1 Byte
P0_START    = P0_STATE + 1 ; 4 Bytes
P0_SIZE     = P0_START + 4 ; 4 Bytes
P1_STATE    = P0_SIZE  + 4 ; 1 Byte
P1_START    = P1_STATE + 1 ; 4 Bytes
P1_SIZE     = P1_START + 4 ; 4 Bytes
P2_STATE    = P1_SIZE  + 4 ; 1 Byte
P2_START    = P2_STATE + 1 ; 4 Bytes
P2_SIZE     = P2_START + 4 ; 4 Bytes
P3_STATE    = P2_SIZE  + 4 ; 1 Byte
P3_START    = P2_STATE + 1 ; 4 Bytes
P3_SIZE     = P3_START + 4 ; 4 Bytes
PART_BPS    = P3_SIZE  + 4 ; 2 Bytes
PART_SPC    = PART_BPS + 2 ; 1 Bytes
PART_RESSEC = PART_SPC + 1 ; 2 Bytes
PART_NUMFAT = PART_RESSEC + 2 ; 1 BYTES
PART_FATS32 = PART_NUMFAT + 1 ; 4 Bytes
PART_ROOTCLUS = PART_FATS32 + 4 ; 4 Bytes
PART_NAME     = PART_ROOTCLUS + 4 ; 4 Bytes

PART_SELECTED = PART_NAME + 4; 1 Bytes
PART_FAT1     = PART_SELECTED + 1; 4 Byte
PART_DATA     = PART_FAT1 + 4; 4 Bytes

; Cluster Info
DIR_CLUSTER   = PART_DATA + 4; 4 Bytes
DIR_NAME      = DIR_CLUSTER + 4; 11 Bytes

CLUSTER       = DIR_NAME + 11 ; 4 BYTES - Current Cluster

DESTINATION   = CLUSTER + 4; 2 BYTES ; Can this work?

LBA           = DESTINATION + 2; 4 BYTES
FAT_OFFSET    = LBA + 4; 1 BYTE
CURSOR_SIZE      = FAT_OFFSET + 1; 4 BYTES
DELAY_TIMER      = CURSOR_SIZE + 4; 1 BYTE


;BPB_BytsPerSec	= $0B ; 2  Bytes
;BPB_SecPerClus	= $0D ; 1  Bytes
;BPB_RsvdSecCnt	= $0E ; 2  Bytes
;BPB_NumFATs	    = $10 ; 1  Bytes
;BPB_FATSz32	    = $24 ; 4  Bytes
;BPB_RootClus	  = $2C ; 4  Bytes
;BPB_Name        = $47 ; 11 Bytes

NUM1 = DELAY_TIMER + 1
NUM2 = NUM1 + 8
RESULT = NUM2 + 8
RESULT2 = RESULT + 8
REM = RESULT2 + 8

zp_tmp_holder         = $F0;
zp_current_partition  = $F2;
