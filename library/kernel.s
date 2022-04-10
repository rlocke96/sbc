; BIOS COMMANDS
;print_string              = $f13a
;print_nlcr                = $f113
;get_char                  = $f167
;print_hex                 = $f123
;print_char                = $f110
;sd_read_multiple_sectors  = $f2c7
;sd_set_sector_count       = $f2ed
;sd_set_cmd_address        = $f2e8
;sd_set_destination        = $f2e3


;
; Directory File Offsets
;
DIR_Name	     = $00
DIR_Ext        = $08
DIR_Attr	     = $0B
DIR_Date       = $18
DIR_FstClusHi	 = $14
DIR_FstClusLo	 = $1A
DIR_FileSize	 = $1C
DIR_Time       = $16

;
; File Attribute Types
;
FILE_Attr_ReadOnly  = $01
FILE_Attr_Hidden    = $02
FILE_Attr_System    = $04
FILE_Attr_Label     = $08
FILE_Attr_SubDir    = $10
FILE_Attr_Archive   = $20
FILE_Attr_Device    = $40
FILE_Attr_Reserve   = $80

; Zero Pages
zp_dir_index = $F0; HOLDS CURRENT DIRECTORY INDEX

zp_printf_source    = $F2
zp_printf_argument  = $F4

zp_cd_directory = $F6

  ;include "Library\Math.asm"
.include "string.s"

HEX_to_String
  pha
  ror
  ror
  ror
  ror
  and #15
  cmp #10
  bmi @skipletter
  adc #6
@skipletter
  adc #48
  TAX
  pla
  and #15
  cmp #10
  bmi @skipletter2
  adc #6
@skipletter2
  adc #48
  TAY
  rts

SIZE_TO_STRING
  LDA NUM2+3
  jsr HEX_to_String
  STX FILE_SIZE
  STY FILE_SIZE+1
  LDA NUM2+2
  jsr HEX_to_String
  STX FILE_SIZE+2
  STY FILE_SIZE+3
  LDA NUM2+1
  jsr HEX_to_String
  STX FILE_SIZE+4
  STY FILE_SIZE+5
  LDA NUM2+0
  jsr HEX_to_String
  STX FILE_SIZE+6
  STY FILE_SIZE+7
  RTS

LOAD_FILENAME
  ; LOAD FILENAME
  LDY #DIR_Name
  LDX #$00
@loop
  LDA (zp_dir_index),Y
  STA FILE_NAME,X
  INY
  INX
  CPY #DIR_Name+8
  BNE @loop
  RTS

LOAD_EXTENSION
  LDY #DIR_Ext
  LDX #$0
@loop_ext
  LDA (zp_dir_index),Y
  STA FILE_EXT,X
  INY
  INX
  CPY #DIR_Ext+3
  BNE @loop_ext
  RTS

LOAD_SIZE
  LDY #DIR_Attr
  LDA (zp_dir_index),Y
  BIT #FILE_Attr_SubDir
  BNE @subdir
  LDY #DIR_FileSize
  LDA (zp_dir_index),Y
  STA NUM1
  INY
  LDA (zp_dir_index),Y
  STA NUM1+1
  JSR BINBCD16
  JSR SIZE_TO_STRING
  LDY #$00
@loop
  LDA FILE_SIZE,Y
  CMP #$30
  BEQ @space
  RTS
@space
  LDA #$20
  STA FILE_SIZE,Y
  INY
  JMP @loop

  RTS
@subdir
  LDY #$0
@loop_subdir
  LDA DIR_PLACEHOLDER,Y
  STA FILE_SIZE,Y
  INY
  CPY #FILE_NAME - FILE_SIZE - 1
  BNE @loop_subdir
  RTS

LOAD_YEAR
  LDA #$00
  LDY #$04
@loop
  STA NUM1,Y
  STA NUM2,Y
  DEY
  BNE @loop
  JSR FILE_DATE_YEAR
  STA NUM1
  ; Year is stored as years from 1980
  LDA #$BC
  STA NUM2
  LDA #$7
  STA NUM2+1
  JSR Addition
  LDY #$00
@loop2
  LDA RESULT,Y
  STA NUM1,Y
  INY
  CPY #$04
  BNE @loop2
  JSR BINBCD16
  LDA NUM2+1
  JSR HEX_to_String
  STX FILE_YEAR
  STY FILE_YEAR + 1
  LDA NUM2+0
  JSR HEX_to_String
  STX FILE_YEAR + 2
  STY FILE_YEAR + 3
  RTS

LOAD_HOUR
  JSR CLEAR_MATH
  JSR FILE_TIME_HOUR
  STA NUM1
  JSR BINBCD16
  LDA NUM2
  JSR HEX_to_String
  STX FILE_HOUR
  STY FILE_HOUR+1

LOAD_MINUTE
  JSR CLEAR_MATH
  JSR FILE_TIME_MINUTE
  STA NUM1
  JSR BINBCD16
  LDA NUM2
  JSR HEX_to_String
  STX FILE_MINUTE
  STY FILE_MINUTE+1

LOAD_DAY
  JSR CLEAR_MATH
  JSR FILE_DATE_DAY
  STA NUM1
  JSR BINBCD16
  LDA NUM2
  JSR HEX_to_String
  STX FILE_DAY
  STY FILE_DAY+1
  RTS

LOAD_MONTH
  JSR CLEAR_MATH
  JSR FILE_DATE_MONTH
  STA NUM1
  JSR BINBCD16
  LDA NUM2
  JSR HEX_to_String
  STX FILE_MONTH
  STY FILE_MONTH+1
  RTS

CLEAR_MATH
  LDY #$00
  LDA #$00
@loop
  STA NUM1,Y
  STA RESULT,Y
  INY
  CPY #$08
  BNE @loop
  RTS

FILE_TIME_HOUR
  LDY #DIR_Time+1
  LDA (zp_dir_index),Y
  LSR
  LSR
  LSR
  RTS

FILE_DATE_YEAR
  LDY #DIR_Date+1
  LDA (zp_dir_index),Y
  LSR
  RTS

FILE_DATE_MONTH
  LDY #DIR_Date+1
  LDA (zp_dir_index),Y
  LDY #DIR_Date
  LSR A       ;Shift the MSB
  LDA (zp_dir_index),Y
  ROR A       ;Rotate the LSB
  LSR A
  LSR A
  LSR A
  LSR A
  RTS

FILE_TIME_MINUTE
  LDY #DIR_Time+1
  LDA (zp_dir_index),Y
  STA FILE_TIME_MINUTE_TMP
  LDY #DIR_Time
  LDA (zp_dir_index),Y
  ASL
  ROL FILE_TIME_MINUTE_TMP
  ASL
  ROL FILE_TIME_MINUTE_TMP
  ASL
  ROL FILE_TIME_MINUTE_TMP
  LDA FILE_TIME_MINUTE_TMP
  AND #63
  RTS

FILE_TIME_MINUTE_TMP
  .byte $00

FILE_DATE_DAY
  LDY #DIR_Date
  LDA (zp_dir_index),Y
  AND #$1F
  RTS

FILE_RECORD
FILE_MONTH
  .byte "XX","/"
FILE_DAY
  .byte "XX","/"
FILE_YEAR
  .byte "XXXX"," "
FILE_HOUR
  .byte "09",":"
FILE_MINUTE
  .byte "00"
FILE_AMPM
  .byte "   "
FILE_SIZE
  .byte "XXXXXXXXXX"," "
FILE_NAME
  .byte "FILENAME"," "
FILE_EXT
  .byte "EXT",$0A,$0D,$0
DIR_PLACEHOLDER
  .byte "<DIR>     "
