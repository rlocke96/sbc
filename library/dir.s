DIRECTORY = $1000

.globalzp zp_dir_index

DIR

  JSR CLUSTER_DATA_TO_LDA
  JSR LOAD_LDA_TO_SD_CMD
  JSR LOAD_CLUSTER

  LDA #<DIRECTORY
  STA zp_dir_index
  LDA #>DIRECTORY
  STA zp_dir_index+1

  ; Loop 16 Times per 512 Sector (32 Bytes Per Record)
  LDX #$00
  LDY #$00
@loop
  LDY #$00
@loop_test
  JSR loop_record
  BCC @EXIT

  ; Add 32 Bytes to dir index
  clc
	lda zp_dir_index
	adc #$20
	sta zp_dir_index
  bcc @ok
  inc zp_dir_index+1
@ok

  INY
  CPY #$10
  BNE @loop
  INX
  CPX PART_SPC
  BNE @loop

  JSR NEXT_CLUSTER
  BCC @EXIT
  JMP DIR

@EXIT
  RTS




loop_record
  PHX
  PHY
  LDY #DIR_Attr
  LDA (zp_dir_index),Y
  BEQ .end_record
  CMP #$08
  BEQ .next_record
  CMP #$0F
  BEQ .next_record
  LDY #$00
  LDA (zp_dir_index),Y
  CMP #$e5
  BEQ .next_record

  JSR LOAD_FILENAME
  JSR LOAD_EXTENSION
  JSR LOAD_SIZE
  JSR LOAD_YEAR
  JSR LOAD_MONTH
  JSR LOAD_DAY
  JSR LOAD_HOUR
  JSR LOAD_MINUTE

.print_record
  LDX #<FILE_RECORD
  LDY #>FILE_RECORD
  JSR print_string

.next_record
  PLY
  PLX
  SEC
  RTS

.end_record
  PLY
  PLX
  CLC
  RTS
