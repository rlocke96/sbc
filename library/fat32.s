;
; FAT32.asm
; By Robert Locke
; Started 11/9/2020
;

FAT_INIT
  LDX #<SD_CMD
  LDY #>SD_CMD
  jsr sd_set_cmd_address

  RTS

CLUSTER_DATA_TO_LDA
  ;
  ;
  ;

  JSR _CLEAR_MATH
  LDY #$00
@loop
  LDA CLUSTER,Y
  STA NUM1,Y
  INY
  CPY #$04
  BNE @loop

  LDA #$02
  STA NUM2

  JSR sub

  JSR _RESULT_NUM1

  LDA PART_SPC
  STA NUM2

  JSR Multiplying

  JSR _RESULT_NUM1

  LDY #$00
@loop_part_data
  LDA PART_DATA,Y
  STA NUM2,Y
  INY
  CPY #$04
  BNE @loop_part_data

  JSR add_32

  jsr RESULT_TO_LBA

  RTS

CLUSTER_FAT_TO_LBA
  JSR _CLEAR_MATH
  LDY #$00
@loop_cluster
  LDA CLUSTER,Y
  STA NUM1,Y
  iny
  CPY #$04
  BNE @loop_cluster

  LDA #$80
  STA NUM2

  JSR DIVIDE

  LDA REM

  STA FAT_OFFSET

  LDY #$00
@loop_fat_address
  LDA PART_FAT1,Y
  STA NUM2,Y
  INY
  CPY #$04
  BNE @loop_fat_address

  JSR add_32

  JSR RESULT_TO_LBA


  RTS

RESULT_TO_LBA
  LDY #$00
@loop2
  LDA RESULT,Y
  STA LBA,Y
  INY
  CPY #$04
  BNE @loop2
  RTS

CLUSTER_DIR_TO_CLUSTER
  LDY #$00
@loop
  LDA DIR_CLUSTER,Y
  STA CLUSTER,Y
  INY
  CPY #$04
  BNE @loop
  RTS

LOAD_LDA_TO_SD_CMD
  LDY #$00
  LDX #$03
@loop
  LDA LBA,Y
  STA SD_Address,X
  DEX
  INY
  CPY #$04
  BNE @loop
  LDA #$01
  STA SD_CRC
  RTS

CURSOR_DEC
  LDA CURSOR_SIZE+0
  BNE .DEC0
  LDA CURSOR_SIZE+1
  BNE .DEC1
  LDA CURSOR_SIZE+2
  BNE .DEC2
  DEC CURSOR_SIZE+3
.DEC2
  DEC CURSOR_SIZE+2
.DEC1
  DEC CURSOR_SIZE+1
.DEC0
  DEC CURSOR_SIZE+0

  LDA CURSOR_SIZE+0
  BNE .NOTEMPTY
  LDA CURSOR_SIZE+1
  BNE .NOTEMPTY
  LDA CURSOR_SIZE+2
  BNE .NOTEMPTY
  LDA CURSOR_SIZE+3
  BNE .NOTEMPTY
  CLC
  RTS
.NOTEMPTY
  SEC
  RTS

LOAD_CLUSTER
  LDA #$00
  STA DELAY_TIMER

  ldx #$00
  ldy #$10
  jsr sd_set_destination


  JSR sd_readsector
;  LDA SD_Address+0
;  JSR print_hex
;  LDA SD_Address+1
;  JSR print_hex
;  LDA SD_Address+2
;  JSR print_hex
;  LDA SD_Address+3
;  JSR print_hex
;  JSR print_nlcr

  LDY #$01
@loop
  PHY
  JSR sd_next_sector
  PLY
  INY
  CPY PART_SPC
  BNE @loop



  RTS

NEXT_CLUSTER
  JSR CLUSTER_FAT_TO_LBA
  JSR LOAD_LDA_TO_SD_CMD
  ldx #$00
  ldy #$10
  jsr sd_set_destination

  LDA #$00
  STA DELAY_TIMER
  JSR sd_readsector


  JSR _CLEAR_MATH
  LDA FAT_OFFSET
  STA NUM1
  LDA #$04
  STA NUM2
  JSR Multiplying
  JSR _RESULT_NUM1
  LDA #$00
  STA NUM2
  STA NUM2+2
  STA NUM2+3
  LDA #$10
  STA NUM2+1
  JSR add_32
  LDA RESULT
  STA zp_tmp_holder
  LDA RESULT+1
  STA zp_tmp_holder+1

  LDA #$00
  STA DELAY_TIMER

  LDY #$00
@loop_fat
  LDA (zp_tmp_holder),Y
  STA CLUSTER,Y
;  PHY
;  JSR print_hex
;  PLY
  INY
  CPY #$04
  BNE @loop_fat

;  JSR print_nlcr

  LDA CLUSTER
  CMP #$FF
  BNE @loop_table
  LDA CLUSTER+1
  CMP #$FF
  BNE @loop_table
  LDA CLUSTER+2
  CMP #$FF
  BNE @loop_table
  LDA CLUSTER+3
  CMP #$0F
  BNE @loop_table
  CLC
  RTS
@loop_table
  SEC
  RTS
