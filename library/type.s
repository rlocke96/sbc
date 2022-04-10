;
; type.asm
; by Robert Locke
;


TYPE

  JSR CLUSTER_DATA_TO_LDA
  JSR LOAD_LDA_TO_SD_CMD
  JSR LOAD_CLUSTER

  LDA #$00
  STA zp_tmp_holder
  LDA #$10
  STA zp_tmp_holder+1

  LDA #$0
  STA COUNTER_OF_SECTORS
  LDA #$C0
  STA DELAY_TIMER


.sector_loop
  LDX #$00
.loop_x
  LDY #$00
.type_loop
  LDA (zp_tmp_holder),Y
  PHY
  PHX
;  JSR ACIAout
  JSR print_char
  PLX
  PLY
  JSR CURSOR_DEC
  BCC .EXIT
  INY
  BNE .type_loop
  INX
  INC zp_tmp_holder+1
  CPX #$2
  BNE .loop_x
  INC COUNTER_OF_SECTORS
  LDA COUNTER_OF_SECTORS
  CMP PART_SPC
  BNE .sector_loop
.done

  JSR NEXT_CLUSTER
  BCC .EXIT
  JMP TYPE

.EXIT
  RTS

COUNTER_OF_SECTORS
  .byte $00
