; Volume Boot Loader
; Location of kernel.bin hardcode
; loads itself into memory


to_memory = $600
.include "bios.inc"

.segment "PBR"
MAIN
  LDX #<MESSAGE_BEGIN
  LDY #>MESSAGE_BEGIN
  JSR print_string

  LDA #<BEFORE
  STA from
  LDA #>BEFORE
  STA from+1
  LDA #<to_memory
  STA to
  LDA #>to_memory
  STA to+1

  LDA #<(END_COPY-BEGIN)
  STA sizel
  LDA #>(END_COPY-BEGIN)
  STA sizeh

  jsr memory_move_down

  LDA #<sector_buffer
  sta from
  lda #>sector_buffer
  sta from+1
  lda #$00
  sta to
  lda #$10
  sta to+1

  stz sizel
  lda #$02
  sta sizeh



  ldx #$00
:
  lda KERNEL,x
  sta sector_lba,x
  inx
  cpx #$04
  bne :-

  jmp to_memory

loop:
  jmp loop


KERNEL
  .byte $01,$02,$03,$04
END
MESSAGE_BEGIN
  .byte "Starting Volume Boot Record",$0A,$0D,$00

BEFORE:
.segment "PBR_RAM"
BEGIN:
  ldy #$00
@loop:
  phx
  phy
  jsr sdcard_read_sector
  bcc error

  LDA #<sector_buffer
  sta from
  lda #>sector_buffer
  sta from+1

  jsr memory_move_down
  ply
  plx
  jsr sd_next_sector

  clc
  lda #$02
  adc to+1

  iny
  cpy #$10
  bne @loop

  jmp $1000
  brk


sd_next_sector:
  phy
  clc
  ldy #$00
  lda sector_lba,y
  adc #$01
  sta sector_lba,y
  bcc @done
@loop:
  iny
  lda sector_lba,y
  adc #$00
  sta sector_lba,y
  cpy #$03
  bne @loop
@done:
  ply
  rts

error:
  lda #'x'
  jsr print_char
  brk


END_COPY:

.segment "PBR_TAIL"
  .byte $55,$AA
