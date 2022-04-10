;----------------------------------------------------------------------
; sdcard.s
; by Robert Locke
; based on https://github.com/gfoot/sdcard6502
; References:
; http://www.dejazzer.com/ee379/lecture_notes/lec12_sd_card.pdf
;----------------------------------------------------------------------


.include "io.inc"

.import delay, delay_medium, delay_long
.import print_char

.export sd_readsector, sd_set_destination, sd_set_cmd_address, sd_next_sector, sd_init
.export sd_cmd, sd_address, sd_crc

.segment "ZEROPAGE"
zp_sd_cmd_address .res 2
zp_memory_destination .res 2
zp_sector_count .res 1

.segment "BIOSVAR"

sd_cmd .res 1
sd_address .res 4
sd_crc .res 1

.segment "CODE"

sd_cs   = %00010000
sd_sck  = %00001000
sd_mosi = %00000100
sd_miso = %00000010
sd_cd   = %00000001

porta_outputpins =  sd_cs | sd_sck | sd_mosi

;----------------------------------------------------------------------
; sd_init
;
; function: Initialize SD Card to spi mode
; 	
;
;----------------------------------------------------------------------

sd_init:
  ldx #<message_init
  ldy #>message_init
  jsr print_string

 
  ; let the sd card boot up, by pumping the clock with sd cs disabled

  ; we need to apply around 80 clock pulses with cs and mosi high.
  ; normally mosi doesn't matter when cs is high, but the card is
  ; not yet is spi mode, and in this non-spi state it does care.

  lda #porta_outputpins   ; set various pins on port a to output
  sta ddra

  lda #sd_cs | sd_mosi
  ldx #160               ; toggle the clock 160 times, so 80 low-high transitions

@preinitloop:
  eor #sd_sck
  sta porta
  dex
  bne @preinitloop


@cmd0: ; go_idle_state - resets card to idle state, and spi mode
  lda #$0
  jsr print_sd_cmd

  ; Load address of cmd0_bytes into zeropage address
  ; example: cmd0_bytes = $fedc, zp_sd_cmd_address=$dc, zp_sd_cmd_address+1=$fe
  lda #<cmd0_bytes
  sta zp_sd_cmd_address
  lda #>cmd0_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; expect status response $01 (not initialized)
  cmp #$01
  bne @initfailed

@cmd8: ; send_if_cond - tell the card how we want it to operate (3.3v, etc)
  lda #$08
  jsr print_sd_cmd

  lda #<cmd8_bytes
  sta zp_sd_cmd_address
  lda #>cmd8_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; expect status response $01 (not initialized)
  cmp #$01
  bne @initfailed

  ; read 32-bit return value, but ignore it
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte

@cmd55: ; app_cmd - required prefix for acmd commands
  lda #$55
  jsr print_sd_cmd

  lda #<cmd55_bytes
  sta zp_sd_cmd_address
  lda #>cmd55_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; expect status response $01 (not initialized)
  cmp #$01
  bne @initfailed

@cmd41: ; app_send_op_cond - send operating conditions, initialize card
  lda #$41
  jsr print_sd_cmd

  lda #<cmd41_bytes
  sta zp_sd_cmd_address
  lda #>cmd41_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; status response $00 means initialised
  cmp #$00
  beq @initialized

  ; otherwise expect status response $01 (not initialized)
  cmp #$01
  bne @initfailed

  ; not initialized yet, so wait a while then try again.
  ; this retry is important, to give the card time to initialize.
  jsr delay_long
  jmp @cmd55


@initialized:
  lda #'y'
  jsr print_char
  rts

@initfailed:
  lda #'x'
  jsr print_char
  rts

cmd0_bytes:
  .byte $40, $00, $00, $00, $00, $95
cmd8_bytes:
  .byte $48, $00, $00, $01, $aa, $87
cmd55_bytes:
  .byte $77, $00, $00, $00, $00, $01
cmd41_bytes:
  .byte $69, $40, $00, $00, $00, $01


;----------------------------------------------------------------------
; sd_readbyte
;
; function: enable the sd card and cycles SPI clock 8 times, MOSI high
;	captures MISO in a register
;
; return:	a register
;----------------------------------------------------------------------
sd_readbyte:
  ; enable the card and tick the clock 8 times with mosi high,
  ; capturing bits from miso and returning them

  ldx #8                      ; we'll read 8 bits
@loop:

  lda #sd_mosi                ; enable card (cs low), set mosi (resting state), sck low
  sta porta
  ;jsr delay	; removed from bios .4; not sure if needed -bob locke 2022/03/20

  lda #sd_mosi | sd_sck       ; toggle the clock high
  sta porta
  ;jsr delay	; removed from bios .4; not sure if needed -bob locke 2022/03/20

  lda porta                   ; read next bit
  and #sd_miso

  clc                         ; default to clearing the bottom bit
  beq @bitnotset              ; unless miso was set
  sec                         ; in which case get ready to set the bottom bit
@bitnotset:

  tya                         ; transfer partial result from y
  rol                         ; rotate carry bit into read result
  tay                         ; save partial result back to y

  dex                         ; decrement counter
  bne @loop                   ; loop if we need to read more bits

  rts

;----------------------------------------------------------------------
; sd_writebyte
;
; function: tick the clock 8 times with descending bits on mosi
; 	sd cards are mostly half-duplex so we ignore anything it sends back here
;
; pass:	a register
; clobbers:	x, y registers
;----------------------------------------------------------------------
sd_writebyte:

  ldx #8                      ; send 8 bits

@loop:
  asl                         ; shift next bit into carry
  tay                         ; save remaining bits for later

  lda #0
  bcc @sendbit                ; if carry clear, don't set mosi for this bit
  ora #sd_mosi

@sendbit:
  sta porta                   ; set mosi (or not) first with sck low
  jsr delay
  eor #sd_sck
  sta porta                   ; raise sck keeping mosi the same, to send the bit
  jsr delay
  tya                         ; restore remaining bits to send

  dex
  bne @loop                   ; loop if there are more bits to send

  rts




;----------------------------------------------------------------------
; sd_waitresult
;
; function: wait for the sd card to return other than $ff
;----------------------------------------------------------------------
sd_waitresult:
  jsr sd_readbyte
  cmp #$ff
  beq sd_waitresult
  rts


;----------------------------------------------------------------------
; sd_sendcommand
;
; function: send command to sd card over spi
;
; pass:		a register
;----------------------------------------------------------------------
sd_sendcommand:
  ldx #0
  lda (zp_sd_cmd_address,x)

  lda #sd_mosi           ; pull cs low to begin command
  sta porta

  ldy #0
  lda (zp_sd_cmd_address),y    ; command byte
  jsr sd_writebyte
  ldy #1
  lda (zp_sd_cmd_address),y    ; data 1
  jsr sd_writebyte
  ldy #2
  lda (zp_sd_cmd_address),y    ; data 2
  jsr sd_writebyte
  ldy #3
  lda (zp_sd_cmd_address),y    ; data 3
  jsr sd_writebyte
  ldy #4
  lda (zp_sd_cmd_address),y    ; data 4
  jsr sd_writebyte
  ldy #5
  lda (zp_sd_cmd_address),y    ; crc
  jsr sd_writebyte

  jsr sd_waitresult

  ; debug print the result code
  ;jsr print_hex

  pha

  ; end command
  lda #sd_cs | sd_mosi   ; set cs high again
  sta porta

  pla   ; restore result code
  rts

;----------------------------------------------------------------------
; sd_readsector
;----------------------------------------------------------------------
sd_readsector:

  ; command 17, arg is sector number, crc not checked
  lda #$51           ; cmd17 - read_single_block
  ldy #0
  sta (zp_sd_cmd_address),y
  jsr sd_sendcommand
  cmp #$00
  beq @readsuccess
  jmp @end

@readsuccess:

  ; wait for data
  jsr sd_waitresult
  cmp #$fe
  beq @readgotdata

  jmp @end

@readgotdata:
  ; need to read 512 bytes.
  ldx #$02
@readloop_high:
  ldy #$00
@readloop_low:
  phx
  phy
  jsr sd_readbyte
  ply
  plx
  sta (zp_memory_destination),y
  iny
  bne @readloop_low
  inc zp_memory_destination+1
  dex
  bne @readloop_high

  ; end command
  lda #sd_cs | sd_mosi
  sta porta
@end:
  rts

;----------------------------------------------------------------------
;----------------------------------------------------------------------
sd_writesector:
  rts

;----------------------------------------------------------------------
;----------------------------------------------------------------------
sd_next_sector:
  clc
  ldy #$04
  lda (zp_sd_cmd_address),y
  adc #$01
  sta (zp_sd_cmd_address),y
  bcc @done
@loop:
  dey
  lda (zp_sd_cmd_address),y
  adc #$00
  sta (zp_sd_cmd_address),y
  bne @loop
@done:
  jsr sd_readsector
  rts

;----------------------------------------------------------------------
;----------------------------------------------------------------------
sd_set_destination:
  ; read sectors 0 to 1
  ; set destination address
  stx zp_memory_destination
  sty zp_memory_destination+1
  rts

;----------------------------------------------------------------------
;----------------------------------------------------------------------
sd_set_cmd_address:
  stx zp_sd_cmd_address
  sty zp_sd_cmd_address+1
  rts

;----------------------------------------------------------------------
;----------------------------------------------------------------------
print_sd_cmd:
  lda #'.'
  jsr print_char

  rts

message_init:
  .byte "Initalizing SD Card",00
message_failed:
  .byte "SD Card failed",00
