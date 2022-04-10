;----------------------------------------------------------------------
; spi.s
; by Robert Locke
; based on https://github.com/gfoot/sdcard6502
; References:
; http://www.dejazzer.com/ee379/lecture_notes/lec12_sd_card.pdf
; https://github.com/hauerdie/6502_spi
;----------------------------------------------------------------------

.include "io.inc"

.export _spi_init
.export _spi_read
.export _spi_write
.export _spi_begin
.export _spi_end
.export _spi_temp

.segment "BSS"
_spi_temp .res 1

.segment "CODE"

cs   	= %00010000
sclk  	= %00001000
mosi 	= %00000100
miso 	= %00000010
spi_cd  = %00000001

;----------------------------------------------------------------------
; spi_init
;
; function: Initialize SPI ports
; 	
;
;----------------------------------------------------------------------
_spi_init:
  lda #cs | mosi  ; set chip select high, set mosi high
  sta spi_port

  lda #cs | sclk | mosi ; set various pins on port a to output
  sta spi_ddr

  rts

;----------------------------------------------------------------------
; spi_end
;
; function: End SPI Transmission by turning CS high
;----------------------------------------------------------------------
_spi_end:
  pha
  lda spi_port
  ora #cs
  sta spi_port
  pla
  rts

;----------------------------------------------------------------------
; spi_begin
;
; function: Begin SPI Transmission by turning CS low
;----------------------------------------------------------------------
_spi_begin:
  lda #cs
  eor #$ff
  and spi_port
  sta spi_port
  rts

;----------------------------------------------------------------------
; spi_read
;
; function: Read SPI mosi
; 
; returns:	A register
;----------------------------------------------------------------------
_spi_read:
  phx
  phy
  lda #$00
  sta _spi_temp
  lda #mosi
  ora spi_port
  sta spi_port
  ldy #8
@loop:
  lda #sclk
  ora spi_port
  sta spi_port 	; Set Clock High

  ldx spi_port	; Load current SPI into X
  lda #sclk
  eor spi_port
  sta spi_port	; Set Clock Low
  clc		; Clear Flag
  txa		; Move X into A
  and #miso ; Get MISO Value 
  beq @skip
  sec		; Set Carry Flag 
@skip:
  rol _spi_temp	; Load Carry Flag into temp
  dey
  bne @loop
  lda _spi_temp
  ply
  plx
  rts


_spi_write:
  sta _spi_temp
  ldy #8
@loop:
  lda #mosi
  asl _spi_temp
  bcc @is_low
  ora spi_port
  sta spi_port
  jmp @is_high
@is_low:
  lda #mosi
  eor #$ff
  and spi_port
  sta spi_port
@is_high:
  lda #sclk
  ora spi_port
  sta spi_port
  lda #sclk
  eor spi_port
  sta spi_port
  dey
  bne @loop
  rts
