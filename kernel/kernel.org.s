;
; kernel.bin
; by robert locke
; version .03
; start 11/8/2020
;
; functions needed in this version
; dir - directory listing
;   ability to read entire cluster and follow fat table

;.include "address.inc"
.include "bios.inc"
.include "io.inc"
;.include "spi.inc"

.segment "BEGIN"
main
  ldx #$00
@loop:
  lda $400,x
  jsr print_hex
  inx
  bne @loop
;  ldx #$FF
;  txs
;  cld

;  lda #$00
;  sta delay_timer

;  ldx #<message
;  ldy #>message
;  jsr print_string
spi_test:

test_spi_init:
  lda #'I'
  jsr print_char
  lda #' '
  jsr print_char
  jsr _spi_init

  lda #'B'
  jsr print_char
  lda #' '
  jsr print_char
  jsr _spi_begin

;  ldx #<message_spi
;  ldy #>message_spi
;  jsr print_string
;test_spi_begin:
;  jsr _spi_begin
;  ldx #<message_spi
;  ldy #>message_spi
;  jsr print_string
;  jsr _spi_write
;  ldx #<message_spi
;  ldy #>message_spi
;  jsr print_string
;  jsr _spi_end  
  lda #'.'
  jsr print_char

loop:
  jmp loop



;message
;  .byte $0a,$0d,"kernel.bin version .03.06 ",$0a,$0d,$00
;clear_screen
;  .byte $1b,"[2j",$1b,"[h",$00
;message_spi:
;  .byte "SPI Test",$0a,$0d,$00
.include "spi.s"
