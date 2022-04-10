;
; kernel.bin
; by robert locke
; version .05
; start 11/8/2020
; v.04 start 3/20/2022
; v.05 start 4/9/2022
;


.include "bios.inc"

.segment "BEGIN"
main
  ldx #$FF
  txs
  cld

loop:

  jsr rtc_get_hours
  jsr print_hex
  lda #':'
  jsr print_char
  jsr rtc_get_minutes
  jsr print_hex
  lda #':'
  jsr print_char
  jsr rtc_get_seconds
  jsr print_hex

  lda #' '
  jsr print_char

  jsr rtc_get_month
  jsr print_hex
  lda #'/'
  jsr print_char
  jsr rtc_get_date
  jsr print_hex
  lda #'/'
  jsr print_char
  jsr rtc_get_century
  jsr print_hex
  jsr rtc_get_year
  jsr print_hex


  jmp loop
