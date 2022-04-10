.include "io.inc"
.export via_init

.segment "CODE"

sd_cs   = %00010000
sd_sck  = %00001000
sd_mosi = %00000100
sd_miso = %00000010

via_init:
  portb_outputpins = sd_cs | sd_sck | sd_mosi
  lda #portb_outputpins   ; set various pins on port b to output
  sta ddrb
  rts

