;----------------------------------------------------------------------
; print.s
; by Robert Locke
;
; print commands, make everything nicer to view
;----------------------------------------------------------------------

.include "print.inc"
.include "acia.inc"

.segment "ZEROPAGE"
zp_string_address .res 2

.segment "PRINT"

;----------------------------------------------------------------------
; print_char
;
; function:
;
;----------------------------------------------------------------------
print_char:
  phx
  phy
  jsr acia_out
  ply
  plx
  rts

;----------------------------------------------------------------------
;----------------------------------------------------------------------
print_nlcr:
  pha
  lda #$0d
  jsr print_char
  lda #$0a
  jsr print_char
  pla
  rts

;----------------------------------------------------------------------
;----------------------------------------------------------------------
print_hex:
  pha
  ror
  ror
  ror
  ror
  jsr print_nybble
  pla
print_nybble:
  and #15
  cmp #10
  bmi skipletter
  adc #6
skipletter:
  adc #48
  jsr print_char
  rts

;----------------------------------------------------------------------
;----------------------------------------------------------------------
print_string:
  stx zp_string_address
  sty zp_string_address+1
  pha
  ldy #$00
@loop:
  lda	(zp_string_address),y		; get byte from sign on message
  beq	@exit		; exit loop if done
  jsr	print_char		; output character
  iny				; increment index
  bne	@loop		; loop, branch always
@exit:
  pla
  rts

;----------------------------------------------------------------------
;----------------------------------------------------------------------
print_clear_screen:
  lda #<clear_screen
  sta zp_string_address
  lda #>clear_screen
  sta zp_string_address+1
  jsr print_string
  rts

;----------------------------------------------------------------------
;----------------------------------------------------------------------
get_char:
  jsr acia_in
  bcc get_char
  rts

;----------------------------------------------------------------------
;----------------------------------------------------------------------
clear_screen:
  .byte $1b,"[2j",$1b,"[h",$00
