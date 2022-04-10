.export print_char, print_nlcr, print_hex, print_nybble, print_string, get_char

;  .include "math.s"

.byte $00,$00,$00
zp_string_address = $44

print_char:
  PHX
  PHY
  jsr ACIAout
  PLY
  PLX
  rts

print_nlcr
  lda #$0d
  jsr print_char
  lda #$0a
  jsr print_char
  rts

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
skipletter
  adc #48
  jsr print_char
  rts

print_string:
  STX zp_string_address
  STY zp_string_address+1
  PHA
  LDY #$00
@loop
  LDA	(zp_string_address),Y		; get byte from sign on message
  BEQ	@exit		; exit loop if done
  JSR	print_char		; output character
  INY				; increment index
  BNE	@loop		; loop, branch always
@exit
  PLA
  rts

print_decimal
  jsr BINBCD16
  lda BCD+2
  jsr print_hex
  lda BCD+1
  jsr print_hex
  lda BCD
  jsr print_hex
  rts

print_clear_screen
  lda #<CLEAR_SCREEN
  sta zp_string_address
  lda #>CLEAR_SCREEN
  sta zp_string_address+1
  jsr print_string
  rts

get_char
  jsr ACIAin
  bcc get_char
  rts

CLEAR_SCREEN
  .byte $1B,"[2J",$1B,"[H",$00

