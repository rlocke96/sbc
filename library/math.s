; Library functions
; Math

; 16 Bit Addition
;   Space Required: 64 Bits
;   Number 1:
;   Number 2:

.export Addition, DIVIDE, add, add_32, sub, sub_32, Multiplying, division, BINBCD16, _CLEAR_NUM2, _CLEAR_MATH, _RESULT_NUM1

Addition
add
  clc				; clear carry
  lda NUM1
  adc NUM2
  sta RESULT			; store sum of LSBs
  lda NUM1+1
  adc NUM2+1			; add the MSBs using carry from
  sta RESULT+1			; the previous calculation
  rts

add_32
  clc
  lda NUM1
  adc NUM2
  STA RESULT
  lda NUM1+1
  adc NUM2+1
  STA RESULT+1
  lda NUM1+2
  adc NUM2+2
  STA RESULT+2
  lda NUM1+3
  adc NUM2+3
  STA RESULT+3
  RTS


Subtraction
sub
  sec				      ; set carry for borrow purpose
	lda NUM1
	sbc NUM2	  		; perform subtraction on the LSBs
	sta RESULT
	lda NUM1+1			; do the same for the MSBs, with carry
	sbc NUM2+1			; set according to the previous result
	sta RESULT+1
	rts

sub_32
  sec
  LDA NUM1
  SBC NUM2
  STA RESULT
  LDA NUM1+1
  SBC NUM2+1
  STA RESULT+1
  LDA NUM1+2
  SBC NUM2+2
  STA RESULT+2
  LDA NUM1+3
  SBC NUM2+3
  STA RESULT+3
  RTS


  ; 16 Bit Multiplication
  ;   Space Required: 64 Bits
  ;   Number 1:
  ;   Number 2:
;  NUM1  .word $0000
;  NUM2  .word $0000
;  RESULT .word $0000
;  RESULT2 .word $0000

Multiplying
  LDA #0       ;Initialize RESULT to 0
  STA RESULT+2
  LDX #16      ;There are 16 bits in NUM2
@L1
  LSR NUM2+1   ;Get low bit of NUM2
  ROR NUM2
  BCC @L2       ;0 or 1?
  TAY          ;If 1, add NUM1 (hi byte of RESULT is in A)
  CLC
  LDA NUM1
  ADC RESULT+2
  STA RESULT+2
  TYA
  ADC NUM1+1
@L2
  ROR A        ;"Stairstep" shift
  ROR RESULT+2
  ROR RESULT+1
  ROR RESULT
  DEX
  BNE @L1
  STA RESULT+3
  RTS

; 16 Bit Addition
;   Space Required: 64 Bits
;   Number 1:
;   Number 2:
;  NUM1  .word $0000
;  NUM2  .word $0000

division
DIVIDE
  LDA #0      ;Initialize REM to 0
  STA REM
  STA REM+1
  LDX #16     ;There are 16 bits in NUM1
@L1
  ASL NUM1    ;Shift hi bit of NUM1 into REM
  ROL NUM1+1  ;(vacating the lo bit, which will be used for the quotient)
  ROL REM
  ROL REM+1
  LDA REM
  SEC         ;Trial subtraction
  SBC NUM2
  TAY
  LDA REM+1
  SBC NUM2+1
  BCC @L2      ;Did subtraction succeed?
  STA REM+1   ;If yes, save it
  STY REM
  INC NUM1    ;and record a 1 in the quotient
@L2
  DEX
  BNE @L1
  RTS


; Binary to Decimal
BCD = NUM2
BIN = NUM1
BINBCD16
  SED ; Switch to decimal mode
  LDA #0 ; Ensure the result is clear
  STA BCD+0
  STA BCD+1
  STA BCD+2
  LDX #16 ; The number of source bits

CNVBIT
  ASL BIN+0 ; Shift out one bit
  ROL BIN+1
  LDA BCD+0 ; And add into result
  ADC BCD+0
  STA BCD+0
  LDA BCD+1 ; propagating any carry
  ADC BCD+1
  STA BCD+1
  LDA BCD+2 ; ... thru whole result
  ADC BCD+2
  STA BCD+2
  DEX ; And repeat for next bit
  BNE CNVBIT
  CLD ; Back to binary
  RTS


MEM = $92
; Shift a 16 bit value by one place right (e.g. divide by two)
_LSR16
  LSR MEM+1       ;Shift the MSB
  ROR MEM+0       ;Rotate the LSB
  RTS

_CLEAR_MATH
  LDY #$00
  LDA #$00
@loop
  STA NUM1,Y
  STA NUM2,Y
  STA REM,Y
  STA RESULT,Y
  STA RESULT2,Y
  INY
  CPY #$04
  BNE @loop
  RTS

_RESULT_NUM1
  LDY #$00
@loop
  LDA RESULT,Y
  STA NUM1,Y
  INY
  CPY #$04
  BNE @loop
  RTS

_CLEAR_NUM2
  LDA #$00
  LDY #$00
@loop
  STA NUM2,Y
  INY
  CPY #$04
  BNE @loop
  RTS
