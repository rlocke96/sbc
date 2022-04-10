;----------------------------------------------------------------------
; acia driver for 6551
;----------------------------------------------------------------------

.include "io.inc"
.include "acia.inc"
.include "delay.inc"

.segment "CODE"

;----------------------------------------------------------------------
; acia_init
;
; function: initialize the 65c51 acia (asynchronous communication interface adapter)
;
;----------------------------------------------------------------------
acia_init:
  pha
  lda     #$00
  sta     acia_status            ; soft reset
  lda     #$0b
  sta     acia_cmd               ; parity disabled, irq disabled
  lda     #$1f
  sta     acia_ctrl              ; set output for 8-n-1 19200
  lda     #$ff
  sta     delay_timer
  pla
  rts


;----------------------------------------------------------------------
; acia_out
;
; function: byte out to acia
;
; pass:		a register
;----------------------------------------------------------------------
acia_out:
        pha                             ; save accumulator
@loop:
        lda     acia_status              ; read 6551 status
        and     #$10                    ; is tx buffer full?
        beq     @loop                   ; if not, loop back
        pla                             ; otherwise, restore accumulator
        sta     acia_data                ; write byte to 6551
        jsr     acia_delay		; The tx buffer full doesn't work on the 65C51.
					; Delay or interrupt is only way to verify buffer empty
        rts

;----------------------------------------------------------------------
; acia_in
;
; function: byte in from acia.
;
; returns:	a register
;----------------------------------------------------------------------
acia_in:
  lda     acia_status            ; read 6551 status
  and     #$08                    ;
  beq     @exit               ; if rx buffer empty, no byte
  lda     acia_data              ; read byte from 6551
@done:
  sec                             ; flag byte received
  rts
@exit:
  clc                             ; flag no byte received
  rts

;----------------------------------------------------------------------
; acia_delay
;
; function: delay acia output
; 
;----------------------------------------------------------------------
acia_delay:				; Delay based on clock speed and baud rate
  jsr delay_medium
  rts
