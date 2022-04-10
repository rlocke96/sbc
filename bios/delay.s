;----------------------------------------------------------------------
; delay function
;----------------------------------------------------------------------

.include "delay.inc"

.segment "BIOSVAR"
delay_timer:
  .res 1


.segment "CODE"


;----------------------------------------------------------------------
; delay
;
; function: delay using loops
;
; input:	delay_timer set's number of loops
; 
;----------------------------------------------------------------------
delay:
  phx
  ldx delay_timer
@loop:
  dex
  bne @loop
  plx
  rts

;----------------------------------------------------------------------
; delay_long
; 
; function: long delay caused by jumping to delay medium 3 times
;	falls through to delay medium
;----------------------------------------------------------------------
delay_long:
  jsr delay_medium
  jsr delay_medium
  jsr delay_medium

;----------------------------------------------------------------------
; delay_medium
;
; function: medium delay caused by jumping to delay 3 times.
;	last is jump so delay's rts will return to calling location
;----------------------------------------------------------------------
delay_medium:
  jsr delay
  jsr delay
  jmp delay
