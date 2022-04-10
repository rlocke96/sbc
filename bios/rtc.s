;----------------------------------------------------------------------
; RTC Driver
;----------------------------------------------------------------------

.include "io.inc"
.include "rtc.inc"

.segment "RTC"

;----------------------------------------------------------------------
; rtc_get_day
;
; function: Get the current day (1-7)
; 		current date is user defined and can be any date
;		and is incremented at midnight
;
; return:	a register
;----------------------------------------------------------------------
rtc_get_day:
  lda rtc+$03
  rts

;----------------------------------------------------------------------
; rtc_get_date
;
; function: Get the current date (01-31)
;
; return:	a register
;----------------------------------------------------------------------
rtc_get_date:
  lda rtc+$04
  rts

;----------------------------------------------------------------------
; rtc_get_month
;
; function: Get the current month (01-12)
;
; return:	a register
;----------------------------------------------------------------------
rtc_get_month:
  lda rtc+$05
  rts

;----------------------------------------------------------------------
; rtc_get_year
;
; function: Get the current year (00-99)
;
; return:	a register
;----------------------------------------------------------------------
rtc_get_year:
  lda rtc+$06
  rts

;----------------------------------------------------------------------
; rtc_get_century
;
; function: Get the current century (00-39)
;
; return:	a register
;----------------------------------------------------------------------
rtc_get_century:
  lda rtc+$07
  rts

;----------------------------------------------------------------------
; rtc_get_seconds
;
; function: Get the current seconds (00-59)
;
; return:	a register
;----------------------------------------------------------------------
rtc_get_seconds:
  lda rtc+$00
  rts

;----------------------------------------------------------------------
; rtc_get_minutes
;
; function: Get the current minutes (00-59)
;
; return:	a register
;----------------------------------------------------------------------
rtc_get_minutes:
  lda rtc+$01
  rts

;----------------------------------------------------------------------
; rtc_get_hours
;
; function: Get the current hours (00-23)
;
; return:	a register
;----------------------------------------------------------------------
rtc_get_hours:
  lda rtc+$02
  rts

;----------------------------------------------------------------------
; rtc_set_day
;
; function: Set the day (1-7)
;
; pass:		a register
;----------------------------------------------------------------------
rtc_set_day:
  phx
  ldx #$03
  jsr rtc_set
  plx
  rts

;----------------------------------------------------------------------
; rtc_set_date
;
; function: Set the date (1-31)
;
; pass:		a register
;----------------------------------------------------------------------
rtc_set_date:
  phx
  ldx #$04
  jsr rtc_set
  plx
  rts

;----------------------------------------------------------------------
; rtc_set_month
;
; function: Set the month (1-12)
;
; pass:		a register
;----------------------------------------------------------------------
rtc_set_month:
  phx
  ldx #$05
  jsr rtc_set
  plx
  rts

;----------------------------------------------------------------------
; rtc_set_year
;
; function: Set the year (00-99)
;
; pass:		a register
;----------------------------------------------------------------------
rtc_set_year:
  phx
  ldx #$06
  jsr rtc_set
  plx
  rts

;----------------------------------------------------------------------
; rtc_set_century
;
; function: Set the century (00-39)
;
; pass:		a register
;----------------------------------------------------------------------
rtc_set_century:
  phx
  ldx #$07
  jsr rtc_set
  plx
  rts

;----------------------------------------------------------------------
; rtc_set_seconds
;
; function: Set the seconds (00-59)
;
; pass:		a register
;----------------------------------------------------------------------
rtc_set_seconds:
  phx
  ldx #$00
  jsr rtc_set
  plx
  rts

;----------------------------------------------------------------------
; rtc_set_minutes
;
; function: Set the minutes (00-59)
;
; pass:		a register
;----------------------------------------------------------------------
rtc_set_minutes:
  phx
  ldx #$01
  jsr rtc_set
  plx
  rts

;----------------------------------------------------------------------
; rtc_set_hours
;
; function: Set the hours (00-23)
;
; pass:		a register
;----------------------------------------------------------------------
rtc_set_hours:
  phx
  ldx #$02
  jsr rtc_set
  plx
  rts

;----------------------------------------------------------------------
; rtc_set
;
; function: set rtc
;
; pass:		a register
;			x offset
;----------------------------------------------------------------------
rtc_set:
  ; Stop the clock
  jsr rtc_stop

  sta rtc,x

  ; Start the clock
  jsr rtc_start
  rts

;----------------------------------------------------------------------
; rtc_stop
;
; function: Sets TE (transfer enable) bit off
;
;----------------------------------------------------------------------
rtc_stop:
  pha
  lda rtc+$0f
  and #$7f
  sta rtc+$0f
  pla
  rts

;----------------------------------------------------------------------
; rtc_start
;
; function: Sets TE (transfer enable) bit on
;
;
;----------------------------------------------------------------------
rtc_start:
  pha
  lda rtc+$0f
  ora #$80
  sta rtc+$0f
  pla
  rts

;----------------------------------------------------------------------
; bcd_to_bin
;
; function: convert binary coded decimal to binary
;
; pass: X
; return: A
;----------------------------------------------------------------------
bcd_to_bin:
  phx
  ldx #$ff
  sec
  sed
@1:
  inx
  sbc #1
  bcs @1
  cld
  txa
  plx
  rts

;----------------------------------------------------------------------
; bin_to_bcd
;
; function: convert binary to binary coded decimal
;
; pass:	Y
; return: A
;----------------------------------------------------------------------
bin_to_bcd:
  phy
  tay
  lda #0
  sed
@loop:
  cpy #0
  beq @end
  clc
  adc #1
  dey
  bra @loop
@end:
  cld
  ply
  rts
