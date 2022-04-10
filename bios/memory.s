;----------------------------------------------------------------------
; Memory Library
; By Robert Locke
;
;----------------------------------------------------------------------

.feature labels_without_colons

; includes

; imports

; exports
.export memory_init
.export memory_begin, memory_end

.export memory_move_down
.export memory_move_up
.export size, sizeh, sizel
.exportzp to, from

.segment "ZEROPAGE"

zp_memory_end .res 2
to	.res 2
from	.res 2

.segment "BIOSVAR"
memory_begin .res 2
memory_end .res 2
size:
sizeh	.res 1
sizel	.res 1

.segment "MEMORY"


;----------------------------------------------------------------------
; memory_init
;
; function: initalize memory by placing $AA then $55 into memory location
;	once done places end memory location into bios variable
;
; return:	memory_end bios variable
;----------------------------------------------------------------------

memory_init:
  ldy #$01
  lda memory_begin,y
  sta zp_memory_end,y
  ldy #$00
  lda memory_begin
  sta zp_memory_end
@main:
  lda #$AA
  sta (zp_memory_end),y
  lda (zp_memory_end),y
  cmp #$AA
  bne @top
  lsr A
  sta (zp_memory_end),y
  lda (zp_memory_end),y
  cmp #$55
  bne @top
  inc zp_memory_end
  bne @main
  inc zp_memory_end+1
  bne @main
@top:
  clc
  lda zp_memory_end
  bne @end
  dec zp_memory_end+1
@end:
  dec zp_memory_end
  lda zp_memory_end
  sta memory_end
  lda zp_memory_end+1
  sta memory_end+1
  rts

;--------------------------------------------------------------- 
; memory_move_down
;
; Function: Copy a section of memory down
; 
; Taken from the following:
; http://6502.org/source/general/memory_move.html
;
;--------------------------------------------------------------- 
; Move memory down
;
; FROM = source start address
;   TO = destination start address
; SIZE = number of bytes to move
;
;--------------------------------------------------------------- 
memory_move_down:
  phx
  phy
  ldy #0
  ldx sizeh
  beq md2
md1:
  lda (from),y ; move a page at a time
  sta (to),y
  iny
  bne md1
  inc from+1
  inc to+1
  dex
  bne md1
md2:
  ldx sizel
  beq md4
md3:
  lda (from),y ; move the remaining bytes
  sta (to),y
  iny
  dex
  bne md3
md4:
  ply
  plx
  rts

;-----------------------------------------------------------------------------------
; Move memory up
;
; FROM = source start address
;   TO = destination start address
; SIZE = number of bytes to move
;
;-----------------------------------------------------------------------------------
memory_move_up:
  ldx sizeh    ; the last byte must be moved first
  clc          ; start at the final pages of from and to
  txa
  adc from+1
  sta from+1
  clc
  txa
  adc to+1
  sta to+1
  inx
  ldy sizel
  beq mu3
  dey          ; move bytes on the last page first
  beq mu2
mu1:
  lda (from),y
  sta (to),y
  dey
  bne mu1
mu2:
  lda (from),y ; handle y = 0 separately
  sta (to),y
mu3:
  dey
  dec from+1   ; move the next page (if any)
  dec to+1
  dex
  bne mu1
  rts
