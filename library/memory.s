; Memory Library
; By Robert Locke

.export memory_init
zp_memory_begin = $D0
zp_memory_end = zp_memory_begin + 2

memory_init
  ldy #$01
  lda zp_memory_begin,y
  sta zp_memory_end,y
  ldy #$00
  lda zp_memory_begin
  sta zp_memory_end
@main
  lda #$AA
  sta (zp_memory_end),y
  lda (zp_memory_end),y
  cmp #$AA
  bne @top
  lsr a
  sta (zp_memory_end),y
  lda (zp_memory_end),y
  cmp #$55
  bne @top
  inc zp_memory_end
  bne @main
  inc zp_memory_end+1
  bne @main
@top
  clc
  lda zp_memory_end
  bne @end
  dec zp_memory_end+1
@end
  dec zp_memory_end
  rts

