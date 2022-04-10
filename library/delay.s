.export longdelay, mediumdelay, delay

.include "address.i"

delay
  PHX
  ldx DELAY_TIMER
@loop
  dex
  bne @loop
  PLX
  rts



;
longdelay
  jsr mediumdelay
  jsr mediumdelay
  jsr mediumdelay
mediumdelay
  jsr delay
  jsr delay
  jmp delay
