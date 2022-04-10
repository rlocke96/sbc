.export ACIAout, ACIAin, acia_init

.include "address.inc"

acia_init
  LDA     #$00
  STA     ACIASTATUS            ; Soft reset
  LDA     #$0B
  STA     ACIACMD               ; Parity disabled, IRQ disabled
  LDA     #$1F
  STA     ACIACTRL              ; Set output for 8-N-1 19200
  RTS


; byte out to ACIA
ACIAout
        PHA                             ; save accumulator
.loop
        LDA     ACIASTATUS              ; Read 6551 status
        AND     #$10                    ; Is tx buffer full?
        BEQ     .loop                   ; if not, loop back
        PLA                             ; Otherwise, restore accumulator
        STA     ACIADATA                ; write byte to 6551
        JSR     mediumdelay
        RTS
        .BYTE $00


;
; byte in from ACIA.
;
ACIAin
  LDA     ACIASTATUS            ; Read 6551 status
  AND     #$08                    ;
  BEQ     .exit               ; If rx buffer empty, no byte
  LDA     ACIADATA              ; Read byte from 6551
.done
  SEC                             ; Flag byte received
  RTS
.exit
  CLC                             ; flag no byte received
  RTS
