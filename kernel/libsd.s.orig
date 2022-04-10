; sd card interface module
;
; requires zero-page variable storage:
;   zp_sd_address - 2 bytes
;   zp_sd_currentsector - 4 bytes

.include "io.inc"
.include "bios.inc"

.export sd_init
.export sd_readbyte
.export sd_readsector
.exportzp zp_sd_address
.exportzp zp_sd_currentsector

sd_cs   = %00010000
sd_sck  = %00001000
sd_mosi = %00000100
sd_miso = %00000010

.segment "ZEROPAGE"
zp_sd_address:
	.res 2
zp_sd_currentsector:
	.res 4

.segment "CODE"
sd_init:
  ; let the sd card boot up, by pumping the clock with sd cs disabled

  ; we need to apply around 80 clock pulses with cs and mosi high.
  ; normally mosi doesn't matter when cs is high, but the card is
  ; not yet is spi mode, and in this non-spi state it does care.

  lda #sd_cs | sd_mosi
  ldx #160               ; toggle the clock 160 times, so 80 low-high transitions
@preinitloop:
  eor #sd_sck
  sta portb
  dex
  bne @preinitloop
  

@cmd0 ; go_idle_state - resets card to idle state, and spi mode
  lda #<sd_cmd0_bytes
  sta zp_sd_address
  lda #>sd_cmd0_bytes
  sta zp_sd_address+1

  jsr sd_sendcommand

  ; expect status response $01 (not initialized)
  cmp #$01
  bne @initfailed

@cmd8 ; send_if_cond - tell the card how we want it to operate (3.3v, etc)
  lda #<sd_cmd8_bytes
  sta zp_sd_address
  lda #>sd_cmd8_bytes
  sta zp_sd_address+1

  jsr sd_sendcommand

  ; expect status response $01 (not initialized)
  cmp #$01
  bne @initfailed

  ; read 32-bit return value, but ignore it
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte

@cmd55 ; app_cmd - required prefix for acmd commands
  lda #<sd_cmd55_bytes
  sta zp_sd_address
  lda #>sd_cmd55_bytes
  sta zp_sd_address+1

  jsr sd_sendcommand

  ; expect status response $01 (not initialized)
  cmp #$01
  bne @initfailed

@cmd41 ; app_send_op_cond - send operating conditions, initialize card
  lda #<sd_cmd41_bytes
  sta zp_sd_address
  lda #>sd_cmd41_bytes
  sta zp_sd_address+1

  jsr sd_sendcommand

  ; status response $00 means initialised
  cmp #$00
  beq @initialized

  ; otherwise expect status response $01 (not initialized)
  cmp #$01
  bne @initfailed

  ; not initialized yet, so wait a while then try again.
  ; this retry is important, to give the card time to initialize.

  ldx #0
  ldy #0
@delayloop
  dey
  bne @delayloop
  dex
  bne @delayloop

  jmp @cmd55


@initialized
  lda #'y'
  jsr print_char
  rts

@initfailed
  lda #'x'
  jsr print_char
@loop
  jmp @loop


sd_cmd0_bytes
  .byte $40, $00, $00, $00, $00, $95
sd_cmd8_bytes
  .byte $48, $00, $00, $01, $aa, $87
sd_cmd55_bytes
  .byte $77, $00, $00, $00, $00, $01
sd_cmd41_bytes
  .byte $69, $40, $00, $00, $00, $01



sd_readbyte:
  ; enable the card and tick the clock 8 times with mosi high, 
  ; capturing bits from miso and returning them

  ldx #$fe    ; preloaded with seven ones and a zero, so we stop after eight bits

.loop:

  lda #sd_mosi                ; enable card (cs low), set mosi (resting state), sck low
  sta portb

  lda #sd_mosi | sd_sck       ; toggle the clock high
  sta portb

  lda portb                   ; read next bit
  and #sd_miso

  clc                         ; default to clearing the bottom bit
  beq .bitnotset              ; unless miso was set
  sec                         ; in which case get ready to set the bottom bit
.bitnotset:

  txa                         ; transfer partial result from x
  rol                         ; rotate carry bit into read result, and loop bit into carry
  tax                         ; save partial result back to x
  
  bcs .loop                   ; loop if we need to read more bits

  rts


sd_writebyte:
  ; tick the clock 8 times with descending bits on mosi
  ; sd communication is mostly half-duplex so we ignore anything it sends back here

  ldx #8                      ; send 8 bits

@loop:
  asl                         ; shift next bit into carry
  tay                         ; save remaining bits for later

  lda #0
  bcc @sendbit                ; if carry clear, don't set mosi for this bit
  ora #sd_mosi

@sendbit:
  sta portb                   ; set mosi (or not) first with sck low
  eor #sd_sck
  sta portb                   ; raise sck keeping mosi the same, to send the bit

  tya                         ; restore remaining bits to send

  dex
  bne @loop                   ; loop if there are more bits to send

  rts


sd_waitresult:
  ; wait for the sd card to return something other than $ff
  jsr sd_readbyte
  cmp #$ff
  beq sd_waitresult
  rts


sd_sendcommand:
  ; debug print which command is being executed

  lda #'c'
  jsr print_char
  ldx #0
  lda (zp_sd_address,x)
  jsr print_hex

  lda #sd_mosi           ; pull cs low to begin command
  sta portb

  ldy #0
  lda (zp_sd_address),y    ; command byte
  jsr sd_writebyte
  ldy #1
  lda (zp_sd_address),y    ; data 1
  jsr sd_writebyte
  ldy #2
  lda (zp_sd_address),y    ; data 2
  jsr sd_writebyte
  ldy #3
  lda (zp_sd_address),y    ; data 3
  jsr sd_writebyte
  ldy #4
  lda (zp_sd_address),y    ; data 4
  jsr sd_writebyte
  ldy #5
  lda (zp_sd_address),y    ; crc
  jsr sd_writebyte

  jsr sd_waitresult
  pha

  ; debug print the result code
  jsr print_hex

  ; end command
  lda #sd_cs | sd_mosi   ; set cs high again
  sta portb

  pla   ; restore result code
  rts


sd_readsector:
  ; read a sector from the sd card.  a sector is 512 bytes.
  ;
  ; parameters:
  ;    zp_sd_currentsector   32-bit sector number
  ;    zp_sd_address     address of buffer to receive data
  
  lda #sd_mosi
  sta portb

  ; command 17, arg is sector number, crc not checked
  lda #$51                    ; cmd17 - read_single_block
  jsr sd_writebyte
  lda zp_sd_currentsector+3   ; sector 24:31
  jsr sd_writebyte
  lda zp_sd_currentsector+2   ; sector 16:23
  jsr sd_writebyte
  lda zp_sd_currentsector+1   ; sector 8:15
  jsr sd_writebyte
  lda zp_sd_currentsector     ; sector 0:7
  jsr sd_writebyte
  lda #$01                    ; crc (not checked)
  jsr sd_writebyte

  jsr sd_waitresult
  cmp #$00
  bne @fail

  ; wait for data
  jsr sd_waitresult
  cmp #$fe
  bne @fail

  ; need to read 512 bytes - two pages of 256 bytes each
  jsr @readpage
  inc zp_sd_address+1
  jsr @readpage
  dec zp_sd_address+1

  ; end command
  lda #sd_cs | sd_mosi
  sta portb

  rts


@fail
  lda #'s'
  jsr print_char
  lda #':'
  jsr print_char
  lda #'f'
  jsr print_char
@failloop
  jmp @failloop


@readpage
  ; read 256 bytes to the address at zp_sd_address
  ldy #0
@readloop
  jsr sd_readbyte
  sta (zp_sd_address),y
  iny
  bne @readloop
  rts



