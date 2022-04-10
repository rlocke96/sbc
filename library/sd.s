.export sd_readsector, sd_set_destination, sd_set_cmd_address, sd_next_sector, sd_init


SD_CS   = %00010000
SD_SCK  = %00001000
SD_MOSI = %00000100
SD_MISO = %00000010
SD_CD   = %00000001


PORTA_OUTPUTPINS =  SD_CS | SD_SCK | SD_MOSI

zp_sd_cmd_address = $40
zp_memory_destination = $42
zp_sector_count = $FF

sd_init:
  ; Let the SD card boot up, by pumping the clock with SD CS disabled

  ; We need to apply around 80 clock pulses with CS and MOSI high.
  ; Normally MOSI doesn't matter when CS is high, but the card is
  ; not yet is SPI mode, and in this non-SPI state it does care.

  lda #PORTA_OUTPUTPINS   ; Set various pins on port A to output
  sta DDRA

  lda #SD_CS | SD_MOSI
  ldx #160               ; toggle the clock 160 times, so 80 low-high transitions

.preinitloop:
  eor #SD_SCK
  sta PORTA
  dex
  bne .preinitloop


.cmd0 ; GO_IDLE_STATE - resets card to idle state, and SPI mode
  lda #$0
  jsr print_sd_cmd


  lda #<cmd0_bytes
  sta zp_sd_cmd_address
  lda #>cmd0_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne .initfailed

.cmd8 ; SEND_IF_COND - tell the card how we want it to operate (3.3V, etc)
  lda #$08
  jsr print_sd_cmd

  lda #<cmd8_bytes
  sta zp_sd_cmd_address
  lda #>cmd8_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne .initfailed

  ; Read 32-bit return value, but ignore it
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte

.cmd55 ; APP_CMD - required prefix for ACMD commands
  lda #$55
  jsr print_sd_cmd

  lda #<cmd55_bytes
  sta zp_sd_cmd_address
  lda #>cmd55_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne .initfailed

.cmd41 ; APP_SEND_OP_COND - send operating conditions, initialize card
  lda #$41
  jsr print_sd_cmd

  lda #<cmd41_bytes
  sta zp_sd_cmd_address
  lda #>cmd41_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Status response $00 means initialised
  cmp #$00
  beq .initialized

  ; Otherwise expect status response $01 (not initialized)
  cmp #$01
  bne .initfailed

  ; Not initialized yet, so wait a while then try again.
  ; This retry is important, to give the card time to initialize.
  jsr longdelay
  jmp .cmd55


.initialized
  lda #'Y'
  jsr print_char
  rts

.initfailed
  lda #'X'
  jsr print_char
  rts

cmd0_bytes
  .byte $40, $00, $00, $00, $00, $95
cmd8_bytes
  .byte $48, $00, $00, $01, $aa, $87
cmd55_bytes
  .byte $77, $00, $00, $00, $00, $01
cmd41_bytes
  .byte $69, $40, $00, $00, $00, $01



sd_readbyte:
  ; Enable the card and tick the clock 8 times with MOSI high,
  ; capturing bits from MISO and returning them

  ldx #8                      ; we'll read 8 bits
@loop:

  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta PORTA
  jsr delay

  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta PORTA
  jsr delay

  lda PORTA                   ; read next bit
  and #SD_MISO

  clc                         ; default to clearing the bottom bit
  beq @bitnotset              ; unless MISO was set
  sec                         ; in which case get ready to set the bottom bit
@bitnotset:

  tya                         ; transfer partial result from Y
  rol                         ; rotate carry bit into read result
  tay                         ; save partial result back to Y

  dex                         ; decrement counter
  bne @loop                   ; loop if we need to read more bits

  rts


sd_writebyte:
  ; Tick the clock 8 times with descending bits on MOSI
  ; SD communication is mostly half-duplex so we ignore anything it sends back here

  ldx #8                      ; send 8 bits

@loop:
  asl                         ; shift next bit into carry
  tay                         ; save remaining bits for later

  lda #0
  bcc @sendbit                ; if carry clear, don't set MOSI for this bit
  ora #SD_MOSI

@sendbit:
  sta PORTA                   ; set MOSI (or not) first with SCK low
  jsr delay
  eor #SD_SCK
  sta PORTA                   ; raise SCK keeping MOSI the same, to send the bit
  jsr delay
  tya                         ; restore remaining bits to send

  dex
  bne @loop                   ; loop if there are more bits to send

  rts


sd_waitresult:
  ; Wait for the SD card to return something other than $ff
  jsr sd_readbyte
  cmp #$ff
  beq sd_waitresult
  rts


sd_sendcommand:
  ldx #0
  lda (zp_sd_cmd_address,x)

  lda #SD_MOSI           ; pull CS low to begin command
  sta PORTA

  ldy #0
  lda (zp_sd_cmd_address),y    ; command byte
  jsr sd_writebyte
  ldy #1
  lda (zp_sd_cmd_address),y    ; data 1
  jsr sd_writebyte
  ldy #2
  lda (zp_sd_cmd_address),y    ; data 2
  jsr sd_writebyte
  ldy #3
  lda (zp_sd_cmd_address),y    ; data 3
  jsr sd_writebyte
  ldy #4
  lda (zp_sd_cmd_address),y    ; data 4
  jsr sd_writebyte
  ldy #5
  lda (zp_sd_cmd_address),y    ; crc
  jsr sd_writebyte

  jsr sd_waitresult
  pha

  ; Debug print the result code
  ;jsr print_hex

  ; End command
  lda #SD_CS | SD_MOSI   ; set CS high again
  sta PORTA

  pla   ; restore result code
  rts

sd_readsector

  ; Command 17, arg is sector number, crc not checked
  lda #$51           ; CMD17 - READ_SINGLE_BLOCK
  ldy #0
  sta (zp_sd_cmd_address),y
  jsr sd_sendcommand
  cmp #$00
  beq @readsuccess
  jmp @end

@readsuccess

  ; wait for data
  jsr sd_waitresult
  cmp #$fe
  beq @readgotdata

  jmp @end

@readgotdata
  ; Need to read 512 bytes.
  ldx #$02
@readloop_high
  ldy #$00
@readloop_low
  phx
  phy
  jsr sd_readbyte
  ply
  plx
  sta (zp_memory_destination),y
  iny
  bne @readloop_low
  inc zp_memory_destination+1
  dex
  bne @readloop_high

  ; End command
  lda #SD_CS | SD_MOSI
  sta PORTA
@end
  rts

sd_writesector
  rts

sd_next_sector
  CLC
  LDY #$04
  LDA (zp_sd_cmd_address),y
  ADC #$01
  STA (zp_sd_cmd_address),Y
  BCC @done
@loop
  DEY
  LDA (zp_sd_cmd_address),y
  ADC #$00
  STA (zp_sd_cmd_address),Y
  BNE @loop
@done
  JSR sd_readsector
  rts

sd_set_destination
  ; Read Sectors 0 to 1
  ; Set Destination Address
  stx zp_memory_destination
  sty zp_memory_destination+1
  rts

sd_set_cmd_address
  stx zp_sd_cmd_address
  sty zp_sd_cmd_address+1
  rts

print_sd_cmd
  lda #'.'
  jsr print_char

  rts

