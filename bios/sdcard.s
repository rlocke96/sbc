;----------------------------------------------------------------------
; sdcard.d
;
;----------------------------------------------------------------------

  .include "spi.inc"
  .include "sdcard.inc"

.segment "BIOSVAR"

sdcard_param:
  .res 1
sector_lba:
  .res 4
  .res 1 

cmd_idx = sdcard_param
cmd_arg = sdcard_param + 1
cmd_crc = sdcard_param + 5

.segment "SECTOR_BUFFER"
sector_buffer:
  .res 512
sector_buffer_end:

.segment "SDCARD"
;------------------------------------------------------------------------
; send_cmd
;
;
;------------------------------------------------------------------------
send_cmd:

  jsr _spi_begin

  lda cmd_idx
  ora #$40
  jsr _spi_write
  lda cmd_arg + 3
  jsr _spi_write
  lda cmd_arg + 2
  jsr _spi_write
  lda cmd_arg + 1
  jsr _spi_write
  lda cmd_arg + 0
  jsr _spi_write
  lda cmd_crc
  jsr _spi_write

  ; Wait for response
  jsr sd_waitresult
  
  jsr _spi_end
  
  rts

;------------------------------------------------------------------------
; sdcard_init
; 
; result: C=0 -> error, C=1 -> success
;------------------------------------------------------------------------
sdcard_init:
  jsr _spi_init
 
  jsr _spi_end
 
  ldx #10
@1:
  jsr _spi_read
  dex
  bne @1

  jsr _spi_begin
  ; Enter idle state
  ; cmd0
  stz cmd_idx
  stz cmd_arg + 3
  stz cmd_arg + 2
  stz cmd_arg + 1
  stz cmd_arg + 0
  lda #$95
  sta cmd_crc
  jsr send_cmd

  ; Send cmd8
  lda #08
  sta cmd_idx
  stz cmd_arg + 3
  stz cmd_arg + 2
  lda #$01
  sta cmd_arg + 1
  lda #$aa
  sta cmd_arg + 0
  lda #$87
  sta cmd_crc
  jsr send_cmd

  jsr _spi_begin
  jsr _spi_read
  jsr _spi_read
  jsr _spi_read
  jsr _spi_read
  jsr _spi_end

@cmd55:
  ; Send cmd55
  lda #55
  sta cmd_idx
  stz cmd_arg + 3
  stz cmd_arg + 2
  stz cmd_arg + 1
  stz cmd_arg + 0
  lda #$01
  sta cmd_crc
  jsr send_cmd
  
  lda #41
  sta cmd_idx
  lda #$40
  sta cmd_arg + 3
  jsr send_cmd

  cmp #$00
  beq @ready

  cmp #$01
  bne @error
 
  jmp @cmd55 ; change to CMD55 later

@ready:
  jsr _spi_end
  sec
  rts 

@error:
  jsr _spi_end
  clc 
  rts

;------------------------------------------------------------------------------------------------
; sdcard_read_sector:
; 
;------------------------------------------------------------------------------------------------
sdcard_read_sector:

  lda #$51
  sta cmd_idx
  lda #$1
  sta cmd_crc
  jsr send_cmd

  cmp #$00
  beq @ready
  
@error: 
  jsr _spi_end
  clc
  rts

@ready:
  jsr _spi_begin
  ; Wait for start of data packet
  ldx #0
@1:
  ldy #0
@2:
  jsr _spi_read
  cmp #$fe
  beq @start
  dey
  bne @2
  dex
  bne @1
  jmp @error

@start:
  ldy #0
@readloop_low:
  jsr _spi_read
  sta sector_buffer,y
  iny
  bne @readloop_low
@readloop_high:
  jsr _spi_read
  sta sector_buffer+256,y
  iny
  bne @readloop_high

  ; Finished Reading 512 bytes
  ; Now reading 16 bit checksum
  jsr _spi_read
  jsr _spi_read

  jsr _spi_end
  sec
 
  rts 


;-----------------------------------------------------------------------------------------
; sd_waitresult
; result: C=0 -> error, C=1 -> success
; 	  A contains result
;-----------------------------------------------------------------------------------------
sd_waitresult:
  phx
  ldx #$03
@loop:
  dex
  beq @error 
  ; wait for the sd card to return something other than $ff
  jsr _spi_read
  cmp #$ff
  beq @loop
  sec
  plx
  rts
@error:
  clc
  plx
  rts

;----------------------------------------------------------------------------------------
; sdcard_check_alive
;----------------------------------------------------------------------------------------
sdcard_check_alive:

  rts

;----------------------------------------------------------------------------------------
; sdcard_write_sector
;----------------------------------------------------------------------------------------
sdcard_write_sector:

  rts

