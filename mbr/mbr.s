; mbr.s
; version 0.0.5
; by Robert Locke
; started 11/6/2020
; last edit 04/08/2022

.include "bios.inc"
.include "mbr.inc"

.segment "ZEROPAGE"

zp_tmp_holder .res 2

;-------------------------------------------------------------------------------
; Master Boot Record Partition Offsets
;-------------------------------------------------------------------------------
mbr_partition         = $1be
mbr_partition_size    = $10
mbr_partition_type    = $04
mbr_partition_start   = $08
mbr_partition_length  = $0c
mbr_partition_state   = $00


.segment "MBR"
begin:

  lda #$00
  sta delay_timer	; Delay timer set to 0

  jsr print_nlcr
  ldx #<message_begin
  ldy #>message_begin
  jsr print_string

  ;load location of beginning of partition entry
  lda #<begin + <mbr_partition
  sta zp_tmp_holder
  lda #>begin + >mbr_partition
  sta zp_tmp_holder+1

@part:
  ldy #mbr_partition_type
  lda (zp_tmp_holder),y
  cmp #$0b
  beq @fat32
  cmp #$0c
  beq @fat32
  jmp @next_part

@fat32:
  ldy #mbr_partition_state
  lda (zp_tmp_holder),y
  cmp #$80
  bne @next_part

  ldy #mbr_partition_start
  ldx #$00
@loop:
  lda (zp_tmp_holder),y
  sta sector_lba,x
  inx
  iny
  cpx #$04
  bne @loop

  ldy 0
@loop1:
  lda begin_copy,y
  sta $1000,y
  iny
  cpy #end_copy-begin_copy
  bne @loop1
  ldx #<message_jmp
  ldy #>message_jmp
  jsr print_string  
  jmp $1000

@next_part:
  clc
  lda zp_tmp_holder
  adc #$10
  cmp #<begin + <mbr_partition + 64 	; Are we at the end of the partition table
  beq @no_boot_partition
  sta zp_tmp_holder
  jmp @part

@no_boot_partition:
@error:

  ldx #<message_part
  ldy #>message_part
  jsr print_string

done:
  jmp done

begin_copy:
  jsr sdcard_read_sector
  jmp sector_buffer+$5A
end_copy:
