; bios
; version .4
; started 2022/03/19
; by robert locke
;

; .include
.include "io.inc"
.include "memory.inc"
.include "print.inc"
.include "acia.inc"
.include "sdcard.inc"

; .import
.import acia_init

; .export
.export irqvec, nmivec, memory_start

.segment "BSS"
memory_start .res 1

.segment "BIOSVAR"
irqvec .res 2
nmivec .res 2


; .segment
.segment "STARTUP"

; reset vectors
irq_vec:
  jmp (irqvec)  ; irq vector

nmi_vec:
  jmp (nmivec)  ; nmi vector

main:
  cld
  ldx #$ff
  txs

  jsr memory_init   ; detect memory size

  lda #$ff
  sta delay_timer ; used for delay on acia out

  jsr acia_init ; setup acia

  lda #'.'
  jsr print_char

  jsr print_nlcr
  ldx #<message_welcome
  ldy #>message_welcome
  jsr print_string  ; prints welcome message of bios

  ldx #<message_mem_start
  ldy #>message_mem_start
  jsr print_string

  lda #>memory_start
  sta memory_begin+1   ; sets beginning location of ram to use
  jsr print_hex
  lda #<memory_start
  sta memory_begin
  jsr print_hex
  jsr print_nlcr

  ldx #<message_mem_end
  ldy #>message_mem_end
  jsr print_string

  lda memory_end + 1
  jsr print_hex
  lda memory_end
  jsr print_hex
  jsr print_nlcr

    jsr sdcard_init       ; sd card initialize
    bcc sdcard_error

    lda #$00
    ldy #$00
@loop:
    sta sector_lba,y
    iny
    cpy #$04
    bne @loop
    lda #01
    sta sector_lba,y  ; setup memory address to 00,00,00,00

    jsr sdcard_read_sector ; read sd card and copied to destination

    lda sector_buffer + $1fe
    cmp #$55
    bne error_message

    lda sector_buffer + $1ff
    cmp #$aa
    bne error_message   ; reads from ram a copy of the first sector of the sd
                        ; card. should end with 55aa.

    ldx #<message_boot
    ldy #>message_boot
    jsr print_string

    jmp sector_buffer ; jump to code section of mbr of sd card

error_message:
  jsr print_nlcr
  ldx #<bios_message_error
  ldy #>bios_message_error
  jsr print_string
  jsr get_char
@loop:
  jmp @loop

sdcard_error:
  jsr print_nlcr
  ldx #<bios_message_init
  ldy #>bios_message_init
  jsr print_string
@loop:
  jmp @loop

bios_message_error:
  .byte "error: 55aa not found at end of sector",$00
message_welcome:
  .byte "SBC bios .5",$0a,$0d,$00
message_boot:
  .byte "Booting MBR now...",$0a,$0d,$00
bios_message_init:
  .byte "Error initalizing sdcard",$0a,$0d,$00
message_mem_start:
  .byte "Initialize Memory:",$0a,$0d,"Memory Start: ",$00
message_mem_end:
  .byte "Memory End: ",$00

.segment "VECTORS"
vector:
  .word nmi_vec
  .word main
  .word irq_vec
