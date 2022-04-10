;
; kernel.bin
; by robert locke
; version .04
; start 11/8/2020
; v.04 start 3/20/2022
;


.include "bios.inc"
;.include "spi.inc"
;.include "sdcard.inc"
.include "fat32.inc"
.include "regs.inc"

.import fat32_dirent

.segment "BEGIN"
main
  ldx #$FF
  txs
  cld

  LDX #<CLEAR_SCREEN_ESC
  LDY #>CLEAR_SCREEN_ESC
  JSR print_string

; Initialize SD Card
;  jsr sdcard_init
;  bcc error

  jsr fat32_init
  bcc error

  lda #'.'
  jsr print_char

  lda #0
  jsr fat32_alloc_context
  sta context

  lda #'.'
  jsr print_char

  lda #$df
  sta skip_mask

  lda #<file_name
  sta fat32_ptr
  lda #>file_name
  sta fat32_ptr+1
  jsr fat32_open
  bcc error

loop:
    ; read and print byte
    jsr fat32_read_byte
    bcc end
    jsr print_char
    jmp loop

end:
    jsr fat32_close

    lda context
    jmp fat32_free_context


  lda #'.'
  jsr print_char

end_of_program:
  jmp end_of_program

error:
  LDX #<KERNEL_ERROR
  LDY #>KERNEL_ERROR
  JSR print_string

  lda fat32_errno
  jsr print_hex
  jmp end_of_program

file_name:
  .byte "/84-0.txt",0
context:
  .byte 0
CLEAR_SCREEN_ESC:
  .byte $1B,"c",$00
KERNEL_MESSAGE:
  .byte "Kernel",$0A, $0D,$00
KERNEL_ERROR:
  .byte "Error: ", $00
