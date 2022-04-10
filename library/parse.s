;
; Parse Command
; By Robert Locke
;




CMD_LIST
  .byte 6,'A' ; Apple
  .word LIST_APPLE
  .byte 5,'A' ; Able
  .word LIST_ABLE

LIST_APPLE
  .byte "PPLE"
  .word CODE_APPLE

LIST_ABLE
  .byte "BLE"
  .word CODE_ABLE

CODE_APPLE
  LDX #<APPLE_STRING
  LDY #>APPLE_STRING
  JSR print_string
  RTS

CODE_ABLE
  LDX #<ABLE_STRING
  LDY #>ABLE_STRING
  JSR print_string
  RTS

APPLE_STRING
  .byte "APPLE",$00

ABLE_STRING
  .byte "ABLE",$00
