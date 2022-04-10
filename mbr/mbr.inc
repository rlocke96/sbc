message_part:
  .byte "error: no partition found",$00
message_begin:
  .byte "mbr: reading partition info",$0a,$0d,$00
message_jmp:
  .byte "Partition sector and jumping.",$0a,$0d,$00
