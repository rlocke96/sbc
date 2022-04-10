.if .defined(MACHINE_SBC)

; base real time clock address
rtc = $8060

; base via address 
portb = $8030
porta = $8031
ddrb = $8032
ddra = $8033
spi_port := portb
spi_ddr  := ddrb
via                 = $8030
via_orb             = via
via_irb             = via
via_ora             = via + 1
via_ira             = via + 1
via_ddrb            = via + 2
via_ddra            = via + 3
via_t1cl            = via + 4
via_t1ch            = via + 5
via_t1ll            = via + 6
via_t1lh            = via + 7
via_t2cl            = via + 8
via_t2ch            = via + 9
via_sr              = via + 10
via_acr             = via + 11
via_pcr             = via + 12
via_ifr             = via + 13
via_ier             = via + 14

; 6551
acia_data    = $8000
acia_status  = $8001
acia_cmd     = $8002
acia_ctrl    = $8003
.else

acia_data    = $8800
acia_status  = acia_data + 1
acia_cmd     = acia_data + 2
acia_ctrl    = acia_data + 3

; base via address 
portb = $8000
porta = $8001
ddrb = $8002
ddra = $8003
via                 = $8000
via_orb             = via
via_irb             = via
via_ora             = via + 1
via_ira             = via + 1
via_ddrb            = via + 2
via_ddra            = via + 3
via_t1cl            = via + 4
via_t1ch            = via + 5
via_t1ll            = via + 6
via_t1lh            = via + 7
via_t2cl            = via + 8
via_t2ch            = via + 9
via_sr              = via + 10
via_acr             = via + 11
via_pcr             = via + 12
via_ifr             = via + 13
via_ier             = via + 14



.endif
