                           TITLE PAGE


                        INTERACTIVE TOY

                   (FURBY.ASM - Version 25)


                    INVENTOR:  Dave Hampton











Attorney Docket No. 64799
FITCH, EVEN, TABIN & FLANNERY
Suite 900
135 South LaSalle Street
Chicago, Illinois  60603-4277
Telephone (312) 372-7842

;╔════════════════════════════════════════════════════════════════════════════╗
;║                                                                            ║
;║      SPC81A Source Code   (Version 25)                                     ║
;║                                                                            ║
;║      Written by: Dave Hampton / Wayne Schulz                               ║
;║      Date:       July 30, 1998                                             ║
;║                                                                            ║
;║      Copyright (C) 1996,1997,1998 by Sounds Amazing!                       ║
;║      All rights reserved.                                                  ║
;╚════════════════════════════════════════════════════════════════════════════╝
;
;
;************************************************************************

;  remember   SBC    if there is a barrow carry is CLEARED
;  also SBC   if the two numbers are equal you still get a negative result

;
;
;╔══════════════════════════════════════════════════════════════════════════╗
;║   MODIFICATION LIST :                                                    ║
;
; Furby29/30/31/32
;     Final testing for shipment of code on 8/2/98.
;     Tables updated,■■■tor speed updated, wake up/name fix
;     sequential tables never getting first entry,fixed.
;     New diag5.asm, Light3.asm (if light osc stalls it wont hang system).
; 
; Furby33
;     In motor brake routine, turn motors off before turning reverse
;     braking pulse on to save transistors.
; 
; Furby34
;     Cleanup start code and wake routines.
;     Light sensor goes max dark and stays there to reff time, then
;     call sleep macro and shut down.
; 
; Furby35
;     Adds four new easter eggs,BURP ATTACK, SAY NAME, TWINkLE SONG,
;     and ROOSTER LOVES YOU. Also add new names.
;
;
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; Release 3

;; File "testR3a"

; 1. Light sensor has a hysteresis point of continually triggering sensor.
; 2. Light sensor decrements two instead of one on hungry counter.
; 3. Diagnos■■■■■■de for light sensor wont trigger very easily.
; 4. When a furby receives the I.R. sleep command he sends the same command
;    out before going to sleep.
;
; 5. When hungry is low enough to trigger sick counter, each sensor
;    deducts two instead of one for each hit.
;
; 6. When diagnostics complete clear memory, reset hungry & sick to FF
;    randomly choose new name and voice, then write EEPROM before
;    going to sleep. Also extend EEPROM diagnostic to test all locations
;    for pass/fail of device.
;
; 7. Add new light routine

; 8. Change hide and seek egg to light,light,light,tummy.

; 9. Change sick/hungry counter so that it can only get so sick and
;    not continue down to zero. (MAX_SICK)

;10. In diagnostics, motor position test ,,,, first goes forward continuously
;    until the front switch is pressed, then goes reverse continuously
;    until the front switch is pressed again, and then does normal position
;    calibration stopping at the calibration switch.

;11. On power up we still use tilt and invert to generate startup random
;    numbers, but if feed switch is pressed for cold boot, we use it to
;    generate random numbers, because it is controlled by the user where
;    the tilt and invert are more flaky.

;12. No matter what age, 25% of time he randomly pulls speech from age
;    to generate more Furbish at older ages.

;13. Twinkle song egg
;    When song is complete, if both front and back switches are pressed
;    we goto deep sleep. That means only the invert can wake us up, not
;    the tilt switch.
;║                                                                          ║
;╚══════════════════════════════════════════════════════════════════════════╝
;***********************************************************************
;***********************************************************************
;***********************************************************************
;***********************************************************************
;***********************************************************************

; Actual numeric value for TI pitch control

;  bit 7 set = subtract value from current course value
;        clr = add value to current course value
;  bit 6 set = select music pitch table
;        clr = select normal speech pitch table
;  bit 0-5 value to change course value (no change = 0)

; A math routine in 'say_0' converts the value for + or -
; if <80 then subtracts from 80 to get the minus version of 00
; ie, if number is 70 then TI gets sent 10 (which is -10)
; If number is 80 or > 80 then get sent literal as positive.

; NOTE: MAX POSITIVE IS 8F (+16 from normal voice of 00)
;       MAX NEGATIVE is 2F (-47 from normal voice of 00)

;This is a difference of 80h - 2Fh or 51h


; 8Fh is hi voice  (8f is very squeeeeeke)
; 2Fh lo voice ( very low)


; The math routine in 'Say_0' allows a +-decimal number in the speech table.
; A value of 80 = no change or 00 sent to TI
; 81 = +1
; 8f = +16
;
;Avalue of 7F = -1 from normal voice
;70 = -16

; The voice selection should take into consideration that the hi voice
; selection plus an additional offset is never greater than 8f
; Or a low voice minus offset never less than 2f.

Voice1      EQU   83h   ;(+3) hi voice
Voice2      EQU   7Ah   ;(-6) mid voice
Voice3      EQU   71h   ;(-15) low voice

;;;; we converted to a random selection table, but since all voice tables
;    use the equates plus some offset, we ■■■e the change in the SAY_0
;    routine. We always assign voice 3 which is the lowest, and based on
;    the random power up pitch selection, the ram location 'Rvoice' holds
;    the number to add to the voice+offset received from the macro table.

Voice EQU   Voice3        ;pitch (choose Voice1, Voice2, Voice3)(voice2=norm)

; Select Voice3 since it is the lowest and then add the difference to get
; Voice2 or Voice 3. Here we assign that difference to an equate to be
; used in the voice table that is randomly selected on power up.

S_voice1  EQU  18 ;Voice3 + 18d = Voice1
S_voice2  EQU  09 ;Voice3 + 09d = Voice2

S_voice3  EQU  0  ;Voice3 + 00d = Voice3



;***********************************************************************

; Motor speed pulse width :
; Motor_on = power to motor, Motor_off is none.


Mpulse_on   EQU   16  ;	
Mpulse_off  EQU   16  ;


Cal_pos_fwd EQU   134   ;calibration switch forward direction
Cal_pos_rev EQU   134   ;calibration switch forward direction

;***********************************************************************
;***********************************************************************
;***********************************************************************
;***********************************************************************
;***********************************************************************
;
;┌─────────────────────────────────────────────────────┐
;│                       PORTS                         │
;│ SPC40A has : 16 I/O pins                            │
;│ PORT_A 4 I/O pins  0-3                              │
;│ PORT_C 4 I/O pins  0-3                              │
;│ PORT_D 8 I/O pins  0-7                              │
;│                                                     │
;│                         RAM                         │
;│                                                     │
;│ SPC40A has : 128 bytes of RAM                       │
;│ from $80 - $FF                                      │
;│                                                     │
;│                         ROM                         │
;│ SPC40A has :                                        │
;│ BANK0 user ROM from $0600 - $7FFF                   │
;│ BANK1 user ROM from $8000 - $FFF9                   │
;│                                                     │
;│                                                     │
;│                         VECTORS                     │
;│ NMI   vector  $7FFA / $7FFB                         │
;│ RESET vector  $7FFC / $7FFD                         │
;│ IRQ   vector  $7FFE / $7FFF                         │
;└─────────────────────────────────────────────────────┘
;┌─────────────────────────────────────────────────────┐
;│                       PORTS                         │
;│ SPC120A has : 17 I/O pins                           │
;│ PORT_A 4 I/O pins  0-3                              │
;│ PORT_B 4 I/O pins  0,1,2,4,5                        │
;│ PORT_C 4 I/O pins  0-3 input only                   │
;│ PORT_D 8 I/O pins  0-7                              │
;│                                                     │
;│                         RAM                         │
;│ SPC120A has : 128 bytes of RAM                      │
;│ from $80 - $FF                                      │
;│                                                     │
;│                         ROM                         │
;│ SPC120A has :                                       │

;│ BANK0 user RO■■■■■■■$0600 - $7FFA                   │
;│ BANK1 user RO■■■■■■■$8000 - $FFFF                   │
;│ BANK2 user RO■■■■■■■$10000 - $17FFF                 │
;│ BANK3 user RO■■■■■■■$1A000 - $1FFFF                 │
;│                                                     │
;│                                                     │
;│                         VECTORS                     │
;│ NMI   vector  $7FFA / $7FFB                         │
;│ RESET vector  $7FFC / $7FFD                         │
;│ IRQ   vector  $7FFE / $7FFF                         │
;└─────────────────────────────────────────────────────┘


; unuseable areas in rom

;SPC40A:     8000H ──  DFFFH should be skiped (Dummy area)
;  bank 0 = 600 - 7FFA
;  bank 1 = 8000 - DFFF reserved , start @ E000 - FFFA

;SPC80A:     10000H ── 13FFFH should be skiped (Dummy area)
;  bank 0 = 600 - 7FFA
;  bank 1 = 8000 - FFFA
;  bank 2 = 10000-13FFF reserved ,  start at 14000 - 17FFF

;SPC120A: ;SPC120A: 18000H ── 19FFFH should be skiped (Dummy area)
;  bank 0 = 600 - 7FFA
;  bank 1 = 8000 - FFFA
;  bank 2 = 10000 - 17FFF
;  bank 3 = 18000 - 19FFF reserved ,  start at 1A000 - 1FFFA

;SPC256A: ;SPC256A: Non dummy area

;SPC512A: ;SPC512A: Non dummy area

;***********************************************************************

.CODE
.SYNTAX  6502
.LINKLIST
.SYMBOLS


;─────────────────── PORT DIRECTION CONTROL REGISTER    ────────────────────────
Ports_dir       EQU     00     ; (write only)
;
; (4 I/O pins) controlled with each bit of this register
; you can't control each pin separately, only as a nibble
; 0 = input / 1 = output
;
; 7      6       5       4       3       2       1       0        (REGISTER BITS)
; D      D       C       C       B       B       A       A        (PORT)
; 7654   3210    7654    3210    7654    3210    7654    3210     (PORT BITS)
;────────────────────────────────────────────────────────────────────────────────
; ────────────────── PORT CONFIGURATION CONTROL REGISTER      ──────────────────

;                 based on if the port pin is input or output
;
Ports_con       EQU     01     ; (write only)
;
; (4 I/O pins) controlled with each bit of this register
; 7      6      5      4      3      2      1      0               (REGISTER BITS)
; D      D      C      C      B      B      A      A               (PORT)
; 7654   3210   7654   3210   7654   3210   7654   3210            (PORT BITS)

; port_a INPUTS can be either:
; 0 = float   1 = pulled high

; port_a OUTPUTS can be either:
; 0 = buffer   1 = upper (4) bits Open dran Pmos (source)
;                 lower (4) bits Open drain Nmos (sink)
;
; port_b INPUTS can be either
; 0 = float   1 = pulled low

; port_b OUTPUTS can be either:
; 0 = buffer   1 = upper (4) bits Open dran Nmos (sink)
;                 lower (4) bits Open drain Nmos (sink)
;
; port_c INPUTS can be either:
; 0 = float   1 = pulled high
; port_c OUTPUTS can be either:
; 0 = buffer   1 = upper (4) bits Open drain Pmos (source)
;                 lower (4) bits Open drain Nmos (sink)
;
; port_d INPUTS can be either:
; 0 = float   1 = pulled low
; port_d OUTPUTS can be either:
; 0 = buffer   1 = Open drain Pmos (source)

;───────────────────────────────────────────────────────────────────────────────

;─────────────────────────── I/O PORTS     ─────────────────────────────────────

Port_A          EQU     02H     ; (read/write) for TI & speech recgn      CPU's
Data_D0           EQU   01H   ;bit 0 data nible port
Data_D1           EQU   02H   ;
Data_D2           EQU   04H   ;
Data_D3           EQU   08H   ;

Port_B            EQU   03H   ;b0/b1 = I/0 B4/B5 = inp only
TI_init           EQU   01H   ;B0 - TI reset control
TI_CTS            EQU   02H   ;B1 - hand shake to TI
IR_IN       EQU   10H   ;B4 - I.R. Recv data
TI_RTS            EQU   20H   ;B5 -TI wants data

Port_C          EQU     04H     ; (read/write)
Motor_cal   EQU   01H   ;C0 - lo when mot■■ crosses switch
Pos_sen           EQU   02H   ;C1 - moto■■■■■ical sensor (intt C1)
Touch_bck   EQU   04H   ;C2 - back touch
Touch frnt  EQU   08H   ;C3 - front touch

Port_D         EQU      05H     ; (read/write)
Ball_side   EQU   01H   ;D0 - hi when on any side (TILT)
Ball_invert EQU   02H   ;D1 - hi when inverted
Light_in    EQU   04H   ;D2 - hi when bright light hits sensor
Mic_in            EQU   08H   ;D3 - hi pulse microphone input
Power_on    EQU   10H   ;D4 - power to rest of circuit
Motor_led   EQU   20H   ;D5 - motor I.R. led driver
Motor_lt    EQU   40H   ;D6 - motor drive left (forward)
Motor_rt    EQU   80H   ;D7 - motor drive right (reverse)

;───────────────────────────────────────────────────────────────────────────────

;──────────────────────── DATA LATCH  PORT_D    ────────────────────────────────
Latch_D         EQU     06H     ; (read)
; read to latch data from pord_d, used for wake-up on pin change
;───────────────────────────────────────────────────────────────────────────────

;───────────────────────── BANK SELECTION REGISTER    ──────────────────────────
Bank            EQU     07H     ; (read/write)  x x x x x x x b
; 0 = bank 0, 1 = bank 1        ;               7 6 5 4 3 2 1 0
; only two banks in SPC40a
;───────────────────────────────────────────────────────────────────────────────────

;───────────────────────────── WAKE UP    ──────────────────────────────────────
Wake_up         EQU     08H     ; (read/write) x x x x x x x w
;                                              7 6 5 4 3 2 1■0

; w=(0=disable, 1=enable wake-up on port_d change)
; read to see if wake-up, or normal reset
; this is the only source for a wake-up
; Always reset stack on wake up.
;────────────────────────────────────────────────────────────────────────────────

;───────────────────────────── SLEEP    ────────────────────────────────────────
Sleep           EQU     09H     ; (write)      x x x x x x x s
;                               ;              7 6 5 4 3 2 1 0
;
; s=(0=don't care, 1=s■■■■■
; writting 1 to bit0, ■■■■s sleep
;───────────────────────────────────────────────────────────────────────────────

;──────────────────────── TIMER A CONTROL REGISTER    ──────────────────────────
; this needs more work to understand DMH
TMA_CON           EQU     0BH     ; (write)
;
;               7 6 5 4 3 2 1 0
;               m x x x
;
;               m= Timer one mode (0=Timer,1=Counter)

;
;                 Bit3: IE1 ■■ IE1= 0: Counter clock= external clock from IOC2
;                 Bit2: T1  ■■    = 1, T1= 0: counter clock= CPUCLK/8192
;                 Bit1: IE0 ■■         T1= 1: counter clock= CPUCLK/65536
;                 Bit0: T0  ■■ IE0= 0: Counter clock= external clock from IOC2
;                                 = 1, T0= 0: counter clock= CPUCLK/4
;                                      T0= 1: counter clock= CPUCLK/64
;───────────────────────────────────────────────────────────────────────────────

;──────────────────────────── INTERRUPTS    ────────────────────────────────────
Interrupts      EQU     0DH    ; (read/write)
;
;        7 6 5 4 3 2 1 0
;        w m a b 3 2 1 e
;
;        w = (0=watch dog ON, power-on default) (1=watch dog OFF)
;        m = (0=Timer A generates NMI INT, 1=Timer A generates IRQ INT)
;        a = (0=Timer A interrupt off, 1=Timer A interrupt on)
;        b = (0=Timer B interrupt off, 1=Timer B interrupt on)
;        3 = (0=CPU CLK/1024 interrupt off,  1=CPU CLK/1024 interrupt on)
;        2 = (0=CPU CLK/8192 interrupt off,  1=CPU CLK/8192 interrupt on)
;        1 = (0=CPU CLK/65536 interrupt off, 1=CPU CLK/65536 interrupt on)
;        e = (0=external interrupt off, 1=external interrupt on)
;             rising edge, from port_c bit1

;───────────────────────────────────────────────────────────────────────────────

;───────────────────────────── TIMERS    ───────────────────────────────────────
; There are two 12bits timers.
; Timer A can be either a timer or a counter. (as set by TIMER_CON)
; Timer B can only be used as a timer.
;
; Timers count-up and on overflow from 0FFF to 0000, this carry bit will
; create an interrupt if the corresponding bit is set in INTERRUPTS register.
; The timer will be auto reloaded with the user setup value, and start,,,
; count-up again.
;
; Counter will reset by user loading #00 into register TMA_LSB and TMA_MSB.
; Counter registers can be read on-the-fly, this will not affect register,,,
; values, or reset them.
;
;───────────────────────────────────────────────────────────────────────────────

;───────────────────────────── TIMER A (low byte) ──────────────────────────────
TMA_LSB         EQU     10H     (read/write)
;
; all 8bits valid (lower 8bits of 12bit timer)

;───────────────────────────────────────────────────────────────────────────────────

;───────────────────────── TIMER A (high byte)    ──────────────────────────────
TMA_MSB         EQU     11H     (read/write)
; read         x  x  x  x  11 10 9  8    timer upper 4bits
;              7  6  5  4  3  2  1  0
;
; write        x  x  t  c  11 10 9  8    timer upper 4bits
;              7  6  5  4  3  2  1  0    register bit
;
;               t=(0=speech mode, 1=Tone mode)
;               this connects the AUDA pin to either
;               the DAC■, or Timer generated square wave
;
;               c=(0=CPU clock,   1=CPU clock/4:
;───────────────────────────────────────────────────────────────────────────────

;───────────────────────────── TIMER B (low byte) ──────────────────────────────
TMB_LSB         EQU     12H
;
; all 8bits valid (lower 8bits of 12bit timer)
;───────────────────────────────────────────────────────────────────────────────

;───────────────────────── TIMER B (high byte)    ──────────────────────────────
TMB_MSB         EQU     13H
; read         x  x  x  x  11 10 9  8    timer upper 4bits
;              7  6  5  4  3  2  1  0
;
; write        x  x  t  c  11 10 9  8    timer upper 4bits
;              7  6  5  4  3  2  1  0    register bit
;
;               t=(0=speech mode, 1=Tone mode)
;               this connects the AUDA pin to either
;               the DAC2, or Timer generated square wave
;
;               c=(0=CPU clock,   1=CPU clock/4:
;───────────────────────────────────────────────────────────────────────────────
;──────────────────────────  D/A converters    ─────────────────────────────────
DAC1            EQU     14H     ; (write)
DAC2            EQU     15H     ; (write)
;───────────────────────────────────────────────────────────────────────────────

;───────────────────────────────────────────────────────────────────────────────
; this needs more work to understand DMH
;    16H    ADCoutputPort16H:

DAC_ctrl    EQU   16H

;

Page  11 / 297

Page  12 / 297

Page  13 / 297

Page  14 / 297

Page  15 / 297

Page  16 / 297

Page  17 / 297

Page  18 / 297

Page  19 / 297

Page  20 / 297

Page  21 / 297

Page  22 / 297

Page  23 / 297

Page  24 / 297

Page  25 / 297

Page  26 / 297

Page  27 / 297

Page  28 / 297

Page  29 / 297

Page  30 / 297

Page  31 / 297

Page  32 / 297

Page  33 / 297

Page  34 / 297

Page  35 / 297

Page  36 / 297

Page  37 / 297

Page  38 / 297

Page  39 / 297

Page  40 / 297

Page  41 / 297

Page  42 / 297

Page  43 / 297

Page  44 / 297

Page  45 / 297

Page  46 / 297

Page  47 / 297

Page  48 / 297

Page  49 / 297

Page  50 / 297

Page  51 / 297

Page  52 / 297

Page  53 / 297

Page  54 / 297

Page  55 / 297

Page  56 / 297

Page  57 / 297

Page  58 / 297

Page  59 / 297

Page  60 / 297

Page  61 / 297

Page  62 / 297

Page  63 / 297

Page  64 / 297

Page  65 / 297

Page  66 / 297

Page  67 / 297

Page  68 / 297

Page  69 / 297

Page  70 / 297

Page  71 / 297

Page  72 / 297

Page  73 / 297

Page  74 / 297

Page  75 / 297

Page  76 / 297

Page  77 / 297

Page  78 / 297

Page  79 / 297

Page  80 / 297

Page  81 / 297

Page  82 / 297

Page  83 / 297

Page  84 / 297

Page  85 / 297

Page  86 / 297

Page  87 / 297

Page  88 / 297

Page  89 / 297

Page  90 / 297

Page  91 / 297

Page  92 / 297

Page  93 / 297

Page  94 / 297

Page  95 / 297

Page  96 / 297

Page  97 / 297

Page  98 / 297

Page  99 / 297

Page 100 / 297

Page 101 / 297

Page 102 / 297

Page 103 / 297

Page 104 / 297

Page 105 / 297

Page 106 / 297

Page 107 / 297

Page 108 / 297

Page 109 / 297

Page 110 / 297

Page 111 / 297

Page 112 / 297

Page 113 / 297

Page 114 / 297

Page 115 / 297

Page 116 / 297

Page 117 / 297

Page 118 / 297

Page 119 / 297

Page 120 / 297

Page 121 / 297

Page 122 / 297

Page 123 / 297

Page 124 / 297

Page 125 / 297

Page 126 / 297

Page 127 / 297

Page 128 / 297

Page 129 / 297

Page 130 / 297

Page 131 / 297

Page 132 / 297

Page 133 / 297

Page 134 / 297

Page 135 / 297

Page 136 / 297

Page 137 / 297

Page 138 / 297

Page 139 / 297

Page 140 / 297

Page 141 / 297

Page 142 / 297

Page 143 / 297

Page 144 / 297

Page 145 / 297

Page 146 / 297

Page 147 / 297

Page 148 / 297

Page 149 / 297

Page 150 / 297

Page 151 / 297

Page 152 / 297

Page 153 / 297

Page 154 / 297

Page 155 / 297

Page 156 / 297

Page 157 / 297

Page 158 / 297

Page 159 / 297

Page 160 / 297

Page 161 / 297

Page 162 / 297

Page 163 / 297

Page 164 / 297

Page 165 / 297

Page 166 / 297

Page 167 / 297

Page 168 / 297

Page 169 / 297

Page 170 / 297

Page 171 / 297

Page 172 / 297

Page 173 / 297

Page 174 / 297

Page 175 / 297

Page 176 / 297

Page 177 / 297

Page 178 / 297

Page 179 / 297

Page 180 / 297

Page 181 / 297

Page 182 / 297

Page 183 / 297

Page 184 / 297

Page 185 / 297

Page 186 / 297

Page 187 / 297

Page 188 / 297

Page 189 / 297

Page 190 / 297

Page 191 / 297

Page 192 / 297

Page 193 / 297

Page 194 / 297

Page 195 / 297

Page 196 / 297

Page 197 / 297

Page 198 / 297

Page 199 / 297

Page 200 / 297

Page 201 / 297

Page 202 / 297

Page 203 / 297

Page 204 / 297

Page 205 / 297

Page 206 / 297

Page 207 / 297

Page 208 / 297

Page 209 / 297

Page 210 / 297

Page 211 / 297

Page 212 / 297

Page 213 / 297

Page 214 / 297

Page 215 / 297

Page 216 / 297

Page 217 / 297

Page 218 / 297

Page 219 / 297

Page 220 / 297

Page 221 / 297

Page 222 / 297

Page 223 / 297

Page 224 / 297

Page 225 / 297

Page 226 / 297

Page 227 / 297

Page 228 / 297

Page 229 / 297

Page 230 / 297

Page 231 / 297

Page 232 / 297

Page 233 / 297

Page 234 / 297

Page 235 / 297

Page 236 / 297

Page 237 / 297

Page 238 / 297

Page 239 / 297

Page 240 / 297

Page 241 / 297

Page 242 / 297

Page 243 / 297

Page 244 / 297

Page 245 / 297

Page 246 / 297

Page 247 / 297

Page 248 / 297

Page 249 / 297

Page 250 / 297

Page 251 / 297

Page 252 / 297

Page 253 / 297

Page 254 / 297

Page 255 / 297

Page 256 / 297

Page 257 / 297

Page 258 / 297

Page 259 / 297

Page 260 / 297

Page 261 / 297

Page 262 / 297

Page 263 / 297

Page 264 / 297

Page 265 / 297

Page 266 / 297

Page 267 / 297

Page 268 / 297

Page 269 / 297

Page 270 / 297

Page 271 / 297

Page 272 / 297

Page 273 / 297

Page 274 / 297

Page 275 / 297

Page 276 / 297

Page 277 / 297

Page 278 / 297

Page 279 / 297

Page 280 / 297

Page 281 / 297

Page 282 / 297

Page 283 / 297

Page 284 / 297

Page 285 / 297

Page 286 / 297

Page 287 / 297

Page 288 / 297

Page 289 / 297

Page 290 / 297

Page 291 / 297

Page 292 / 297

Page 293 / 297

Page 294 / 297

Page 295 / 297

Page 296 / 297

Page 297 / 297