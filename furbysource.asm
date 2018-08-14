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

;ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
ÍÍÍÍÍ»
;²
²
;²       SPC81A Source Code   (Version 25)
²
;²
²
;²       Written by: Dave Hampton  /  Wa___ Schulz
²
;²       Date:       July 30, 1998
²
;²
²
;²       Copyright (C) 1996,1997,1998 by Sounds Amazing!
²
;²       All rights reserved.
;ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
ÍÍÍÍÍ¼
;
;
;************************************************************************


;  remember    SBC     if there is a barrow carry is CLEARED
;  also SBC    if the two numbers are equal you still get a negative
result

;
;
;ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
ÍÍÍÍÍ»
;²   MODIFICATION LIST :
²
;

; Furby29/30/31/32
;     Final testing for shipment of code on 8/2/98.
;     Tables updates, __tor speed updates, wake up/name fix
;     sequential tables never getting first entry,fixed.
;     New diag5.asm, Light3.asm (if light osc stalls it wont hang
system).
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
;     Adds four new easter eggs,BURP ATTACK, SAY NAME, TWINKLE SONG,
;     and ROOSTER LOVES YOU. Also add new names.
;
;
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
A-1
; Release 3

;; File "testR3a"

; 1. Light sensor has a hysteresis point of continually triggering
sensor.
; 2. Light sensor decrements two instead of one on hungry counter.
; 3. Diagnos______de for light sensor wont trigger very easily.
; 4. When a furby receives the I.R. sleep command he sends the same
command
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
; 7. Add new light routine.

; 8. Change hide and seek egg to light,light,light,tummy.

; 9. Change sick/hungry counter so that it can only get so sick and
;    not continue down to zero. (MAX_SICK)

;10. In diagnostics, motor position test ,,,, first goes forward
continuously
;    until the front switch is pressed, then goes reverse continuously
;    until the front switch is pressed again, and then does normal
position
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


;²
²
;ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
ÍÍÍÍÍ¼
;************************************************************************
;************************************************************************
;************************************************************************
;************************************************************************
;************************************************************************
A-2
; Actual numeric value for TI pitch control

;  bit 7 set = subtract value from current course value
;        clr = add value to current course value
;  bit 6 set = select music pitch table
;        clr = select normal speech pitch table
;  bit 0-5 value to change course value (no change = 0)

; A math routine in 'say_0' converts the vavlue for + or -
; if <80 and then subtracts from 80 to get the minus version of 00
; ie. if number is 70 then TI gets sent 10 (which is -10)
; If number is 80 or > 80 then get sent literal as positive.

; NOTE: MAX POSITIVE IS 8F (+16 from normal voice of 00)
;       MAX NEGATIVE is 2F (-47 from normal voice of 00)

;This is a difference of 80h - 2Fh or 51h


; 8Fh is hi voice  (8f is very squeeeeeke(
; 2Fh lo voice ( very low)


; The math routine in 'Say_0' allows a +-decimal number in the speech
table.
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

;;;; we converted to a random selection table, but since all voice
tables
;    use the equates plus some offset, we ____ the change in the SAY_0
;    routine. We always assign voice 3 which is the lowest, and based on
;    the random power up pitch selection, the ram location 'Rvoice'
holds
;    the number to add to the voice+offset received from the macro
table.

Voice EQU   Voice3        ;pitch (choose Voice1, Voice2,
Voice3)(voice2=norm)

; Select Voice3 since it is the lowest and then add the difference to
get
; Voice2 or Voice3. Here we assign that different to an equate to be
; used in the voice table that is randomly selected on power up.

S_voice1  EQU  18 ;Voice3 + 18d = Voice1
S_voice2  EQU  09 ;Voice3 + 09d = Voice2
A-3
S_Voice3  EQU  0  ;Voice3 + 00d = Voice3



;************************************************************************

; Motor speed pulse width :
; Motor_on = power to motor, Motor_off is none.


Mpulse_on   EQU    16  ;
Mpulse_off  EQU    16  ;


Cal_pos_fwd EQU    134  ;calibration switch forward direction
Cal_pos_rev EQU    134  ;calibration switch forward direction

;************************************************************************
;************************************************************************
;************************************************************************
;************************************************************************
;************************************************************************
;
;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                       PORTS                         ³
;³ SPC40A has : 16 I/O pins                            ³
;³ PORT_A 4 I/O pins  0-3                              ³
;³ PORT_C 4 I/O pins  0-3                              ³
;³ PORT_D 8 I/O pins  0-7                              ³
;³                                                     ³
;³                         RAM                         ³
;³                                                     ³
;³ SPC40A has : 128 bytes of RAM                       ³
;³ from $80 - $FF                                      ³
;³                                                     ³
;³                         ROM                         ³
;³ SPC40A has :                                        ³
;³ BANK0 user ROM from $0600 - $7FFF                   ³
;³ BANK1 user ROM from $8000 - $FFF9                   ³
;³                                                     ³
;³                                                     ³
;³                         VECTORS                     ³
;³ NMI   vector  $7FFA / $7FFB                         ³
;³ RESET vector  $7FFC / $7FFD                         ³
;³ IRQ   vector  $7FFE / $7FFF                         ³
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                       PORTS                         ³
;³ SPC120A has : 17 I/O pins                           ³
;³ PORT_A 4 I/O pins  0-3                              ³
;³ PORT_B 4 I/O pins  0,1,2,4,5                        ³
;³ PORT_C 4 I/O pins  0-3 input only                   ³
;³ PORT_D 8 I/O pins  0-7                              ³
;³                                                     ³
;³                         RAM                         ³
;³ SPC120A has : 128 bytes of RAM                      ³
;³ from $80 - $FF                                      ³
;³                                                     ³
;³                         ROM                         ³
;³ SPC120A has :                                       ³
A-4
;³ BANK0 user RO       $0600 - $7FFA                   ³
;³ BANK1 user RO       $8000 - $FFFF                   ³
;³ BANK2 user RO       $10000 - $17FFF                 ³
;³ BANK3 user RO       $1A000 - $1FFFF                 ³
;³                                                     ³
;³                                                     ³
;³                         VECTORS                     ³
;³ NMI   vector  $7FFA / $7FFB                         ³
;³ RESET vector  $7FFC / $7FFD                         ³
;³ IRQ   vector  $7FFE / $7FFF                         ³
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ



; unuseable areas in rom

;SPC40A:     8000H ÄA  DFFFH should be skiped (Dummy area)
;  bank 0 = 600 - 7FFA
;  bank 1 = 8000 - DFFF reserved , start @ E000 - FFFA

;SPC80A:     10000H ÄÄ 13FFFH should be skiped (Dummy area)
;  bank 0 = 600 - 7FFA
;  bank 1 = 8000 - FFFA
;  bank 2 = 10000 - 13FFF reserved , start at 14000 - 17FFF

;SPC120A: ;SPC120A: 18000H ÄÄ 19FFFH should be skiped (Dummy area)
;  bank 0 = 600 - 7FFA
;  bank 1 = 8000 - FFFA
;  bank 2 = 10000 - 17FFF
;  bank 3 = 18000 - 19FFF reserved, start at 1A000 - 1FFFA

;SPC256A: ;SPC256A: Non dummy area

;SPC512A: ;SPC512A: Non dummy area

;************************************************************************

.CODE
.SYNTAX  6502
.LINKLIST
.SYMBOLS

;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ PORT DIRECTION CONTROL REGISTER
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
Ports_dir       EQU     00     ; (write only)
;
; (4 I/O pins) controlled with each bit of this register
; you can't control each pin separately, only as a nibble
; 0 = input / 1 = output
;
; 7      6       5       4       3       2       1       0        (REGISTER
BITS)
; D      D       C       C       B       B       A       A        (PORT)
; 7654   3210    7654    3210    7654    3210    7654    3210     (PORT BITS)
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
ÄÄÄÄÄ

; ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ PORT CONFIGURATION CONTROL REGISTER
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
A-5
;                 based on if the port pin is input or output
;
Ports_con       EQU     01     ; (write only)
;
; (4 I/O pins) controlled with each bit of this register
; 7      6       5       4       3       2       1       0        (REGISTER
BITS)
; D      D       C       C       B       B       A       A        (PORT)
; 7654   3210    7654    3210    7654    3210    7654    3210     (PORT BITS)

; port_a INPUTS can be either:
; 0 = float   1 = pulled high

; port_a OUTPUTS can be either:
; 0 = buffer   1 = upper (4) bits Open drain Nmos (sink)
;                 lower (4) bits Open drain Nmos (sink)
;
;
; port_b INPUTS can be either:
; 0 = float   1 = pulled low

; port_b OUTPUTS can be either:
; 0 = buffer   1 = upper (4) bits Open drain Nmos (sink)
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

;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
ÄÄÄÄÄ

;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ I/O PORTS
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

Port_A           EQU     02H    ; (read/write) for TI & speech recogn
CPU's
Data_D0            EQU   01H  ;bit 0 data nible port
Data_D1            EQU   02H  ;
Data_D2            EQU   04H  ;
Data_D3            EQU   08H  ;

Port_B             EQU   03H  ;b0/b1 = I/O  b4/b5 = inp only
TI_init            EQU   01H  ;B0 - TI reset control
TI_CTS             EQU   02H  ;B1 - hand shake to TI
IR_IN         EQU  10H   ;B4 - I.R. Recv data
TI_RTS             EQU   20H  ;B5 - TI wants data

Port_C           EQU     04H    ; (read/write)
Motor_cal     EQU  01H   ;C0 - lo when mo___ crosses switch
Pos_sen            EQU   02H  ;C1 - moto______ical sensor (intt C1)
Touch_bak     EQU  04H   ;C2 - back touch
Touch_frnt    EQU  08H   ;C3 - front touch
A-6
