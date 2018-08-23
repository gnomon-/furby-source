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
;²       Written by: Dave Hampton  /  Wayne Schulz
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
A-7
A-8
A-9
A-10
A-11
A-12
A-13
A-14
A-15
A-16
A-17
A-18
Stacktop     EQU   FFH   ;Stack Top


;**********************************************************************
****
;**********************************************************************
****
;**********************************************************************
****
;**********************************************************************
****

        ORG      00H
        BLKW     300H.00H        ;Fill 0000 ÄÄÄ 05FFH- 00

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                                                      ³
;³       P R O G R A M     S T A R T S   H E R E        ³
;³                                                      ³
;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

        ORG      0600H

RESET:


      Include          Wake2.asm    ;asm file


;********* end Tracker


; For power on test, WE only clear ram to E9h and use EAh for a
; messenger to the warm boot routine. We always clear ram and initialize
; registers on power up, but if it is a warm boot then read E_PROM
; and setup ram locations. Location EAH is set or cleared duri___ power
up
; and then the stack can use it during normal run.

; Clear RAM to 00H
;----------------------------------------------------------------------
----
        LDA     #00H            ; data for fill
        LDX     #E9H          ; start at ram location

RAMClear:
        STA     00,X            ; base 00, offset x
        DEX                     ; next ram location
        CPX     #7FH           ; check for end
        BNE     RAMClear        ; branch, not finished
                                ; fill done
A-19
;----------------------------------------------------------------------
----

Main:

InitIO:
      LDA   #01         ;turn DAC on
      STA   DAC_ctrl    ;DAC control

      LDA   #Port_def   ;set direction control
      STA   Ports_dir   ;load reg

      LDA   #Con_def    ;set configuration
      STA   Ports_con   ;load reg

      LDA   #00         ;set for bank 0
      STA   Bank        ;set it
      LDA   #00H        ;disable wakeup control
      STA   Wake_UP           ;
      LDA   #00h        ;disable sleep control
      STA   Sleep       ;set dont care

        LDA     #Intt_dflt    ;Initialize timers, etc.
        STA     Interrupts    ;load reg

        LDA     #00H          ;set timer mode
        STA     TMA_CON       ;set reg
      LDA   #TimeA_low  ;get preset timer for interrupts
      STA   TMA_LSB            ;load

      LDA   #TimeA_hi   ;get hi byte for preset
      STA   TMA_MSB           ;load it

      LDA   #TimeB_low  ;get preset timer for interrupts
      STA   TMB_LSB           ;load
      LDA   #TimeB_hi   ;get hi byte for preset
      STA   TMB_MSB           ;load it

      LDA   #C0h        ;preset status for motors off
      STA   Stat_3            ;
      LDA   #00h        ;init ports
      STA   Port_A            ;output

      LDA   #33H        ;init ports
      STA   Port_B_Image      ;ram image
      STA   Port_B            ;output

      LDA   #0_H        ;init ports
      STA   Port_C            ;output

      LDA   #D0H        ;init ports
      STA   Port_D_Image      ;ram image
      STA   Port_D            ;output

      LDA   #FFh        ;milisec timer reload value
      STA   Mili_sec    ;also preset IRQ timer

        CLI             ;Enable IRQ
A-20
A-21
A-22
A-23
A-24
A-25
A-26
A-27
A-28
A-29
A-30
A-31
A-32
A-33
A-34
A-35
A-36
A-37
A-38
A-39
A-40
A-41
A-42
A-43
A-44
A-45
A-46
A-47
A-48
A-49
A-50
A-51
A-52
A-53
A-54
A-55
A-56
A-57
A-58
Simon_snd_lo	EQU	#B0h	;using macro 432 for simon chooses "sound
Simon_snd_hi	EQU	#01h	;hi byte adrs 432 = 1B0h

Simon_lght_lo	EQU	#B1h	;using macro 433 for simon chooses "light
Simon_lght_hi	EQU	#01h	;hi byte adrs 433 = 1B1h

Skeyfrnt_lo	EQU	#0Fh	;using macro 15 for user feed back
Skeyfrnt_hi	EQU	#00h	;use for "front"

Skeybck_lo	EQU	#B2h	;using macro 434 for user feed back
Skeybck_hi	EQU	#01h	;use for "back"

Skeylght_lo	EQU	#B3h	;using macro 435 for user feed back
Skeylght_hi	EQU	#01h	;use for "light"

Skeysnd_lo	EQU	#B4h	;using macro 436 for user feed back
Skeysnd_hi	EQU	#01h	;use for "sound"

Simonlost_lo	EQU	#D8h	; lost game is macro 472
Simonlost_hi	EQU	#01


; Available ram not in use during this game

;HCEL_LO	Counter of which sensor were on

;HCEL_HI	Random play ram 1
;BIT_CT		Random play ram 2
;Task_ptr	Random play ram 3
;Bored_count	Random play ram 4

;TEMP5		Random save ram 1  ( was TMA_INT ) TEMP5 used in
'RAN_SEQ'
;Temp_ID2	Random save ram 2
;Temp_ID	Random save ram 3
;Learn_temp Random save ram 4


Game_simon:
; do delay before start of game

      LDA	#Simondelay_lo	;get macro lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	#Simondelay_hi	;get macro hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JSR	Get_macro	;go start motor/speech
      JSR	Notrdy		;Do / get status for speech and motor
      
      LDA Name		;current setting for table offset
      CLC
      ROL	A		;2's comp
      TAX
      LDA	Name_table,X	;get lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	Name_table,X	;get hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JSR	Get_macro	;go start motor/speech
      JSR	Notrdy		;Do/ get  status for speech and motor

A-59
      LDA	#Listen_me_lo	;get macro lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	#Listen_me_hi	;get macro hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JSR	Get_macro	;go start motor/speech
      JSR	Notrdy		;Do / get  status for speech and motor

      LDA	#Simondelay_lo	;get macro lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	#Simondelay_hi	;get macro hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JSR	Get_macro	;go start motor/speech
      JSR	Notrdy		;Do / get  status for speech and motor

      LDA	#04		;number of sensors in 1st game
GS_rentr:
      STA	HCEL_LO		;load counter
      STA	IN_DAT		;save for later use
      JSR	Simon_random	;go load 2 grps of 4 ram locations
Simon1:
            LDA	HCEL_HI		;get 1st ram location
      JSR	Simon_sensor	;go to speech
      JSR	Rotate_play	;get next 2 bits for sensor choice
      DEC	IN_DAT		;-1 (number of sensors played this game)
      BNE	Simon1		;loop til all speech done

      JSR	Recover_play	;reset random rams
      LDA	#GameT_reload	;reset timer
      STA	Bored_timer	;set
      LDA	#00
      STA	Stat_4		;clear all sensors
      LDA	HCEL_LO		;go counter
      STA	IN_DAT		;reset it
Simon2:
      JSR	Test_all_sens	;go check all sensors
      LDA	Stat_4		;get em
      BNE	Simon3		;jump if any triggered
      JSR	Simon_timer	;go check for timeout
      LDA	Bored_timer	;
      BNE	Simon2		;loop if not
      JMP	Simon_over	;bailout if 0
Simon3:
; do to lack of time I resort to brute force ... YUK....
      LDA	Stat_4		;get which sensor
      CMP	#08h		;front sw
      BNE	Simon3a		;jump if not
      LDA	#Skeyfrnt_lo	;get macro lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	#Skeyfrnt_hi	;get macro hi byte
      JMP	Simon3dn	;go speak it

      CMP	#04h		;light
A-60
      BNE	Simon3c		;jump if not
      LDA	#Skeylght_lo	;get macro lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	#Skeylght_hi	;get macro hi byte
      JMP	Simon3dn	;go speak it
Simon3c:
      CMP	#01h		;sound
      BNE	Simon3d		;jump if not
      LDA	#Skeysnd_lo	;get macro lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	#Skeysnd_hi	;get macro hi byte
      JMP	Simon3dn	;go speak it
Simon3d:
      CMP	#Do_invert	;?
      BEQ	Simon3e		;jump if is invert
      LDA	#00		;
      STA	Stat_4		;clear sensor flags
      JMP	Simon2		;ignore all other sensors loop up
Simon3e:
      JMP	Simon_over	;bail out if is

Simon3dn:
      STA	Macro_Hi	;save for macro call
      JSR	Get_macro	;go start motor/speech
      JSR	Notrdy		;Do / get  status for speech and motor

            LDA	HCEL_HI		;get  1st ram location
      AND	#03		;bit 0 & 1
      TAX			;point to interpret table entry
      LDA	Simon_convert,X	;translat game to sensors
      CMP	Stat_4		;ck for correct sensor
      BNE	Simon_lost	;done if wrong
      LDA	#00
      STA	Stat_4		;clear all sensors
      JSR	Rotate_play	;get next 2 bits for sensor choice
      DEC	IN_DAT		;-1 (number of sensors played this game)
      BNE	Simon2		;loop til all sensors done
      JSR	Simon_won	;game won
      JSR	Recover_play	;reset random rams
      INC	HCEL_LO		;increase number of sensors in next game
      CLC
      LDA	HCEL_LO		;get current
      STA	IN_DAT		;reset game sensor counter
      SBC	#16		;ck if max number of sensors
      BCS	Simon4		;
      JMP	Simon1		;loop up
Simon4:
      LDA	#16		;set to max
      JMP	GS_rentr	;start next round



;;;;;; Simon subroutines

Simon_lost:
;	LDA	Stat_4		;ck for invert sw to end game
;	CMP	#Do_invert	;?
;	BEQ	Simon_over	;bail out if is

      LDA	#Simonlost_lo	;get macro lo byte	
A-61
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	#Simonlost_hi	;get macro hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JSR	Get_macro	;go start motor/speech
      JSR	Notrdy		;Do / get  status for speech and motor
      JMP	Game_simon	;start at beginning

Simon_won:
      LDA	HCEL_LO		;game number (how many steps)
      CLC
      ROL	A		;2's offsett for speech win table
      TAX			;
      LDA	Simon_won_tbl,X	;get lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      INX			;
      LDA	Simon_won_tbl,X	;get hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JSR	Get_macro	;go start motor/speech
      JSR	Notrdy		;Do / get  status for speech and motor
      RTS


Rotate_play:
      ROR	Bored_count	;shfl to carry
      ROR	Task_ptr	;carry & shfl to carry
      ROR	BIT_CT		;carry & shfl to carry
      ROR	HCEL_HI		;carry & shfl to carry throw away lo bit
      ROR	Bored_count	;shfl to carry
      ROR	Task_ptr	;carry & shfl to carry
      ROR	BIT_CT		;carry & chfl to carry
      ROR	HCEL_HI		;carry & shfl to carry throw away lo bit
      RTS			;

Recover_play:
      LDA	TEMP5		;recover random data
      STA	HCEL_HI
      LDA	Temp_ID2
      STA	BIT_CT
      LDA	Temp_ID
      STA	Task_ptr
      LDA	Learn_temp
      STA	Bored_count
      RTS			;
;
Simon_over:
      JSR	Clear_all_gam	;go clear all status, cancel game
      LDA	#00		;
      STA	Task_ptr	;reset for normal use
      JMP	End_all_games	;done go say "me done"
;
Simon_sensor:
      AND	#03h		;get sensor
      CLC
      ROL	A		;2's offset
      TAX			;offset
      LDA	Psimon_table,X	;
      STA	Macro_Lo	;
      INX			;
      LDA	Psimon_table,X	;
      STA	Macro_Hi	;save hi byte of Macro table entry
A-62
      JSR	Get_macro	;go start motor/speech
      JSR	Notrdy		;Do / get  status for speech and motor
      RTS			;
;
Simon_delay:
      LDA	#Simondelay_lo	;get macro lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	#Simondelay_hi	;get macro hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JSR	Get_macro	;go start motor/speech
      JSR	Notrdy		;Do / get  status for speech and motor
      RTS			;
;
Simon_random:
      JSR	Random		;get random number (0-255)
      STA	TEMP5		;   "
      STA	HCEL_HI
      JSR	Random		;get random number (0-255)
      STA	Temp_ID2	;   "
      STA	BIT_CT
      JSR	Random		;get random number (0-255)
      STA	Temp_ID	;   "
      STA	Task_ptr
      JSR	Random		;get random number (0-255)
      STA	Learn_temp	;   "
      STA	Bored_count
      RTS
;
Simon_timer:
      LDA	Milisec_flag	;if >0 then 742 mili seconds have passed
      BEQ	Simon_tdn	;bypass if 0
      LDA	#00		;clear it
      STA	Milisec_flag	;reset

      LDA	Bored_timer	;get current timer * 742mSec sec
      BEQ	Simon_tdn	;do nothing if -
      DEC	Bored_timer	;-1
Simon_tdn:
      RTS			;
;
Psimon_table:
      DW	430	;front switch  ( 00 )
      DW	431	;back switch   ( 01 )
      DW	433	;sound sensor  ( 11 ) (lt & snd swaped in table)
      DW	432	;light sensor  ( 10 )
;
Simon_convert:	;converts game table to sensor table
      DB	08h	;front sw
      DB	10h	;back sw
      DB	04h	;light
      DB	01h	;sound
;
Simon_won_tbl:	;for each game won there is a macro (or re-use them)
      DW	72	; 0 (not used,,,, place holder)
      DW	72	; 1 (not used,,,, place holder)
      DW	72	; 2 (not used,,,, place holder)
      DW	72	; 3 (not used,,,, place holder)

      DW	72	; 4  (1st game has 4 sensors, each game adds one)
      DW	72	; 5
A-63
      DW	72	; 6
      DW	72	; 7
      DW	380	; 8
      DW	380	; 9
      DW	380	; 10
      DW	380	; 11
      DW	471	; 12
      DW	471	; 13
      DW	471	; 14
      DW	471	; 15
      DW	439	; 16

;

End_all_games:	;when any game ends, they jump here and say done

Saygamdn_lo	EQU	#D9h	;using macro 473 for game over speech
Saygamdn_lo	EQU	#01h	;


      LDA	#Bored_reld	;reset bored timer
      STA	Bored_timer	;

      LDA	#Saygamdn_lo	;get macro lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	#Saygamdn_hi	;get macro hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JMP	Start_macro	;go set group/table pointer for motor & spch

;**************************************************************

;Burp attack egg

Burpsnd_lo	EQU	#D6h	;using macro 470 for user feed back
Burpsnd_hi	EQU	#01h	;


Game_Burp:

      JSR	Clear_all_gam

      LDA	#Bored_reld	;reset bored timer
      STA	Bored_timer	;

      LDA	#Burpsnd_lo	;get macro lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	#Burpsnd_hi	;get macro hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JMP	Start_macro	;go set group/table pointer for motor & spch




;
;**************************************************************

;easter egg says NAME

Game_name:
A-64
      JSR	Clear_all_gam

      LDA	#Bored_reld	;reset bored timer
      STA	Bored_timer	;

      LDA	Name		;current setting for table offset
      CLC
      ROL	A		;2's comp
      TAX
      LDA	Name_table,X	;get macro lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	Name_table,X	;get macro hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JMP	Start_macro	;go set group/table pointer for motor & spch
;
;**************************************************************

;Twinkle song egg

; When song is complete, if both front and back switches are pressed
; we goto deep sleep. That means only the invert can wake us up, not
; the invert switch.


Twinklesnd_lo	EQU	#D5h	;using macro 469
Twinklesnd_hi	EQU	#01h	;

Sleep_lo	EQU	#A6h	;using macro 166 ()before going to sleep)
Sleep_hi	EQU	#00h	;

Game_twinkle:

      JSR	Clear_all_gam
      LDA	#03		;song counter
      STA	HCEL_LO		;set
Gtwnk:
      DEC	HCEL_LO		;-1
      LDA	Stat_2		;Get system clear done flags
      AND	#Not_tch_ft	;clear previously inverted flag
      AND	#Not_tch_bk	;clear previously inverted flag
      STA	Stat_2		;update

      LDA	#Bored_reld	;reset bored timer
      STA	Bored_timer	;

      LDA	#Twinklsnd_lo	;get macro lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	#Twinklsnd_hi	;get macro hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JSR	Get_macro	;go start motor/speech
      JSR	Notrdy		;Do / get  status for speech and motor
      JSR	Test_all_sens	;get status
      JST	Test_all_sens	;get status 2nd time for debounce
      LDA	Stat_4		;switch status
      AND	#18h		;isolate front and back switches
      CMP	#18h		;
      BEQ	Start_sleep	;if both switches pressed, goto sleep
      LDA	HCEL_LO		;get song loop counter
      BNE	Gtwnk		;loop
A-65
      JMP	Idle		;not so egg complete

Start_sleep:
      LDA	#Sleep_lo	;get macro lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	#Sleep_hi	;get macro hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JSR	Get_macro	;go start motor/speech
      JSR	Notrdy		;Do / get  status for speech and motor
      LDA	#11h		;set deep sleep mode
      STA	Deep_sleep	;
      JMP	GoToSleep	;nity-night
;
;**************************************************************

;Rooster loves you  egg

Roostersnd_lo	EQU	#D4h	;using macro 468
Roostersnd_hi	EQU	#01h	;


Game_rooster:

      JSR	Clear_all_gam
      
      LDA	#Bored_reld	;reset bored timer
      STA	Bored_timer	;
      
      LDA	#Roostersnd_lo	;get macro lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	#Roostersnd_hi	;get macro hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JMP	Start_macro	;go set group/table pointer for motor & spch

;**************************************************************
;

; If a game requires sensor input without triggering the normal
; sensor cycle for speech, then this rtn will check all sensors for
; change and the calling game can check for the appropriate trigger
; DO NOT USE I.R. SENSOR SINCE ITS RAM LOCATIONS ARE USED IN GAMES

Test_all_sens:
      JSR	Get_back	;
      JSR	Get_Tilt	;
      JSR	Get_invert	;
      JSR	Get_front	;
      JSR	Get_light	;
      JSR	Get_sound	;
      JSR	Get_feed	;
      RTS			;back to game



;**************************************************************
;**************************************************************
;**************************************************************

;***** Side??all switch triggers when ball falls off center and I/O goes
A-66
hi.

CK_tilt:		;tilt sensor
      JSR	Get_Tilt	;go ck for sensor trigger
      BCS	Normal_tilt	;go fini normal spch/motor table
      JMP	Idle		;no request

Get_Tilt:	;this is the subroutine entry point.
      LDA	Stat_2		;Get system
      AND	#Not_bside	;clear previously on side flag
      STA	Stat_2		;update
Side_out:
      CLC			;clear indicates no request
      RTS			;

Do_bside:
      LDA	Stat_2		;system
      AND	#Bside_dn	;ck if previously done
      BNE	Side_out	;jump if was
      LDA	Stat_2		;get system
      ORA	#Bside_dn	;flag set ,only execute once
      STA	Stat_2		;update system

      LDA	Stat_4		;game mode status
      ORA	#Do_tilt	;flag sensor is active
      STA	Stat_4		;update
      SEC			;carry set indicates sensor is triggered
      RTS			;

Normal_tilt:	;Idle rtn jumps here to complete speech/motor table


;;;;;;;  also for testing, when tilt is triggered, it resets all
;        easter egg routines to allow easy entry of eggs.


      JSR	Clear_all_gam	;



;****************************************************************

      JSR	Lifre		;go tweek health/hungry counters
      BCS	More_tilt	;if clear then do sensor else bail
      JMP	Idle		;done
More_tilt:

;****************************************************************

      LDA	#Tilt_split	;get random/sequential split
      STA	IN_DAT		;save for random routine

      LDX	#Seq_tilt	;get how many sequential selections
      LDA	#Ran_tilt	;get number of random selections
      JSR	Ran_seq		;go decide random/sequential
A-67
      LDX	Sensor_timer	;get current for training subroutine

      BCS	Tilt_ran	;Random mode when carry SET

      LDA	Sensor_timer	;ck if timed out since last action
      BEQ	Tilt_reset	;yep

      LDA	Tilt_count	;save current
      STA	BIT_CT		;temp store

      INC	Tilt_count	;if not then next table entry
      LDA	Tilt_count	;get
      CLC
      SBC	#Seq_tilt-1	;ck if > assignment
      BCC	Tilt_side	;jump if <
      LDA	#Seq_tilt-1	;dont inc off end
      STA	Tilt_count	;
      JMP	Tilt_side	;do it
Tilt_reset:
      LDA	#00		;reset to 1st entry of sequential
      STA	BIT_CT		;temp store
      STA	Tilt_count	;
Tilt_side:
      LDA	#Global_time	;get timer reset value
      STA	Sensor_timer	;reset it
      LDA	BIT_CT		;Acc holds value for subroutine

Tilt_ran:
      STA	IN_DAT		;save decision
      LDA	#Tilt_ID	;which ram location for learned word count (offset)
      JSR	Start_learn	;go record training info
      LDA	IND_DAT		;get decision

      JSR	Decid_age	;do age calculation for table entry
      LDX	TEMP0		;age offset
      LDA	Tilt_S1,X	;get lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      INX			;
      KDA	Tilt_S1,X	;get hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JMP	Start_macro	;go set group/table pointer for motor & spch
;
;
;****************************************************************
;****************************************************************
;****************************************************************
;
;
;
;***** Inverted ball switch triggers when ball touches top and I/O goes hi.

Ck_invert:			; upside down sense

      JSR	Get_invert	;go ck for sensor trigger
      BCS	Normal_invert	;go fini normal spch/motor table
      JMP	Idle		;no request

Get_invert:	;this is the subroutine entry point
A-68
      LDA   Port_D            ;get I/O
      AND	#Ball_invert	;ck if we upside down

      BNE	Do_binvrt	;jump if inverted (hi)

      LDA	Stat_2		;Get system
      AND	#Not_binvrt	;clear previously inverted flag
      STA	Stat_2		;update
Invrt_out:
      CLC			;clear carry indicates no sensor change
      RTS			;

Do_binvrt:
      LDA	Stat_2		;get system
      AND	#Binvrt_dn	;ck if prev done
      BNE	Invrt_out	;jump if was
      LDA	Stat_2		;get system
      ORA	#Binvrt_dn	;flag set ,only execute once
      STA	Stat_2		;update system

      LDA	Stat_4		;game mode status
      ORA	#Do_invert	;flag sensor is active
      STA	#Do_invert	;flag sensor is active
      STA	Stat_4		;update
      SEC			;set indicates sensor is triggered
      RTS			;

Normal_invert:


;******************************************************************

      JSR	Life		;go tweek health/hungry counters
      BCS	More_invert	;if clear then do sensor else bail
      JMP	Idle		;done
More_invert:

;******************************************************************


      LDA	#Invert_split	;get random/sequential split
      STA	IN_DAT		;save for random routine

      LDX	#Seq_invert	;get how many sequential selections
      LDA	#Ran_invert	;get number of random selections
      JSR	Ran_seq		;go decide random/sequential

      LDX	Sensor_timer	;get current  for training subroutine

      BCS	Invrt_rnd	;Random mode when carry SET

      LDA	Sensor_timer	;ck if timed out since last action
      BEQ	Invrt_reset	;yep

      LDA	Invrt_count	;save current
      STA	BIT_CT		;temp store

      INC	Invrt_count	;if not then next table entry
      LDA	Invrt_count	;get
A-69
      CLC
      SBS	#Seq_invert-1	;ck if > assignment
      BCC	Invert_set	;jump if <
      LDA	#Seq_invert-1	;done inc off end
      STA	Invrt_count	;
      JMP	Invrt_set	;do it
Invrt_rest:
      LDA	#00		;reset to 1st entry of sequential
      STA	BIT_CT		;temp store
      STA	Invrt_count	;
Invrt_set:
      LDA	#Global_time	;get timer reset value
      STA	Sensor_timer	;reset it
      LDA	BIT_CT		;speech to call

Invrt_rnd:

      STA	IN_DAT		;save decision
      LDA	#Invert_ID	;which ram location for learned word count (offset)
      JSR	Start_learn	;go record training info
      LDA	IN_DAT		;get back word to speak

      JSR	Decid_age	;do age calculation for table entry
      LDX	TEMP0	;age offset
      LDA	Invrt_S1,X	;get lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      INX			;
      LDA	Invrt_S1,X	;get hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JMP	Start_macro	;go set group/table pointer for motor & spch
;
;
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;
Ck_back:		;Back touch sensor

      JSR	Get_Back	;go ck for sensor trigger
      BCS	Normal_back	;go fini normal spch/motor table
      JMP	Idle		;no request

Get_back:	;this is the subroutine entry point.

      LDA	Port_C		;get I/O
      AND	#Touch_bck	;ck if Firby's back is rubbed
      BEQ	Do_tch_bk	;jump if lo
      LDA	Stat_2		;Get system
      AND	#Not_tch_bk	;clear previously inverted flag
      STA	Stat_2		;update
Tch1_out:
      CLC			;clear carry for no sensor request
      RTS			;

Do_tch_bk:
      LDA	Stat_2		;get system
      AND	#Tchbk_dn	;ck if prev done
      BNE	Tch1_out	;jump if was
A-70
      LDA	Stat_2		;get system
      ORA	#Tchbk_dn	;flag set ,only execute once
      STA	Stat_2		;update system

      LDA	Stat_4		;game mode status
      ORA	#Do_back	;flag sensor is active
      STA	Stat_4		;update
      SEC			;set indicates sensor is triggered
      RTS			;

Normal_back:	;enter here to complere sensor speech/motor

;****************************************************************

      JSR	Life		;go tweak health/hungry counters
      BCS	More_back	;if clear then do sensor else bail
      JMP	Idle		;done
More_back:

;****************************************************************


      LDA	#Back_split	;get random/sequential split
      STA	IN_DAT		;save for random routine

      LDX	#Seq_back	;get how many sequential selections
      LDA	#Ran_back	;get number of random slections
      JSR	Ran_seq		;go decide random/sequentaial

      LDX	Sensor_timer	;get current  for training subroutine

      BCS	Back_rnd	;Random mode when carry SET

      LDA	Sensor_timer	;ck if timed out since last action
      BEQ	Back_reset	;yep

      LDA	Tchbck_count	;save current
      STA	BIT_CT		;temp store

      INC	Tchbck_count	;if not then next table entry
      LDA	Tchbck_count	;get
      CLC
      SBC	#Seq_back-1	;ck if > assignment
      BCC	Back_set	;jump if <
      LDA	#Seq_back-1	;dont inc off end
      STA	Tchbck_count	;
      JMP	Back_set	;do it
Back_reset:
      LDA	#00		;reset to 1st entry of sequential
      STA	BIT_CT		;temp store
      STA	Tchbck_count	;
Back_set:
      LDA	#Global_time	;get timer reset value
      STA	Sensor_timer	;reset it
      LDA	BIT_CT		;get current pointer to tables

Back_rnd:

      STA	IN_DAT		;save decision
      LDA	#Back_ID	;which ram location for learned word count (offset)
A-71
      JSR	Start_learn	;go record training info
      LDA	IN_DAT		;get back word to speak

      JSR	Decid_age	;do age calculation for table entry
      LDX	TEMP0		;age offset
      LDA	Tback_S1,X	;get lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      LDA	Tback_S1,X	;get hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JMP	Start_macro	;go set group/table pointer for motor & spch
;
;
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;
; The IR routine turns interrupts off fo 100 Msec, which stops the
; timing chain (multiplies time by 100). This front end leaves
; interrupts on and sits in a loop for 5msec to determine if I.R. is
; active and if so, executes normal I.R. routine, else exits.

;********* start Tracker

;The way to include the IR program, I list as the following:
;It shows the program prargraph from Ck_IR: to Ck_fron:
;of course, It also attach the IR.asm file
;the IR.asm file I just make a little bit change, to make they work
;at any system clock assume by constant SystemClock:
;please advise.. :>

Ck_IR:
      LDA	Last_IR		;timer stops IR from hearing own IR xmit
      BEQ	CKIR_S		;jump if timer 0
      JMP	Idle		;abort if >0
CKIR_S:
      LDA	#FFh		;set loop timer
      STA	TEMP1		;
      LDA	#10h		;set gross timer
      STA	TEMP2		;
IR_req:
      LDA	Port_B		;ck if IR signal active (hi)
      AND	#IR_IN		;get port pin
      BNE	Got_IR		;got do input if active
      LDA	Port_B		;ck if IR signal active (hi)
      AND	#IR_IN		;get port pin
      BNE	Got_IR		;go do input if active
      DEC	TEMP1		;inside loop
      BNE	IR_req		;
      LDA	#FFh		;reset loop timer
      STA	TEMP1		;
      DEC	TEMP2		;outside loop
      BNE	IR_req		;loop thru
      JMP	Idle		;no activity found

Got_IR:
      LDA	#05		;number of times to ck for IR reception
A-72
      STA	TEMP4		;
Got_IR2:
        JSR	    D_IR_test	  ;used as a subroutine for diags
        BCS	    New_IR		  ;jump if found data
      DEC	TEMP4		;
      BNE	Got_IR2		;loop
        JMP	    Idle		  ;bail out if not
New_IR:
        JMP	    Normal_IR

;*************************
; Begin Koball's code
;*************************

D_IR_test:
        SET				  ;;Tracker
        JSR	    GBYTE		  ;;Tracker First time to read
        LDA	    #Intt_dflt	  ;Initialize timers, etc.
;;Tracker
        STA	    Interrupts	  ;load reg
;;Tracker
        LDA	    IN_DAT		  ;;load result to ACC
        CLI				  ;;Tracker

        RTS

Normal_IR:
; There are 4 I.R. table arranged as all other tables, one for each age.
; But here we get a random number which determines which one of the
; four tables we point to and the actual number received is the on of
; sixteen selection.


        LDA	    IN_DAT		  ;;Tracker add
      AND	#0Fh		;kill hi nibble (compliment of lo nibble)
      STA	IN_DAT		;save

      CMP	#08		;test for special sneeze command
      BNE	No_sneeze	;jump if not
      LDA	#Really_sick-30	;force Furby to get sick
      STA	Sick_counter	;update

No_sneeze:
      LDA	Bored_timer	;get current count
      STA	TEMP1		;save
Get_IR_Rnd:
      JSR	Random		;get something
      DEC	TEMP1		;-1
      BNE	Get_IR_rnd	;loop getting random numbers
      LDA	Seed_1		;get new random pointer
      AND	#0Fh		;kill hi nibble
      STA	TEMP1		;save
      CLC
      SBC	#11		;ck if > 11
      BCC	NormIR_2	;jump if not
      LDA	#96		;point to table 4
      JMP	Got_normIR	;
NormIR_2:
      LDA	TEMP1		;recover random number
      CLC
A-73
      SBC	#07		;ck if > 7
      BCC	NormIR_3	;jump if not
      LDA	#64		;point to table 3
      JMP	Got_normIR	;
NormIR_3:
      LDA	TEMP1		;recover random number
      CLC
      SBC	#03		;ck if > 03
      BCC	NormIR_4	;jump if not
      LDA	#32		;point to table 2
      JMP	Got_normIR	;
NormIR_4:
      LDA	#00		;force table 1

Got_normIR:

      CLC
      ROL	IN_DAT		;16 bit offset for speech
      CLC
      ADC	IN_DAT		;create speech field ofsett pointer
      TAX			;set offset

        LDA	    IR_S1,X		  ;get lo byte
        STA	    MACRO_Lo	  ;save lo byte of Macro table entry
        INX	    			  ;
        LDA	    IR_S1,X		  ;get hi byte
        STA	    Macro_Hi	  ;save hi byte of Macro table entry
        JMP	    Start_macro	  ;go set group/table pointer for motor & spch

        Include	     IR2.Asm		;asm file

;********* end Tracker

;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************

Ck_front:		; touch front  (tummy)

      JSR	Get_front	;go ck for sensor trigger
      BCS	Normal_front	;go fini normal spch/motor table
      JMP	Idle		;no request

Get_front:	;this is the subroutine entry point.

      LDA	Port_C		;get I/O
      AND	#Touch_frnt	;ck if Firby's chest is rubbed
      BEQ	Do_tch_ft	;jump if lo
      LDA	Stat_2		;Get system
      AND	#Not_tch_ft	;clear previously inverted flah
      STA	Stat_2		lupdate
Touch_end:
      CLC			;clear indicates no sensor request
      RTS			;
Do_tch_ft:
      LDA	Stat_2		;get system
      AND	#Tchft+dn	;ck if prev done
      BNE	Touch_end	;jump if was
A-74
      LDA	Stat_2		;get system
      ORA	#Tchft_dn	;flag set ,only execute once
      STA	Stat_2		;update system

      LDA	Stat_4		;game mode status
      ORA	#Do_tummy	;flag sensor is active
      STA	Stat_4		;update
      SEC			;set indicates sensor is triggered
      RTS			;

Normal_front:		;enter here to complete sensor speech/motor


;****************************************************************

      JSR	Life		;go tweek health/hungry counters
      BCS	More_front	;if clear then do sensor else bail
      JMP	Idle		;done
More_front:

;****************************************************************


      LDA	#Front_split	;get random/sequential split
      STA	IN_DAT		;save for random routine

      LDX	#Seq_front	;get how many sequential selections
      LDA	#Ran_front	;get sequential split
      JSR	Ran_seq		;go decide random/sequential

      LDX	Sensor_timer	;get current  for training subroutine

      BCS	Front_rnd	;Random mode when carry set

      LDA	Sensor_timer	;ck if timed out since last action
      BEQ	Front_rest	;yep

      LDA	Tchfrnt_count	;save current
      STA	BIT_CT		;temp store

      INC	Tchfrnt_count	;if not then next table entry
      LDa	Tchfrnt_count	;get
      CLC
      SBS	#Seq_front-1	;ck if > assignment
      BCC	Front_set	;jump if <
      LDA	#Seq_front-1	;dont inc off end
      STA	Tchfrnt_count	;
      JMP	Front_set	;do it
Front_reset:
      LDA	#00		;reset to 1st entry of sequental
      STA	BIT_CT		;temp store
      STA	Tchfrnt_count	;
Front_set:
      LDA	#Global_time	;get timer reset value
      STA	Sensor_timer	;reset it
      LDA	BIT_CT		;get current pointer to tables

Front_rnd:

      STA	IN_DAT		;save decision
A-75
      LDA	#Front_ID	;which ram location for learned word count (offset)
      JSR	Start_learn	;go record training info
      LDA	IN_DAT		;get back word to speak

      KSR	Decid_age	;do age calculation for table entry
      LDX	TEMP0		;age offset
      LDA	Tfrnt_S1,X	;get lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      INX			;
      LDA	Tfrnt_S1,X	;get hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JMP	Start_macro	;go set group/table pointer for motor & spch

;
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;
;
Ck_feed:		; food sensor
;
      JSR	Get_feed	;go ck for sensor trigger
      BCS	Normal_feed	;go fini normal spch/motor table

      JMP	Idle		;no request

Get_feed:	;this is the subroutine entry point.

; Each trigger increments the health status at a greater rate

; Special enable routine to share port pin D1 with invert switch.
; Feed switch is pulled hi by the DAC1 (aud-a) output only after
; we test the invert line. If invert is not hi, then turn on
; DAC1 and ck feed line on same port D1.


      LDA	Port_D		;get I/O
      AND	#Ball_invert	;ck if we are inverted
      BEQ	St_feed		;jump if not inverted (lo=not inverted)
      CLC			;indicates no request
      RTS			;if inverted then bypass
St_feed:
      LDA	#FFh		;turn DAC2 on to enable feed switch
      STA	DAC2		;out
      LDA	Port_D		;get I/O
      AND	#Ball_invert	;ck if feed switch closed
      BNE	Start_feed	;jump if hi
      LDA	#00
      STA	DAC2		;clear feed sw enable
      LDA	Stat_3		;Get system
      AND	#Not_feed	;clear previously inverted flag
      STA	Stat_3		;update
Feed_out:
      CLC			;clear indicates no request
      RTS			;go test next

Start_feed:
      LDA	#00
A-76
      STA	DAC2		;clear feed sw enable

;	LDA	Stat_3		;get system
;	AND	#Feed_dn	;ck if prev done
;	BNE	Feed_out	;jump if was
;	LDA	Stat_3		;get system
;	ORA	#Feed_dn	;flag set ,only execute once
;	STA	Stat_3		;update system

      LDA	Stat_4		;game mode status
      ORA	#Do_feed	;flag sensor is active
      STA	Stat_4	;set when sensor is triggered
      RTS			;

Normal_feed:		;enter here to complete speech/motor

;****************************************************************

;  health table calls here and decision for which speech pattern

      LDA	#Food		;each feeding increments hunger counter
      CLC
      ADC	Hungry_counter	;feed him!
      BCC	Feeding_dn	;jump if no roll over
      LDA	#FEh		;max count
Feeding_dn:
      STA	Hungry_counter	;update

;;;;;	JSR	Life		;go finish sick/hungry speech


;****************************************************************

      LDA	#Feed_split	;get random/sequential split
      STA	IN_DAT		;save for random routine

      LDX	#Seq_feed	;get how many sequential selections
      LDA	#Ran_feed	;get random assignment
      JSR	Ran_seq		;go decide random/sequential

      LDX	Sensor_timer	;get current  for training subroutine

      BCS	Feedrand	;Random mode when carry set

      LDA	Sensor_timer		;ck if timed out since last action
      BEQ	Feed_reset	;yep

      LDA	Feed_count	;save current
      STA	BIT_CT		;temp store

      INC	Feed_count	;if not then next table entry
      LDA	Feed_count	;get
      CLC
      SBC	#Seq_feed-1	;ck if > assignment
      BCC	Feed_set	;jump if <
      LDA	#Seq_feed-1	;dont inc off end
      STA	Feed_count	;
      JMP	Feed_set	;do it
Feed_reset:
A-77
      LDA	#00		;reset to 1st entry of sequential
      STA	BIT_CT		;temp store
      STA	Feed_count	;
Feed_set:
      LDA	#Global_time	;get timer reset value
      STA	Sensor_timer	;reset it
      LDA	BIT_CT		;get current pointer to tables

Feedrand:

      STA	IN_DAT		;save decision
      LDA	#Feed_ID	;which ram location for learned word count (offset)
      JSR	Start_learn	;go record training info
      LDA	IN_DAT		;get back word to speak

      JSR	Decid_age	;do age calculation for table entry
      LDX	TEMP0		;age offset
      LDA	Feed_S1,X	;get lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      INX			;
      LDA	Feed_S1,X	;get hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JMP	Start_macro	;go set group table pointer for motor & spch

;
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;
Ck_light:		;Bright light sensor

      JSR	Get_light	;now handled as a subroutine
      BCC	Ck_light2	;jump if new level > reff
      JMP	Idle		;nothing to do
Ck_light2:
      JMP	Normal_light		;jump if new level > reff


      Include		Light5.asm	;asm file

Normal_light:

; below routines are jumped to by light exec if > reff


;****************************************************************

      JSR	Life		;go tweek health/hungry counters
      BCS	More_light	;if clear then do sensor else bail
      JMP	Idle		;done
More_light:

;****************************************************************


      LDA	#Light_split	;get random/sequential split
      STA	IN_DAT		;save for random routine
A-78
      LDX	#Seq_light	;get how many sequential selections
      LDA	#Ran_light	;get number of random slections
      JSR	Ran_seq		;go decide random/sequentaial
      
      LDX	Sensor_timer	;get current  for training subroutine
      
      BCS	Lghtrand	;Random mode when carry SET
      
      LDA	Sensor_timer	;ck if timed out since last action
      BEQ	Lght_reset	;yep
      
      LDA	Lght_count	;save current
      STA	BIT_CT		;temp store
      
      INC	Lght_count	;if not then next table entry
      LDA	Lght_count	;get
      CLC
      SBC	#Seq_light-1	;ck if > assignment
      BCC	Lght_set	;jump if <
      LDA	#Seq_light-1	;dont inc off end
      STA	Lght__count	;
      JMP	Lght__set	;do it
Lght_reset:
      LDA	#00		;reset to 1st entry of sequential
      STA	BIT_CT		;temp store
      STA	Lght_count	;
Lght_set:
      LDA	#Global_time	;get timer reset value
      STA	Sensor_timer	;reset it
      LDA	BIT_CT		;get current pointer to tables

Lghtrand:

      STA	TEMP4		;save seq/rand pointer
      LDA	Stat_3		;system
      AND	#Lght_stat	;ck bit for light/dark table
      BEQ	Do_dark		;jump if clear

      LDA	TEMP4		;get ponter

      STA	IN_DAT		;save decision
      LDA	#Light_ID	;which ram location for learned word count (offset)
      JSR	Start_learn	;go record training info
      LDA	IN_DAT		;get back word to speak

      JSR	Decid_age	;do age calculation for table entry
      LDX	TEMP0		;age offset
      LDA	Light_S1,X	;get lo byte
      STA	Macro_Lo	;save lo byte of Macro table entry
      INX			;
      LDA	Light_S1,X	;get hi byte
      STA	Macro_Hi	;save hi byte of Macro table entry
      JMP	Start_macro	;go set group table pointer for motor & spch

Do_dark:
      LDA	TEMP4		;get pointer

      STA	IN_DAT		;save decision
A-79