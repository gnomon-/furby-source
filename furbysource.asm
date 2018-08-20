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
      JSR   Kick_IRQ    ;wait for interrupt to restart
      JSR   TI_reset    ;go init TI (uses 'Cycle_timer')

; Preset motor speed, assuming mid battery life, we eat the pulse width
; so that the motor wont be runing at 6 volts and burn out. We then
; predict what the pulse width should be for any voltage.

;     LDA   #Mpulse_on  ;preset motor speed
      LDA   #ll
      STA   Mon_len           ;set motor on pulse timing

      LDA   #05         ;
      STA   Moff_len    ;

;ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ_____ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
;* 'Diagnostics and celibration Routine                   *
;ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ_ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
;

      Include           Dieg7.asm   ;asm file

; ****** Only called by diagnostic speech routines *********

; Be sure to set 'MACRO_HI' and all calls are in that 128 byte block.

Dieg_macro:
      STA   Macro_Lo    ;save lo byte of Macro table entry
        LDA     #0b8h   ;#90h            ;_ex offset to adrs_400 added
to diag call
      CLC
      ADC   Macro_Lo    ;add in offset
      STA   Macro_Lo    ;update
      LDA   #01         ;get hi byte adrs 400 = 190h
      STA   Macro_Hi    ;save hi byte of Macro table entry
      JSR   Get_marcro  ;go start motor/speech
      JSR   Notrdy            ;Do / get status for speech and motor
      RTS               ;yo


; Enter with Areg holding how many 30 mili second delay cycles

Half_delay
      STA   TEMP1       ;save timer
Half d2:
      LDA   #10         ;set 1/2 sec      (y * 2.9 mSec)
      STA   Cycle_timer ;set it
Half_d3:
      LDA   Cycle_timer ;ck if done
      BNE   Half_d3           ;loop
      DEC   TEMP1       ;
      BNE   Half_d2           ;loop
      RTS               ;done
A-21
Test_byp:   ;We assume diagnostic only runs on coldboot

;***************************************************************
      
      LDA   #FFh        ;initialize word training variable
      STA   Temp_ID           ;

      LDA   #FFh        ;
      STA   Hungry_counter    ;preset furby's health
      STA   Sick_counter

;***********************************************************

; We sit here and wait for tilt to go away, and just keep incrementing
; counter until it does. This becomes the new random generator seed.

Init_rnd:
      INC   TEMP1       ;random counter
      LDA   Port_D            ;get switches
      AND   #03         ;check tilt & invert sw
      BNE   Init_rnd    ;loop til gone
      LDA   TEMP1       ;get new seed
      STA   Spcl_seed1  ;stuff it
      STA   Seed_1            ;also load for cold boot
;*************************************************************

; Use fee sw to generate a better random number

      JSF   Get_feed    ;go test sensor
      LDA   Stat_4            ;get system
      AND   #Do_feed    ;ck sw
      BNE   Feed_rnd    ;if feed sw then cold boot
      JMP   End_coldinit      ;else do warm boot
Feed_rnd:
      INC   TEMP1       ;random counter
      LDA   Stat_4            ;system
      AND   #DFh
      STA   Stat_4            ;update
      JSR   Get_feed    ;go test sensor
      LDA   Stat_4            ;get system
      AND   •Do_feed    ;ck sw
      BNE   Feed_md     ;wait for feed to go away
      LDA   TEMPI       ;get new seed
      STA   Spcl_seedl  ;stuff it
      STA   Seed_l            ;also load for cold boot

;************************************************************

;; IF this is a cold boot , reset command then clear EEPROM and
;  chose a new name and voice.

Do_cold_boot:

      LDA   #00
      STA   Warm_cold   ;flag cold boot
A-22
      LDA   Stat_0            ;system
      ORA   #Say_new_name     ;make system say new name
      STA   Stat_0            ;

;*******  NOTE :::::
;
;   VOICE AND NAME SLECTION MUST HAPPEN BEFORE EEPROM WRITE OR
;   THEY WILL ALWAYS COME UP  00    because ram just got cleared!!!!!!



; Random voice selection here

      LDA   #80h        ;get random/sequential split
      STA   IN_DAT            ;save for random routine
      
      LDX   #00         ;make sure only gives random
      LDA   #10h        ;get number of random selections
      JSR   Ran_seq           ;go get random selection
      
      TAX
      LDA   Voice_table,X     ;get new voice
      STA   Rvoice            ;set new voice pitch

;**********************************************************************
*

; On power up or reset, Furby must go select a new name ,,, ahw how
cute.

      JSR   Random            ;
      AND   #1Fh        ;get 32 possible
      STA   Name        ;set new name pointer
      JSR   Do_EE_write ;write the EEPROM

End_coldinit:



;ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
;* 'Special initialization prior to normal run mode               *
;*  Jump to Warm_boot when portD wakes us up
;ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
;

Warm_boot:  ;normal start when Port_D wakes us up.

      JSR   S_EEPROM_READ     ;read data to ram

;Eprom_read_byp:

;***************************************************************
; If light osc fails, or too dark and that sends us to sleep, we
; set 'Dark_sleep_prev' and save it in EEPROM in 'Seed_2'.
; when the sleep routine executes,(00 01 based on this bit)
; When we wake up we recover this bit and it becomes the previous done
; flag back in 'Stat_0', so that if the osc is
A-23
; still dark or failed, Furby wont go back to sleep.
      LDA   Seed_2            ;from EEPROM
      BED   No_prevsleep      ;jump if none
      LDA   Stat_0            ;system
      ORA   #Dark_sleep_prev ;prev done
      STA   Stat_0            ;update

No_prevsleep:

;***************************************************************

      LDA   Spcl_seed1  ;recover start up random number
      STA   Seed_1            ;set generator

;***************************************************************


; Pot_timeL2 is save in ram through sleep mode and then reloaded '
; Pot_timeL which is the working register for the motor position.
; This allows startup routines to clear ram without forgetting the
; last motor position.

      LDA   Pot_timeL2  ;get current count
      STA   Pot_ imeL   ;save in motor routine counter

;*************************

; Get age and make sure it is not greater than 3 (age4)

      LDA   Age         ;get current age
      AND   #83h    ;preserve bit 7 which is 9th age counter bit
;;;;;             and insure age not >3

      STA   Age         ;set system.

;*************************

      LDA   #Bored_reld ;reset timer
      STA   Bored_timer ;

      LDA   #03         ;set timer
      STA   Last_IR           ;timer stops IR from hearing own IR xmit
      
      JSR   Get_light   ;go get light level sample
      LDA   TEMP1       ;get new count
      STA   Light_reff  ;update system


;**********************************************************************
*

      LDA   Warm_cold   ;decide if warm or cold boot
      CMP   #11h        ;ck for warm boot
      BEQ   No_zero           ;jump if is
A-24
      LDA   #00         ;point to macro 0 (SENDS TO SLEEP POSITION)
      STA   Macro_Lo
      STA   Macro_Hi
      JSR   Get_macro   ;go start motor/speech
      JSR   Notrdy            ;Do / get status for speech and motor

No_zero:


      LDA   #11         ;preset motor speed
      STA   Mon_len           ;set motor on pulse timing

      LDA   #05        ;set motor to 3/4 speed for speed test
      STA   Moff_len   ;set motor off pulse timing
;
;
      LDA   • 00        ;clear all system sensor requests
      STA   3tat_4            ;update


; Currently uses 4 tables, one for each age.
      LDA   Stat_0            ;system
      ORA   #Init_motor :flag motor to do speed test
      ORA   #Init_Mspeed      ;2nd part of test
      STA   Stat_0            ;update


;************************************************************

; Do wake up routine :

      lda   #Global_time      ;reset timer to trigger sensor learning
      STA   Sensor_timer      ;

      LDA   #80h        ;get random/sequential split
      STA   IN_DAT            ;save for random routine

      LDX   #00h        ;make sure only gives random
      LDA   #10h        ;get number of random selections
      JSR   Ran_seq           ;go get random selection
      LDA   TEMP1       ;get decision

      STA   IN_DAT            ;save decision
      LDA   #Wake_ID    ;which ram location for learned word count
(offset)
      JSR   Start_learn ;go record training info
      LDA   IN_DAT            ;get back word to speak

      JSR   Decid_age   ;do age calculation for table entry
      LDX   TEMP0       ;age offset
      LDA   Wakeup_S1,X ;get new sound/word
      STA   Macro_Lo    ;save lo byte of Macro table entry
      INX               ;
      LDA   Wakeup_S1,X ;get new sound/word
      STA   Macro_Hi    ;save hi byte of Macro table entry
      JMP   Start_macro ;go start speech

;************************************************************
A-25