;*******************************************************************
;;   MODS :

; LIGHT3.asm
; Add test to light counter so that if the oscillator
; fails, the system will ignore light sensor and keep running.
;
; Light!
; When goes to conpiete dark and hits the 'Dark_sleep' level
; and stays there until the reff level updates, at that point
; we send Furby to sleep.

;
;    Lights (used in F-RELS2 )
;    Change detection of light threshold to prevent false or continue.s trigger.



;*******************************************************************

Bright             EQU     15      ;light sensor trigger > reff level   (Hon)
Dim                EQU     15      ;Light sensor trigger < reff level   (Hon)

Shift_reff         EQU     10      imax count to set or clear prev done flag==

Dark_sleep         EQU     BOh     ;when timer A hi =0f and timer A low
;                                  is = to this EQU then send him to sleep



; The CDS light sensor generates a square wave of 5OOhz to 24khz based on
; light brightness. We can loop on the sense line and count time for the
; lo period to determine if light has changed and compare it to previous
; samples. This also determines going lighter or darker. We also set a timer
; so that if someone holds their hand over the sensor and we announce it.
; if the change isnt stable for 10 second, we ignore the change back to the
; previour state. If it does exii  for > 10 seconds, then it becomes the
; new sample to compare against on the next cycle.

; In order to announce light change,    the system must have a consistent
; count > 'Shlft_reff'.

;    If a previous retf has been set then the 'Up_light' bit is set to
;    look for counts greater than the reff. The system passes through the
;    light routine 'Shift_reff' times. If it is consistently greater than
;    the reff level, we get a speech trigger  If any single pass is less
;    than the reff, the counter is set back to zero. This scenario also
;    is obeyed when the trigger goes away, ie remove your hand, and the system
;    counts down to zero.(‘Up_light' bit is cleared ) If during this time any
;    trigger greater than reff occurs, the count is set back to max.
;    This should prevent false triggers.




Get_light:         ;alt entry for diagnostics

; This uses timer A tc get a count from the lo period of the clk

        SEI                     ;interrupts off
        LDA     #0C0H           ;disable timer, clock, ext ints.
        STA     Interrupts      ; & watchdog; select IRQ int.
        LDA     #000H           ;set timer A for timer mode
        STA     TMA_CON         ;
        LDA     #OOOH           ;re-start timer A
        STA     TMA_LSB         ;
        LDA     #000H           ;now CPUCLK; was #010H - CPUCLK/4   (Hon)
        STA     TMA_MSB         ;
Ck_lght2i:
        LDA     TMA_MSB         ;test for dead light osc
        AND     #0Fh            ;get timer
        CMP     #OFh            ;ck for > OE
        BNE     Ck_lt2a         ;jump if not
        LDA     TMA_LSB         ;get lo byte
        CLC
        SBC     #EOh            ;ck for > (mnb+lsb =OFEO)
        BCC     Ck_lt2a         ;jump if not
        JMP     Light_fail      ;bail out if >

Ck_lt2a:
        LDA     Port_D          ;get I/O
        AND     #Light_in       ;ck light elk is hi
        BEQ     Ck_lght2        ;wait for it to go hi

        LDA     #000H           ;re-start timer A
        STA     TMA_LSB         ;
        LDA     #000H           ;now CPUCLK: was #OIOH = CPUCLK/4    (Hon)
        STA     TMA_MSB         ;

Ck_lght3:
        LDA     TMA_MSB         ;test for dead light osc
        AND     #OFh            ;get timer
        ???     #0Fh            ;ck for > OE            ;; XXX: UNCLEAR
        BNE     Ck_lt3a         ;jump if not
        LDA     TMA_LSB         ;get lo byte
        CLC
        SBC     #EOh            ;ck for > (msb+lsb =OFEO)
        BCS     Light_fail      ;bail out if >                          ..

Ck_lt3a:
        LDA     Port_D          ;get I/O
        AND     #Light_in       ;ck light clk is lo
        BNE     Ck_lght3        ;wait for it to go lo to insure the clk edge
Ck_lght4:

        LDA     #OOOH           ;re-start timer A
        STA     TMA_LSb         ;
        LDA     #OOOH           ;now CPUCLK: was #010H = CPUCLK/4   (Hon)
        STA     TMA_MSB         ;

Ck_lght4a:
        LDA     Port_D          ;get I/O
        AND     #Light_in       ;ck if still lo
        BEQ     Ck_lght4a       ;loop till hi

; Timer A holds count for lo peiiod of elk

Lght4cmp:
        LDA     TMA_MSB        ;get timer high byte
        AND     #OOFH          ; mask out high nybble
        STA     TEMP2          ; and save it
        LDA     TMA_LSB        ;get timer low byte
        STA     TEMP1          ; and save it

        LDA     TMA_MSB        ;get timer A high byte again

---

           AND   iOOFH               mask out high nybble
           CMP   TEMP2             / and compare it with last reading
           BNE   Lghtictrp         /loop until they're  Tual

; take 12 bit timer (2 bytea) and move to one byte and trash lo nible
/ of low byte. End up with hi 8 bits out of 12.

        LDX      #04               /loop counter
Light_byte:
        RQR      TEMP2             /get lo bit into carry
        ROR      TEMPI             /shuffle down and get carry from TEMP2
        DEX                        /-I
        BNE      Light_byte        /loop till done

Ck_lght4bs                                   !
        LDA      #Intt_dflc        /Initialize timers, etc.
        STA      Interrupt*        /re-establish normal system
        CLI                        /re-enable interrupt
        JSR      Kick_IRQ          zwait tor motor R/C to start working again
        CLC                        /clear

/-now have new cour.* in      'TEMPI'

           LDA   Light_reff        /get previous sample
           SBC   TEMPI             zck against current sample
           BCC   Ck_lghc5          /jump if negative
           CLC
           SBC   •Bright           zck if difference > reff
           BCS   Lght_brt          /go do speech
           JMP   Kill_ltrf         /bail out if not

Ck_lght5:
        CLC
        LDA      TEMPI             ;try the reverse subtraction
        SBC      Light_ref£        ; prev
        BCC      Kill_ltr£         ;quit if negative
        CLC
        SBC      •Dim              /is diff < reff
        BCC      Kill_ltrf         /bail out if not
Lght_dimi
        LDA      Stat_3            z system
        AND      #Nt_lght_Etat     /clear 1 it to indicate dark table
        STA      Stat_3            /update system
        JMP      Do_lght           /go fini
Lght_brti
        LDA      Stat_3            /system
        ORA      •Lght_atac        /set bit to indicate light table
        STA      Stat_3            /update system
        JMP      Do_lght i

Light_£ail/
       LDA       #FFh              zforce lo number so no conflicts
       STA       TEMPI
       IDA       •Intt_d£lt        /Initialize timers, etc.
       STA       Interrupts        /re-establish normal si tern
       CLI                         /re-enable interrupt
       JSR       Kick_IRQ          /wait for motor R/C to start working again
       JMP       Kill_ehi£t        /ret with no req

Do_lght:
        LDA      Stat_l          ;ayatem
        AND      •Up_light       ;ck if incmnt mode
        BNE      RaC ahftup      .•jump if incnmt mode
        LDA      #Shi£t_re£f     ;set to max
        STA      Light_shi£t     l
        JMP      No It todo      i
Rst_shftup:

          INC    Light_ahift     !*1
          LDA    Light_ahift     iget   counter
          CLC
          SBC    #Shi£t_re£f     ;ck if > max reff count
          BCC    No_lt_todo      (junp if < max count
          LDA    #Shift_reff     ; reaet to max
          STA    Light_ahift     »

          LDA    Stat_0          ;aystem
          AND    #Lt_prev_dn     ; check if previously done
          BNE    New_ltreff      ; junp if was

          LDA    Stat_0          ;system
          ORA    #Lt_prev_dn     /set previously done
          STA    Stat_0          .■update

(         LDA    Stat_l          .-system
(         AND    #EFh            :set sytem to shift decrimt mode
;         STA    Stat_l          ; up '-ate

          LDA    #Light_reload   ;rese- for next trigger
          STA    Light_timer     .-set it
          JMP    Do_ltchg        ;go announce it

New_ltref£:
        LDA      Light_timer     ;get current
        BNE      No_lt_todo      .■nothing to do                    ,
        LDA      TEMPI           ;get new count
        STA      Light_ref£      (update system

          :da    Stat_l          .• system
          AND    #EFh            (set sytem to shift decrrmt mode
          STA    Stat_l          .• update

          LDA    TEMPI           .•get current value
          CLC
          SBC    •Dark_sleep     (Ck if > sleep level
          BCS    Ck_drk          (jump if >
          LDA    Stat_0          (system
          AND    •7Fh            (kill prev done
          STA    Stat_0          (Update
          JMP    Kill_ltr£       1


Ck_drki
          LDA    Stat_0          lsystem

          AI.D   #D rk_sleep_prev ;ck if thia wan already done
          BNE    Kill_ltrf       (junp if was

          LDA    Stat_0          ;system
          ORA    #REQ_dark_aleep (set it
          ORA    #Dark_sleep_prev ;aet also
          STA    Stat_0          (update

Kill_ltr£i
1       LDA    Stat_0   '       jeystero
;       AND    #Lt_prev_dn      ;check if previoualy done
[       BEQ    No_lt_‘ odo      ;jun*> if clear
        LDA    Light_ahi£t      ;get shift counter
        BEQ    Kill_ahift       ;junv if went zero last time
        LDA    Stat_l           jsystem
        AND    #Up_light        ;ck if incrnnt mode
        BEQ    Rst_sh£cdn       ;jujn> if decrimt mode
        LDA    #00              »aet to min
        STA    Light_shift      I
        JMP    No_lt_todo       ;
Ret ohftdnt
        DEC    Light_shi£t      j-i
        JMP    No_lt_todo       (done
Kill_shi£t s
        LDA    Stat_0           /system
        AND    #FDh             (dears Lt_prev_dn
        STA    Stat_0           (update

        LDA    Stat_l           /system
        ORA    #Up_light        .-prepare to incrrmt   ,Light_shift'
        STA    Stat_l           (update



No_lt_todo:
        SEC                     ,-carry set indicates no light change
        RTS
