; dasmfw: Disassembler Framework V0.35
; Loaded: binary file "ESS540.bin"
; Loaded: Info file "ESS540_dasmfw.nfo"


;****************************************************
; Used Labels
;****************************************************

BOOTROM EQU     $0120
INPBUF  EQU     $C080
CMDAD1  EQU     $C100
CMDAD2  EQU     $C200
DRVTAB  EQU     $DE80
GRADDR  EQU     $DE92
SCORAD  EQU     $DE94
CURPOS  EQU     $DE96
CURROW  EQU     $DE98
CURLIN  EQU     $DE99
MDE9A   EQU     $DE9A
MDE9C   EQU     $DE9C
MDE9E   EQU     $DE9E
MDEA0   EQU     $DEA0
MDEA2   EQU     $DEA2
MDEA4   EQU     $DEA4
MDEA5   EQU     $DEA5
MDEA6   EQU     $DEA6
MDEA7   EQU     $DEA7
MDEA8   EQU     $DEA8
MDEA9   EQU     $DEA9
VIDRAM  EQU     $E000
CRTC    EQU     $EC00
CRTC_1  EQU     $EC01
FDC     EQU     $EC04
FDCTRK  EQU     $EC05
FDCSEC  EQU     $EC06
FDCDAT  EQU     $EC07
FDCSTA  EQU     $EC08
DRVLAT  EQU     $EC0C
ACIASR  EQU     $EF60
ACIADR  EQU     $EF61
VIA_1   EQU     $EF81
VIA_8   EQU     $EF88
VIA_13  EQU     $EF8D
RESET   EQU     $FFFE

;****************************************************
; Program's Code Areas
;****************************************************


        ORG     $F000

; EC-6809 MEMORY MAP

; 0000-BFFF  USER RAM
; C000-DFFF  FLEX
; C000       FLEX LOAD ADDRESS
; C000-C07F  STACK STACK POINTER INIT = C07F
; C080-C0FF  INPUT BUFFER
; C100-C6FF  UTILITY COMMAND AREA
; C700-C83F  SCHEDULER & SPOOLER
; C840-C97F  FILE CONTROL BLOCK
; C980-CBFF  SYSTEM FILES AREA
; CC00-D3FF  DISK OPERATING SYSTEM
; D400-DDFF  FILE MANAGEMENT SYSTEM
; DE00-DFFF  FLOPPY ROUTINES
; E000-E7FF  VIDEO RAM
; E800-EBFF  USER I/O
; EC00-ECFF  FDC + CRTC
; EC00        CRTC
; EC04        FDC STATUS - CDE REGISTER
; EC05        FDC TRACK REGISTER
; EC06        FDC SECTOR REGISTER
; EC07        FDC DATA REGISTER
; EC0C        DRIVE SELECT LATCH
; EF00-EFFF  ACIA + VIA
; EF60        ACIA
; EF80        VIA
; ED00-EDFF  USER I/O
; E800-EFFF  I/O
; F000-F7FF  BOOT + SCREEN ROM
; F800-FFFF  EC-6809 ASSIST09 ROM
; *
; *      TABLE OF SUPPLEMENTARY I/O VECTORS
; *
        LBRA    FLXBOT                  ; F000: 16 02 39        @F000 LOAD BOOT SECTOR & JUMP
        LBRA    SEEK                    ; F003: 16 00 3C        @F003 SEEK TRACK
        LBRA    READ                    ; F006: 16 00 AC        @F006 READ SECTOR
        LBRA    WRITE                   ; F009: 16 00 EA        @F009 WRITE SECTOR
        LBRA    ZF074                   ; F00C: 16 00 65        @F00C SIMPLE RTS
        LBRA    RESTOR                  ; F00F: 16 01 40        @F00F RESTORE
        LBRA    DRVSEL                  ; F012: 16 01 6D        @F012 SELECT DRIVE
        LBRA    VERIF                   ; F015: 16 01 20        @F015 VERIFY
        CLRB                            ; F018: 5F              @F018 CLEAR B AND RETURN
        RTS                             ; F019: 39             
        NOP                             ; F01A: 12             
        LBRA    ZF074                   ; F01B: 16 00 56        SIMPLE RTS
        LBRA    INITAB                  ; F01E: 16 01 B2        INIT TABLE AND TRACK REG
        LBRA    GETDTA                  ; F021: 16 02 82        WAIT FOR AND GET SERIAL DATA
        LBRA    GDTAEC                  ; F024: 16 02 93        GET SERIAL DATA WITH ECHO
        LBRA    WRTDTA                  ; F027: 16 02 92        WRITE SERIAL DATA
        LBRA    DATRDY                  ; F02A: 16 02 65        TEST SERIAL DATA READY
        LBRA    INIACI                  ; F02D: 16 02 6B        INIT ACIA
        RTI                             ; F030: 3B              @F030 RETURN FROM INTERRUPT
        NOP                             ; F031: 12             
        NOP                             ; F032: 12             
        LBRA    WTLDVI                  ; F033: 16 02 95        GET // DATA INPUT
        LBRA    INIT                    ; F036: 16 03 49        INIT OF VIA, ACIA, CRTC
        LBRA    VIARDY                  ; F039: 16 02 A0        TEST // DATA READY
        LBRA    JMPADR                  ; F03C: 16 02 A6        MAY BE PUT CHAR ON SCREEN
        LBRA    GDTVIE                  ; F03F: 16 02 94        GET // DATA INPUT WITH ECHO
; END OF TABLE

; SEEK    SEEKS TO THE TRACK SPECIFIED IN THE 'A' ACCUMULATOR. IN
; DOUBLE-SIDED SYSTEMS, THIS ROUTINE SHOULD ALSO SELECT THE
; CORRECT SIDE DEPENDING ON THE SECTOR NUMBER SUPPLIED IN 'B'.

; ENTRY - (A) = TRACK NUMBER
; (B) = SECTOR NUMBER

; EXIT -  (X) MAY BE DESTROYED (SEE TEXT)
; (A) MAY BE DESTROYED (SEE TEXT)
; (B) = ERROR CONDITION
; (Z) = 1 IF NO ERROR
; = 0 IF AN ERROR

SEEK    CLR     $0D,U                   ; F042: 6F 4D           CLEAR TRY COUNTER
        LBSR    WATFDC                  ; F044: 17 01 1B        WAIT FOR FDC NOT BUSY
        STB     $03,U                   ; F047: E7 43           STORE SECTOR IN REPO
ZF049   STA     FDCDAT                  ; F049: B7 EC 07        SET TRACK REG IN FDC
        BSR     STLASI                  ; F04C: 8D 27           SET LATCH FOR SIDE
        LDB     $03,U                   ; F04E: E6 43           RESTORE SECTOR
        STB     FDCSEC                  ; F050: F7 EC 06        SEND TO FDC SECTOR REG
        LDB     #$14                    ; F053: C6 14           SEEK WITH VERIF CMD
        ORB     $01,U                   ; F055: EA 41           ADD STEP RATE TO COMMAND
        STB     FDC                     ; F057: F7 EC 04        ISSUE CMD TO FDC
        LBSR    TEMPO                   ; F05A: 17 01 A9        FOR CMD EXECUTION
        LBSR    WATFDC                  ; F05D: 17 01 02        WAIT FOR FDC NOT BUSY
        LDB     FDC                     ; F060: F6 EC 04        LOAD STATUS REG
        ANDB    #$10                    ; F063: C4 10           TEST SEEK ERROR
        BEQ     ZF074                   ; F065: 27 0D           RETURN IF NO SEEK ERROR - BUT COULD OTHERS
        TST     $0D,U                   ; F067: 6D 4D           TEST IF FIRST OR SECOND TRY
        BNE     ZF06F                   ; F069: 26 04          
        BSR     SWIDST                  ; F06B: 8D 2E           COME HERE IF FIRST TRY - CHG DENSITY
        BRA     ZF049                   ; F06D: 20 DA           RETRY
ZF06F   BSR     SWIDST                  ; F06F: 8D 2A           COME HERE IF SECOND TRY FAIL - CHG DENSITY BACK
        COMB                            ; F071: 53             
        LDB     #$10                    ; F072: C6 10           LOAD ERROR SEEK ERROR CODE ONLY
ZF074   RTS                             ; F074: 39              RETURN

; SET LATCH FOR SIDE
STLASI  PSHS    X,D                     ; F075: 34 16          
        LDA     $03,U                   ; F077: A6 43           LOAD CURRENT SECTOR
        LDB     $04,U                   ; F079: E6 44           LOAD CURRENT LATCH STATE
        BITB    #$40                    ; F07B: C5 40           CHECK IF DDEN OUTPUT = 1
        BEQ     ZF083                   ; F07D: 27 04           BRANCH IF DDEN = 0 DD
        CMPA    #$0A                    ; F07F: 81 0A           SD CASE - COMPARE SD SECT PER TRACK / SIDE
        BHI     ZF096                   ; F081: 22 13           BRANCH TO SIDE 1 IF SECTOR > $0A
ZF083   LDB     ,U                      ; F083: E6 C4           DD CASE - LOAD CURRENT DRIVE
        LEAX    $0F,U                   ; F085: 30 4F           COMPUTE ADDRESS FOR DD SECTOR PER SIDE TABLE
        CMPA    B,X                     ; F087: A1 85           COMPARE SECTOR NUMBER TO WHAT IS STORED IN REPO AT $0F-$10-$11 (D0-D1-D2)
        BHI     ZF096                   ; F089: 22 0B           BRANCH TO SIDE 1 CASE IF HIGHER
        LDD     #$0010                  ; F08B: CC 00 10        PREPARE A TO NO CHANGE IN HEAD / B TO SET HEAD TO 0 AND KEEP EVERYTHING ELSE -> HEAD 0
ZF08E   STA     $0B,U                   ; F08E: A7 4B           STORE HEAD IN USE IN REPO
        LBSR    WRTLAT                  ; F090: 17 01 2E        GO TO WRITE LATCH
        CLRB                            ; F093: 5F             
        PULS    PC,X,D                  ; F094: 35 96          
; SIDE 1
ZF096   LDD     #$1010                  ; F096: CC 10 10        PREPARE A TO FORCE HEAD 1 / B TO SET HEAD TO 0 AND KEEP EVERYTHING ELSE -> HEAD 1
        BRA     ZF08E                   ; F099: 20 F3          

; SWITCH DRIVE DENSITY
SWIDST  PSHS    X,A                     ; F09B: 34 12          
        LDB     ,U                      ; F09D: E6 C4           LOAD CURRENT DRIVE
        LEAX    $08,U                   ; F09F: 30 48           COMPUTE DENSITY TABLE ADDRESS
        TST     B,X                     ; F0A1: 6D 85           TEST IF DENSITY SD FOR THIS DRIVE
        BEQ     ZF0AC                   ; F0A3: 27 07           IF YES BRANCH
        CLR     B,X                     ; F0A5: 6F 85           COME HERE IF DD - SWITCH TO SD
        LBSR    ZF1B9                   ; F0A7: 17 01 0F        GO TO WRTLAT / SD
        BRA     ZF0B1                   ; F0AA: 20 05          
ZF0AC   INC     B,X                     ; F0AC: 6C 85           COME HERE IF SD - SWITCH TO DD
        LBSR    ZF1BE                   ; F0AE: 17 01 0D        GO TO WRTLAT / DD
ZF0B1   INC     $0D,U                   ; F0B1: 6C 4D           KEEP TRACE THAT DENSITY HAS BEEN CHANGED
        PULS    PC,X,A                  ; F0B3: 35 92          

; READ    THIS ROUTINE READS THE SPECIFIED SECTOR INTO MEMORY AT THE
; SPECIFIED ADDRESS. THIS ROUTINE SHOULD PERFORM A SEEK
; OPERATION IF NECESSARY. A SECTOR IS 256 BYTES IN LENGTH.

; ENTRY - (X) = ADDRESS IN MEMORY WHERE SECTOR IS TO BE PLACED.
; (A) = TRACK NUMBER
; (B) = SECTOR NUMBER

; EXIT -  (X) MAY BE DESTROYED
; (A) MAY BE DESTROYED
; (B) = ERROR CONDITION
; (Z) = 1 IF NO ERROR
; = 0 IF AN ERROR

READ    STD     $02,U                   ; F0B5: ED 42           SAVE TRACK & SECTOR
        STB     FDCSEC                  ; F0B7: F7 EC 06        SECTOR REG
        BSR     STLASI                  ; F0BA: 8D B9           SET LATCH FOR DRIVE & SIDE
        CLR     $0E,U                   ; F0BC: 6F 4E           CLEAR TRY COUNTER
        CMPA    FDCTRK                  ; F0BE: B1 EC 05        TRACK REG
        BEQ     ZF0C8                   ; F0C1: 27 05          
ZF0C3   LBSR    SEEK                    ; F0C3: 17 FF 7C        SEEK TRACK
        BNE     ZF0E4                   ; F0C6: 26 1C          
ZF0C8   LDA     #$80                    ; F0C8: 86 80           READ SECTOR COMMAND
        PSHS    CC                      ; F0CA: 34 01          
        ORCC    #$50                    ; F0CC: 1A 50           MASK IRQ AND FIRQ
        STA     FDC                     ; F0CE: B7 EC 04        ISSUE COMMAND
        LBSR    TEMPO                   ; F0D1: 17 01 32        FOR CMD EXECUTION
        BRA     ZF0DB                   ; F0D4: 20 05          
ZF0D6   LDA     FDCDAT                  ; F0D6: B6 EC 07       
        STA     ,X+                     ; F0D9: A7 80          
ZF0DB   LDA     FDCSTA                  ; F0DB: B6 EC 08       
        BMI     ZF0D6                   ; F0DE: 2B F6           CONTINUE DATA READ
        BEQ     ZF0DB                   ; F0E0: 27 F9           CONTINUE TO WAIT ABOUT STATUS
        PULS    CC                      ; F0E2: 35 01          
ZF0E4   LDB     FDC                     ; F0E4: F6 EC 04        LOAD STATUS REG
        ANDB    #$1C                    ; F0E7: C4 1C           READ OK MASK
        BEQ     ZF0F5                   ; F0E9: 27 0A           IF OK RETURN WITH Z=1
        TST     $0E,U                   ; F0EB: 6D 4E          
        BNE     ZF0F5                   ; F0ED: 26 06           RETURN WITH Z=0 IF $0E,U NOT 0
        INC     $0E,U                   ; F0EF: 6C 4E           INC TRY COUNTER
        LDD     $02,U                   ; F0F1: EC 42           RELOAD CURRENT TRACK AND SECTOR
        BRA     ZF0C3                   ; F0F3: 20 CE           DO AGAIN ONE TIME
ZF0F5   RTS                             ; F0F5: 39             

; WRITE   THIS ROUTINE WRITES THE INFORMATION FROM THE SPECIFED MEMORY
; BUFFER AREA TO THE DISK SECTOR SPECIFIED. THIS ROUTINE SHOULD
; PERFORM A SEEK OPERATION IF NECESSARY. A SECTOR IS 256 BYTES
; IN LENGTH.

; ENTRY - (X) = ADDRESS OF 256 MEMORY BUFFER CONTAINING DATA
; TO BE WRITTEN TO DISK
; (A) = TRACK NUMBER
; (B) = SECTOR NUMBER

; EXIT -  (X) MAY BE DESTROYED
; (A) MAY BE DESTROYED
; (B) = ERROR CONDITION
; (Z) = 1 IF NO ERROR
; = 0 IF AN ERROR

WRITE   STD     $02,U                   ; F0F6: ED 42           SAVE TRACK & SECTOR
        STB     FDCSEC                  ; F0F8: F7 EC 06        SELECT SECTOR IN FDC
        LBSR    STLASI                  ; F0FB: 17 FF 77        SET LATCH FOR DRIVE & SIDE
        CLR     $0E,U                   ; F0FE: 6F 4E          
        CMPA    FDCTRK                  ; F100: B1 EC 05        TRACK REG
        BEQ     ZF10A                   ; F103: 27 05          
ZF105   LBSR    SEEK                    ; F105: 17 FF 3A        SEEK TRACK
        BNE     ZF126                   ; F108: 26 1C          
ZF10A   LDA     #$A2                    ; F10A: 86 A2           WRITE SECTOR WITH SIDE CMP
        PSHS    CC                      ; F10C: 34 01          
        ORCC    #$50                    ; F10E: 1A 50           MASK IRQ AND FIRQ
        STA     FDC                     ; F110: B7 EC 04        ISSUE COMMAND
        LBSR    TEMPO                   ; F113: 17 00 F0        FOR CMD EXECUTION
        BRA     ZF11B                   ; F116: 20 03          
ZF118   STA     FDCDAT                  ; F118: B7 EC 07        STORE DATA TO WRITE
ZF11B   LDA     ,X+                     ; F11B: A6 80           LOAD NEXT DATA
ZF11D   LDB     FDCSTA                  ; F11D: F6 EC 08       
        BMI     ZF118                   ; F120: 2B F6           TEST DRQ
        BEQ     ZF11D                   ; F122: 27 F9           LOOP IF NOT
        PULS    CC                      ; F124: 35 01           RESTORE CODE CONDITION
ZF126   LDB     FDC                     ; F126: F6 EC 04        LOAD STATUS REG
        ANDB    #$5C                    ; F129: C4 5C           WRITE ERROR MASK
        BEQ     ZF137                   ; F12B: 27 0A           RETURN IF OK WITH Z=1
        TST     $0E,U                   ; F12D: 6D 4E           TEST IF FIRST OR SECOND TRY
        BNE     ZF137                   ; F12F: 26 06           RETURN AFTER TWO TRY WITH Z=0
        INC     $0E,U                   ; F131: 6C 4E           INC TRY COUNTER
        LDD     $02,U                   ; F133: EC 42           RELOAD CURRENT TRACK AND SECTOR
        BRA     ZF105                   ; F135: 20 CE           DO AGAIN ONE TIME
ZF137   RTS                             ; F137: 39             

; VERIFY  THE SECTOR JUST WRITTEN TO THE DISK IS TO BE VERIFIED TO
; DETERMINE IF THERE ARE CRC ERRORS. NO SEEK IS REQUIRED AS
; THIS ROUTINE WILL ONLY BE CALLED IMMEDIATELY AFTER A WRITE
; SINGLE SECTOR OPERATION.

; ENTRY - NO ENTRY PARAMETERS

; EXIT -  (X) MAY BE DESTROYED
; (A) MAY BE DESTROYED
; (B) = ERROR CONDITION
; (Z) = 1 IF NO ERROR
; = 0 IF AN ERROR

VERIF   LDA     #$80                    ; F138: 86 80           READ SECTOR COMMAND
        PSHS    CC                      ; F13A: 34 01          
        ORCC    #$50                    ; F13C: 1A 50          
        LBSR    WATFDC                  ; F13E: 17 00 21        WAIT FOR FDC NOT BUSY
        STA     FDC                     ; F141: B7 EC 04        ISSUE READ SECTOR CMD
        LBSR    TEMPO                   ; F144: 17 00 BF       
        LBSR    WATFDC                  ; F147: 17 00 18        WAIT FOR FDC NOT BUSY
        PULS    CC                      ; F14A: 35 01          
        LDB     FDC                     ; F14C: F6 EC 04        LOAD STATUS REG
        ANDB    #$18                    ; F14F: C4 18           TEST IF NO ERROR(CRC ERROR & RECORD NOT FOUND)
        RTS                             ; F151: 39              RETURN WITH Z=1 IF OK OR Z=0 IF ERROR

; RESTORE A RESTORE OPERATION (ALSO KNOWN AS A "SEEK TO TRACK 00") IS TO
; BE PERFORMED ON THE SPECIFIED DRIVE. THE DRIVE IS SPECIFIED
; IN THE FCB POINTED TO BY THE CONTENTS OF THE X REGISTER. NOTE
; THAT THE DRIVE NUMBER IS THE 4TH BYTE OF THE FCB. THIS
; ROUTINE SHOULD SELECT THE DRIVE BEFORE EXECUTING THE RESTORE
; OPERATION.

; ENTRY - (X) = FCB ADDRESS (3,X CONTAINS DRIVE NUMBER)

; EXIT -  (X) MAY BE DESTROYED
; (A) MAY BE DESTROYED
; (B) = ERROR CONDITION
; (Z) = 1 IF NO ERROR
; = 0 IF AN ERROR

RESTOR  BSR     WATFDC                  ; F152: 8D 0E           WAIT FOR FDC NOT BUSY
        LDB     #$00                    ; F154: C6 00           RESTORE CMD
        ORB     $01,U                   ; F156: EA 41           ADD STEP RATE TO COMMAND
        STB     FDC                     ; F158: F7 EC 04        ISSUE CMD
        LBSR    TEMPO                   ; F15B: 17 00 A8        FOR CMD EXECUTION
        CLRA                            ; F15E: 4F             
        LBRA    SEEK                    ; F15F: 16 FE E0        SEEK TRACK

; WAIT FOR FDC NOT BUSY
WATFDC  PSHS    B                       ; F162: 34 04          
ZF164   LDB     FDC                     ; F164: F6 EC 04        LOAD STATUS REG
        ANDB    #$01                    ; F167: C4 01           TEST BUSY
        BNE     ZF164                   ; F169: 26 F9           LOOP IF BUSY
        PULS    PC,B                    ; F16B: 35 84           RESTORE B AND RETURN

; SAVES TRACK NUMBER IN TABLE DE80-DE91
; TABLE MANAGED BY U REGISTER
; 0 : CURRENT DRIVE
; 1 : STEP RATE %00000011 = 30 MS
; 2 : CURRENT TRACK
; 3 : CURRENT SECTOR
; 4 : COPY OF DRIVE LATCH OUTPUT
; 5 : DRV 0 TRACK MEMORY
; 6 : DRV 1 TRACK MEMORY
; 7 : DRV 2 TRACK MEMORY
; 8 : DRV 0 DENSITY 1=DD 0=SD
; 9 : DRV 1 DENSITY
; A : DRV 2 DENSITY
; B : HEAD IN USE $00 HEAD 0 / $10 HEAD 1
; C : USE DURING BOOT - NUMBER OF TRY
; D : TRY NUMBER DURING SEEK
; E : TRY NUMBER DURING READ AND WRITE
; F : DD SECTOR PER SIDE D0
; 10 : DD SECTOR PER SIDE D1
; 11 : DD SECTOR PER SIDE D2

; STORE CURRENT DRIVE TRACK IN REPO
STTRSI  PSHS    X                       ; F16D: 34 10          
        LEAX    $05,U                   ; F16F: 30 45           LOAD X WITH TRACK TABLE BEGIN U+5
        LDA     ,U                      ; F171: A6 C4           LOAD CURRENT DRIVE NUMBER
        LDB     FDCTRK                  ; F173: F6 EC 05        LOAD FDC TRACK REG
        STB     A,X                     ; F176: E7 86           STORE INTO TABLE AT U+5+DRV_NUMBER
        PULS    X                       ; F178: 35 10          
        LDD     #$000F                  ; F17A: CC 00 0F        PREPARE A FOR NO CHANGE IN DENSITY AND HEAD / B FOR DESELECT ALL DRIVES
        BRA     WRTLAT                  ; F17D: 20 42           BRANCH TO WRITE LATCH

; TABLE FOR DRIVE SELECT LATCH CODES
MF17F   FCB     $01,$02,$04             ; F17F: 01 02 04        OBVIOUSLY THE SYSTEM IS LIMITED TO 3 DRIVES

; DRIVE   THE SPECIFIED DRIVE IS TO BE SELECTED. THE DRIVE IS SPECIFIED
; IN THE FCB POINTED TO BY THE CONTENTS OF THE X REGISTER. NOTE
; THAT THE DRIVE NUMBER IS THE 4TH BYTE OF THE FCB.

; ENTRY - (X) = FCB ADDRESS (3,X CONTAINS DRIVE NUMBER)

; EXIT -  (X) MAY BE DESTROYED
; (A) MAY BE DESTROYED
; (B) = $0F IF NON-EXISTENT DRIVE
; = ERROR CONDITION OTHERWISE
; (Z) = 1 IF NO ERROR
; = 0 IF AN ERROR
; (C) = 0 IF NO ERROR
; = 1 IF AN ERROR

DRVSEL  PSHS    X,A                     ; F182: 34 12          
        BSR     STTRSI                  ; F184: 8D E7           SAVES CURRENT DRIVE TRACK NUMBER IN REPO AND DESELECT ALL DRIVES
        PULS    A                       ; F186: 35 02           GET NEW DRIVE NUMBER
        CMPA    #$03                    ; F188: 81 03           ENSURE IT'S < 3
        BCC     ERRDN                   ; F18A: 24 27           BRANCH TO ERROR IF A>=3
        LEAX    $05,U                   ; F18C: 30 45          
        LDB     A,X                     ; F18E: E6 86           GET NEW DRIVE TRACK NUMBER
        STB     FDCTRK                  ; F190: F7 EC 05        TO FDC TRACK REG
        STA     ,U                      ; F193: A7 C4           STORE NEW DRIVE NUMBER IN TAB (POS 0)
        LEAX    MF17F,PCR               ; F195: 30 8C E7        LATCH CODES TABLE ADDRESS
        LDA     A,X                     ; F198: A6 86           LOAD LATCH CODE DEPENDING OF DRIVE NUMBER
        PSHS    A                       ; F19A: 34 02           STORE
        LEAX    $08,U                   ; F19C: 30 48           COMPUTE DRIVE DENSITY TABLE ADDRESS
        LDB     ,U                      ; F19E: E6 C4           LOAD NEW DRIVE NUMBER
        LDB     B,X                     ; F1A0: E6 85           LOAD DENSITY FOR NEW DRIVE IN B
        BEQ     ZF1A8                   ; F1A2: 27 04           BRANCH IF DENSITY IS ZERO
        BSR     ZF1BE                   ; F1A4: 8D 18           COME HERE IF DENSITY IS NOT 0 - WILL SET DD
        BRA     ZF1AB                   ; F1A6: 20 03          
ZF1A8   LBSR    ZF1B9                   ; F1A8: 17 00 0E        COME HERE IF DENSITY IS 0 - WILL SET SD
ZF1AB   PULS    X,A                     ; F1AB: 35 12           RESTORE CODE FOR LATCH IN A
        LDB     #$0F                    ; F1AD: C6 0F           PREPARE B FOR NO CHANGE IN LATCH MSB AND SET LSB TO 0
        BSR     WRTLAT                  ; F1AF: 8D 10           GO TO WRITE LATCH
        CLRB                            ; F1B1: 5F              CLEAR B SET Z=1 CLEAR C
        RTS                             ; F1B2: 39              OK

; ERROR IN DRIVE NUMBER
ERRDN   PULS    X                       ; F1B3: 35 10          
        LDB     #$F0                    ; F1B5: C6 F0           LOAD ERROR CODE INVERTED Z=0
        COMB                            ; F1B7: 53              INVERT CODE AND SET C
        RTS                             ; F1B8: 39              RETURN NOT OK

ZF1B9   LDD     #$4040                  ; F1B9: CC 40 40        PREPARE A TO FORCE SD / B TO SET DENSITY TO 0 AND KEEP EVERYTHING ELSE -> SD
        BRA     WRTLAT                  ; F1BC: 20 03           GO TO SELECT DRIVE AND SIDE
ZF1BE   LDD     #$0040                  ; F1BE: CC 00 40        PREPARE A TO NO CHANGE IN DENSITY / B TO SET DENSITY TO 0 AND KEEP EVERYTHING ELSE -> DD

; SELECT DRIVE & SIDE
WRTLAT  PSHS    D                       ; F1C1: 34 06           PUSH A THEN B TO STACK
        ANDA    $01,S                   ; F1C3: A4 61           DO A AND B -> A
        COMB                            ; F1C5: 53              B LOGICAL COMPLEMENT
        ANDB    $04,U                   ; F1C6: E4 44           AND WITH PREVIOUS LATCH STATE - KEEP BITS WHERE B BIT WAS 0 AT CALL
        STB     $04,U                   ; F1C8: E7 44           STORE
        ORA     $04,U                   ; F1CA: AA 44           ADD LATCH CODE / HEAD BIT / DENSITY BIT FOR SELECTED DRIVE
        STA     $04,U                   ; F1CC: A7 44           STORE NEW LATCH CODE IN REPO
        STA     DRVLAT                  ; F1CE: B7 EC 0C        WRITE NEW CODE TO LATCH
        PULS    PC,D                    ; F1D1: 35 86          

; INIT DRIVES TABLE AND TRACK REG
INITAB  ANDB    #$03                    ; F1D3: C4 03           B=STEP RATE / KEEP ONLY B0 & B1
        LDU     #DRVTAB                 ; F1D5: CE DE 80       
        STB     $01,U                   ; F1D8: E7 41           STORE STEP RATE IN REPO
        CLR     $04,U                   ; F1DA: 6F 44           CLEAR DRIVE SELECT LATCH COPY
        CLR     DRVLAT                  ; F1DC: 7F EC 0C        CLEAR DRIVE SELECT LATCH
        CLR     $0B,U                   ; F1DF: 6F 4B          
        CLR     ,U                      ; F1E1: 6F C4           CLEAR CURRENT DRIVE
        CLR     $05,U                   ; F1E3: 6F 45           CLEAR DRIVE 0 TRACK MEMORY
        LDA     #$01                    ; F1E5: 86 01          
        STA     $08,U                   ; F1E7: A7 48           SET DENSITY DD FOR DRIVE 0
        STA     $09,U                   ; F1E9: A7 49           FOR DRIVE 1
        STA     $0A,U                   ; F1EB: A7 4A           FOR DRIVE 2
        LDD     #$1010                  ; F1ED: CC 10 10        STORE DD SECTOR BY SIDE D0 & D1
        STD     $0F,U                   ; F1F0: ED 4F           STORE DD SECTOR BY SIDE D2
        STA     $11,U                   ; F1F2: A7 C8 11       
        LDA     #$01                    ; F1F5: 86 01           RESTORE DRIVES 01 AND 00
ZF1F7   PSHS    A                       ; F1F7: 34 02           STORE DRIVE NUMBER
        LBSR    DRVSEL                  ; F1F9: 17 FF 86        SELECT DRIVE
        PULS    A                       ; F1FC: 35 02           RESTORE DRIVE NUMBER
        BSR     DRVRST                  ; F1FE: 8D 10           RESTORE DRIVE
        DECA                            ; F200: 4A              A-1->A
        BPL     ZF1F7                   ; F201: 2A F4           LOOP - BRANCH IF N CLEAR (NOT NEGATIVE)
        CLR     FDCTRK                  ; F203: 7F EC 05        CLEAR TRACK REG

; TEMPO FOR CMD EXECUTION
TEMPO   LBSR    ZF209                   ; F206: 17 00 00       
ZF209   LBSR    ZF20C                   ; F209: 17 00 00       
ZF20C   LBSR    ZF20F                   ; F20C: 17 00 00       
ZF20F   RTS                             ; F20F: 39             

; THIS ROUTINE IS CALL FOR D0 AND D1 ONLY AT INIT
; DO A RESTORE AND A IN DELAY TRACK 00 VERIFY
DRVRST  PSHS    A                       ; F210: 34 02          
        LDB     #$00                    ; F212: C6 00           DO DRIVE RESTORE
        ORB     $01,U                   ; F214: EA 41           ADD STEP RATE
        STB     FDC                     ; F216: F7 EC 04        SEND COMMAND TO FDC
        LDX     #$5000                  ; F219: 8E 50 00       
        LDA     $01,U                   ; F21C: A6 41           LOAD STEP RATE
        INCA                            ; F21E: 4C             
        ASLA                            ; F21F: 48             
        ASLA                            ; F220: 48             
        ASLA                            ; F221: 48             
        ASLA                            ; F222: 48             
        LDB     #$FA                    ; F223: C6 FA           $FA = 250
        MUL                             ; F225: 3D              A*B -> A:B (D)
        LEAX    D,X                     ; F226: 30 8B           COMPUTE A*B+$5000 -> X
ZF228   BSR     TEMPO                   ; F228: 8D DC           FOR CMD EXECUTION
        LDB     FDC                     ; F22A: F6 EC 04        LOAD STATUS REG
        ANDB    #$04                    ; F22D: C4 04           TEST IF TRACK 00
        BNE     ZF235                   ; F22F: 26 04           BRANCH IF TRACK 0 REACHED
        LEAX    -$01,X                  ; F231: 30 1F           DECREASE X
        BNE     ZF228                   ; F233: 26 F3           WAIT AGAIN AND RE CHECK TIL X=0
ZF235   LDA     #$D0                    ; F235: 86 D0           FORCE INTERRUPT CMD
        STA     FDC                     ; F237: B7 EC 04        SEND COMMAND TO FDC
        PULS    PC,A                    ; F23A: 35 82          

; LOAD BOOT SECTOR & JUMP
FLXBOT  LDB     #$00                    ; F23C: C6 00           PREPARE FOR STEP RATE 6 MS
        LDS     #INPBUF                 ; F23E: 10 CE C0 80     ?? LOAD FLEX INPUT BUFFER ADDRESS IN S
        LBSR    INITAB                  ; F242: 17 FF 8E        INIT TABLE + TRACK REG
        LDA     #$03                    ; F245: 86 03           PREPARE FOR 3 LOAD TRY
        STA     $0C,U                   ; F247: A7 4C          
ZF249   LBSR    RESTOR                  ; F249: 17 FF 06       
        LDX     #CMDAD2                 ; F24C: 8E C2 00        BOOT LOADER LOADED AT $C200
        LDD     #$0002                  ; F24F: CC 00 02        TRACK 0 SECTOR 2
        STD     -$02,X                  ; F252: ED 1E          
        STD     $00FE,X                 ; F254: ED 89 00 FE    
        BSR     CHKFLX                  ; F258: 8D 24          
        BNE     ZF269                   ; F25A: 26 0D           IF NO 'FLEX' FOUND BOOT LOADER STAY ONLY ON SECTOR 1
        LDX     #CMDAD1                 ; F25C: 8E C1 00        BOOT LOADER LOADED AT $C100
        LDD     #$0001                  ; F25F: CC 00 01        TRACK 0 SECTOR 1
        BSR     CHKFLX                  ; F262: 8D 1A          
        BNE     ZF276                   ; F264: 26 10          
        JMP     CMDAD2                  ; F266: 7E C2 00        JUMP TO BOOT LOADER $C200
ZF269   LDX     #CMDAD1                 ; F269: 8E C1 00        BOOT LOADER LOADED AT $C100
        LDD     #$0001                  ; F26C: CC 00 01        TRACK 0 SECTOR 1
        BSR     CHKFLX                  ; F26F: 8D 0D          
        BNE     ZF276                   ; F271: 26 03          
        JMP     CMDAD1                  ; F273: 7E C1 00        JUMP TO BOOT LOADER $C100
ZF276   DEC     $0C,U                   ; F276: 6A 4C           NO 'FLEX' STRING FOUND IN MEMORY
        BNE     ZF249                   ; F278: 26 CF           DO ANOTHER TRY
        JMP     [RESET]                 ; F27A: 6E 9F FF FE     3 SUCCESSIVE FAILS - RESET

; LOAD AND CHECK 'FLEX' IS IN MEMORY - RETURN Z=0 IF NOT FOUND
CHKFLX  LBSR    ZF0C3                   ; F27E: 17 FE 42        GO TO SECTOR READ ROUTINE
        BNE     ZF291                   ; F281: 26 0E           END ROUTINE IF READ FAIL
        LDD     ,--X                    ; F283: EC 83          
        CMPD    #$4558                  ; F285: 10 83 45 58     COMPARE TO 'EX'
        BNE     ZF291                   ; F289: 26 06          
        LDD     ,--X                    ; F28B: EC 83          
        CMPD    #$464C                  ; F28D: 10 83 46 4C     COMPARE TO 'FL'
ZF291   RTS                             ; F291: 39             

; TEST SERIAL DATA READY
DATRDY  PSHS    A                       ; F292: 34 02          
        LDA     ACIASR                  ; F294: B6 EF 60        LOAD STATUS REGISTER
        ANDA    #$01                    ; F297: 84 01           GET DATA RECEIVE DATA REG FULL
        PULS    PC,A                    ; F299: 35 82          

; INIT ACIA
INIACI  LDA     #$03                    ; F29B: 86 03           RESET ACIA CODE
        STA     ACIASR                  ; F29D: B7 EF 60        STORE INTO CONTROL REGISTER
        LDA     #$11                    ; F2A0: 86 11           SET CONTROL 8N2 div16
        STA     ACIASR                  ; F2A2: B7 EF 60        REGISTER UP
        RTS                             ; F2A5: 39             

; WAIT FOR AND GET SERIAL DATA
GETDTA  PSHS    B                       ; F2A6: 34 04           SAVE B REG
ZF2A8   LDB     ACIASR                  ; F2A8: F6 EF 60        LOAD STATUS REGISTER
        BITB    #$01                    ; F2AB: C5 01           TEST DATA RECEIVE DATA REG FULL
        BEQ     ZF2A8                   ; F2AD: 27 F9           LOOP IF EMPTY
        LDA     ACIADR                  ; F2AF: B6 EF 61        LOAD DATA BYTE
        ANDB    #$70                    ; F2B2: C4 70           CHECK ERRORS
        BNE     ZF2A8                   ; F2B4: 26 F2           LOOP IF ERROR
        ANDA    #$7F                    ; F2B6: 84 7F           MASK FOR 7 BITS DATA
        PULS    PC,B                    ; F2B8: 35 84           RESTORE B REG AND RETURN

; GET SERIAL DATA WITH ECHO
GDTAEC  BSR     GETDTA                  ; F2BA: 8D EA           GET DATA

; SEND SERIAL DATA WITH ACIA
WRTDTA  PSHS    A                       ; F2BC: 34 02           SAVE REG A
ZF2BE   LDA     ACIASR                  ; F2BE: B6 EF 60        LOAD STATUS REGISTER
        BITA    #$02                    ; F2C1: 85 02           TEST TRANSMIT DATA REG EMPTY
        BEQ     ZF2BE                   ; F2C3: 27 F9           LOOP IF NOT EMPTY
        PULS    A                       ; F2C5: 35 02           RESTORE REG A
        STA     ACIADR                  ; F2C7: B7 EF 61        SEND TO TRANSMIT DATA REG
        RTS                             ; F2CA: 39             

; WAIT AND LOAD DATA FROM VIA
WTLDVI  LDA     VIA_13                  ; F2CB: B6 EF 8D        LOAD IFR REG
        BITA    #$02                    ; F2CE: 85 02           TEST IF CAR READY
        BEQ     WTLDVI                  ; F2D0: 27 F9           LOOP IF EMPTY
        LDA     VIA_1                   ; F2D2: B6 EF 81        LOAD DATA
        RTS                             ; F2D5: 39             

; GET PARALLEL DATA FROM VIA WITH ECHO
GDTVIE  BSR     WTLDVI                  ; F2D6: 8D F3           GET PARALLEL DATA
        LBSR    JMPADR                  ; F2D8: 17 00 0A        DISPLAY CHAR
        RTS                             ; F2DB: 39             

; TEST PARALLEL DATA READY OUTPUT YES Z=1 NO Z=0
VIARDY  PSHS    A                       ; F2DC: 34 02           SAVE REG A
        LDA     VIA_13                  ; F2DE: B6 EF 8D        LOAD STATUS REGISTER
        BITA    #$02                    ; F2E1: 85 02           TEST IF CAR READY
        PULS    PC,A                    ; F2E3: 35 82           RESTORE REG A AND RETURN

; JUMP TO ADDRESS (DEPENDING OF THE CHAR RECEIVED)
JMPADR  JMP     [GRADDR]                ; F2E5: 6E 9F DE 92    

; INIT OF THE CRTC 6845
INICRT  PSHS    X,D                     ; F2E9: 34 16          
        CLRA                            ; F2EB: 4F             
        LDX     #MF310                  ; F2EC: 8E F3 10        LOAD PARAMETER TABLE
ZF2EF   STA     CRTC                    ; F2EF: B7 EC 00        STORE REG NUMBER IN CRTC
        LDB     ,X+                     ; F2F2: E6 80           LOAD PARAMETER
        STB     CRTC_1                  ; F2F4: F7 EC 01        STORE PARAMETER IN CRTC
        INCA                            ; F2F7: 4C              NEXT REG
        CMPA    #$10                    ; F2F8: 81 10           END OF TABLE
        BNE     ZF2EF                   ; F2FA: 26 F3           LOOP
        LDX     #$0000                  ; F2FC: 8E 00 00       
        STX     CURPOS                  ; F2FF: BF DE 96       
        STX     SCORAD                  ; F302: BF DE 94       
        STX     MDE9A                   ; F305: BF DE 9A       
        STX     CURROW                  ; F308: BF DE 98       
        CLR     MDEA9                   ; F30B: 7F DE A9       
        PULS    PC,X,D                  ; F30E: 35 96          

; CRTC INIT PARAMETERS (ORIGINAL FROM ELEKTOR)
MF310   FCB     $63                     ; F310: 63              TOTAL HORIZONTAL
        FCB     $50                     ; F311: 50              AFFICHAGE HORIZONTAL 80
        FCB     $57                     ; F312: 57              H SYNC POSITION 100
        FCB     $03                     ; F313: 03              LARGEUR SYNC
        FCB     $19                     ; F314: 19              TOTAL VERTICAL
        FCB     $07                     ; F315: 07              V TOTAL ADJUST
        FCB     $18                     ; F316: 18              AFFICHAGE VERTICAL 24
        FCB     $18                     ; F317: 18              V SYNC POSITION
        FCB     $00                     ; F318: 00              MODE INTERLACE 0
        FCB     $0B                     ; F319: 0B              LINE MAX SCAN ADRESSE
        FCB     $68                     ; F31A: 68              CURSOR START
        FCB     $0B                     ; F31B: 0B              CURSOR END
        FCB     $00                     ; F31C: 00              START H
        FCB     $00                     ; F31D: 00              START L
        FCB     $00                     ; F31E: 00              CURSOR START H
        FCB     $00                     ; F31F: 00              CURSOR START L
        FCB     $50                     ; F320: 50             
        FCB     $17                     ; F321: 17             

; THIS IS 6845 CRTC PHILIPPE'S TABLE FOR LCD SCREEN
; MF310   FCB     $69                     ; TOTAL HORIZONTAL
; FCB     $50                     ; AFFICHAGE HORIZONTAL 80
; FCB     $57                     ; H SYNC POSITION 100
; FCB     $08                     ; LARGEUR SYNC
; FCB     $21                     ; TOTAL VERTICAL
; FCB     $06                     ; V TOTAL ADJUST
; FCB     $18                     ; AFFICHAGE VERTICAL 24
; FCB     $1C                     ; V SYNC POSITION
; FCB     $00                     ; MODE INTERLACE 0
; FCB     $09                     ; LINE MAX SCAN ADRESSE
; FCB     $60                     ; CURSOR START
; FCB     $09                     ; CURSOR END
; FCB     $00                     ; START H
; FCB     $00                     ; START L
; FCB     $00                     ; CURSOR START H
; FCB     $00                     ; CURSOR START L
; FCB     $50
; FCB     $17

; TABLE USED FOR SCREEN MANAGEMENT
; $DE92     ADDRESS TO BE CALLED FOR NEXT RECEIVED CHAR MANAGEMENT MSB  GRADDR
; $DE93     ADDRESS TO BE CALLED FOR NEXT RECEIVED CHAR MANAGEMENT LSB
; $DE94		SCREEN ORIGINE ADDRESS IN RAM LSB							SCORAD
; $DE95		SCREEN ORIGINE ADDRESS IN RAM MSB
; $DE96		CURSOR ADDRESS IN RAM LSB									CURPOS
; $DE97		CURSOR ADDRESS IN RAM MSB
; $DE98		CURSOR ROW													CURROW	
; $DE99		CURSOR LINE													CURLIN
; $DEA0
; $DEA1
; $DEA2
; $DEA3
; $DEA4
; $DEA5
; $DEA6
; $DEA7
; $DEA8
; $DEA9

; SCREEN CLEAR (SET X TO 0 AND Y TO $7FF AND CALL PUTSXY)
DELSCR  PSHS    Y                       ; F322: 34 20          
        LDX     #$0000                  ; F324: 8E 00 00       
        LDY     #$07FF                  ; F327: 10 8E 07 FF    
        JSR     PUTSXY                  ; F32B: BD F4 42       
        PULS    PC,Y                    ; F32E: 35 A0          

; WRITE ORIGIN ADDRESS AND CURSOR POSITION INTO CRTC
CURCRT  LDB     #$0C                    ; F330: C6 0C           $0C = 12 LOAD CURSOR ADDRESS IN CRTC RAM
        LDX     #SCORAD                 ; F332: 8E DE 94        CURSOR ADDRESS IN RAM
ZF335   LDA     ,X+                     ; F335: A6 80           DO IT 4X SO LOAD SCORAD, SCORAD+1, CURPOS, CURPOS+1 (DE94-DE97)
        STB     CRTC                    ; F337: F7 EC 00       
        STA     CRTC_1                  ; F33A: B7 EC 01       
        INCB                            ; F33D: 5C             
        CMPB    #$10                    ; F33E: C1 10           STOP BEFORE CRTC ADDRESS 16
        BNE     ZF335                   ; F340: 26 F3          
        RTS                             ; F342: 39             

; DEAL WITH ONE LINE UP
ZF343   JSR     ZF664                   ; F343: BD F6 64       
        DEC     CURLIN                  ; F346: 7A DE 99       
        LDD     SCORAD                  ; F349: FC DE 94       
        ADDD    #$0050                  ; F34C: C3 00 50       
        STD     SCORAD                  ; F34F: FD DE 94       
        BRA     CURCRT                  ; F352: 20 DC          

ZF354   TST     MDEA4                   ; F354: 7D DE A4       
        BNE     CHRVID                  ; F357: 26 46          
        PSHS    A                       ; F359: 34 02          
        LDD     CURPOS                  ; F35B: FC DE 96       
        ANDA    #$07                    ; F35E: 84 07           MAX IS $7FF
        TFR     D,X                     ; F360: 1F 01          
        PULS    A                       ; F362: 35 02          
        BRA     ZF3E0                   ; F364: 20 7A          

; CODE ESC z - PERFORM VARIOUS SCREEN AND VIA INIT
ESC_z   JSR     INICRT                  ; F366: BD F2 E9        GO TO 6845 INITIALIZE
        CLR     MDEA4                   ; F369: 7F DE A4       
        CLR     MDEA5                   ; F36C: 7F DE A5       
        CLR     MDEA7                   ; F36F: 7F DE A7       
        LDA     #$68                    ; F372: 86 68          
        STA     MDEA8                   ; F374: B7 DE A8       
        JSR     ESC_G                   ; F377: BD F5 AC       
        LDA     #$49                    ; F37A: 86 49          
        JSR     VIAINI                  ; F37C: BD F6 90       
        JMP     ESC_E                   ; F37F: 7E F6 3E        GO TO CLEAR SCREEN AND CURSOR TO HOME

; ? MAY BE WARM
INIT    BSR     ESC_z                   ; F382: 8D E2           GO TO SCREEN INIT
        CLR     DRVLAT                  ; F384: 7F EC 0C        CLEAR DRIVE SELECT LATCH
        LDD     #CHRSCR                 ; F387: CC F7 02       
        STD     GRADDR                  ; F38A: FD DE 92        SET GRADDR TO $F702 FOR NEXT CHAR
        RTS                             ; F38D: 39             

ZF38E   LDD     SCORAD                  ; F38E: FC DE 94       
        SUBD    #$0050                  ; F391: 83 00 50       
        STD     SCORAD                  ; F394: FD DE 94       
        JSR     ZF667                   ; F397: BD F6 67       
        INC     CURLIN                  ; F39A: 7C DE 99       
        BRA     CURCRT                  ; F39D: 20 91          

; HERE THE CHAR IS PUT INTO VIDEO RAM
CHRVID  PSHS    A                       ; F39F: 34 02          
        LDX     CURPOS                  ; F3A1: BE DE 96       
        LDB     #$4F                    ; F3A4: C6 4F          
        SUBB    CURROW                  ; F3A6: F0 DE 98       
        PSHS    B                       ; F3A9: 34 04          
        LEAX    B,X                     ; F3AB: 30 85          
        JSR     LIMITX                  ; F3AD: BD F5 F8       
        PULS    B                       ; F3B0: 35 04          
        TSTB                            ; F3B2: 5D             
        BEQ     ZF3DE                   ; F3B3: 27 29          

ZF3B5   LEAX    -$01,X                  ; F3B5: 30 1F          
        CMPX    #$0000                  ; F3B7: 8C 00 00       
        BPL     ZF3CF                   ; F3BA: 2A 13          
        LDX     #$07FF                  ; F3BC: 8E 07 FF       
        LDA     VIDRAM,X                ; F3BF: A6 89 E0 00    
        LDX     #$0000                  ; F3C3: 8E 00 00       
        STA     VIDRAM,X                ; F3C6: A7 89 E0 00    
        LDX     #$07FF                  ; F3CA: 8E 07 FF       
        BRA     ZF3DB                   ; F3CD: 20 0C          

ZF3CF   LDA     VIDRAM,X                ; F3CF: A6 89 E0 00    
        LEAX    $01,X                   ; F3D3: 30 01          
        STA     VIDRAM,X                ; F3D5: A7 89 E0 00    
        LEAX    -$01,X                  ; F3D9: 30 1F          
ZF3DB   DECB                            ; F3DB: 5A             
        BNE     ZF3B5                   ; F3DC: 26 D7          
ZF3DE   PULS    A                       ; F3DE: 35 02          

ZF3E0   CMPA    #$5E                    ; F3E0: 81 5E          
        BCS     ZF3E7                   ; F3E2: 25 03          
        ADDA    MDEA6                   ; F3E4: BB DE A6       
ZF3E7   ORA     MDEA5                   ; F3E7: BA DE A5       
        STA     VIDRAM,X                ; F3EA: A7 89 E0 00    
        LDB     CURROW                  ; F3EE: F6 DE 98       
        CMPB    #$4F                    ; F3F1: C1 4F          
        LBCS    CURRIG                  ; F3F3: 10 25 00 BE    
        TST     MDEA7                   ; F3F7: 7D DE A7       
        LBEQ    ZF650                   ; F3FA: 10 27 02 52    
        JMP     CURRIG                  ; F3FE: 7E F4 B5       

; DELETE SCREEN FROM CURSOR TO END OF LINE ESC K
DELEOL  PSHS    Y                       ; F401: 34 20          
        LDX     CURPOS                  ; F403: BE DE 96       
        LDB     #$4F                    ; F406: C6 4F           $4F = 79 LAST ROW NUMBER
        SUBB    CURROW                  ; F408: F0 DE 98       
        LEAY    B,X                     ; F40B: 31 85          
        BSR     PUTSXY                  ; F40D: 8D 33          
        PULS    PC,Y                    ; F40F: 35 A0          

; CODE ESC o
ESC_o   PSHS    Y                       ; F411: 34 20          
        LDX     MDE9A                   ; F413: BE DE 9A       
ZF416   LDY     CURPOS                  ; F416: 10 BE DE 96    
        BSR     PUTSXY                  ; F41A: 8D 26          
        PULS    PC,Y                    ; F41C: 35 A0          

; CODE ESC l
ESC_l   PSHS    Y                       ; F41E: 34 20          
        LDX     MDE9A                   ; F420: BE DE 9A       
        LEAY    $4F,X                   ; F423: 31 88 4F       
        BSR     PUTSXY                  ; F426: 8D 1A          
        PULS    PC,Y                    ; F428: 35 A0          

; CODE ESC b
ESC_b   PSHS    Y                       ; F42A: 34 20          
        LDX     SCORAD                  ; F42C: BE DE 94       
        BRA     ZF416                   ; F42F: 20 E5          

; ERASE SCREEN FROM CURSOR TO END OF SCREEN ESC J
DELEOS  PSHS    Y                       ; F431: 34 20          
        LDX     CURPOS                  ; F433: BE DE 96       
        LDY     SCORAD                  ; F436: 10 BE DE 94    
        LEAY    $0780,Y                 ; F43A: 31 A9 07 80     $780 = 24 LINES X 80 ROWS
        BSR     PUTSXY                  ; F43E: 8D 02          
        PULS    PC,Y                    ; F440: 35 A0          

; FILL SCREEN WITH SPACE CHAR $20 FROM X TO Y
PUTSXY  JSR     LIMITX                  ; F442: BD F5 F8        LIMIT X TO $07FF
        TFR     Y,D                     ; F445: 1F 20          
        ANDA    #$07                    ; F447: 84 07           MAX FOR D IS $07FF
        PSHS    D                       ; F449: 34 06          
        LDA     #$20                    ; F44B: 86 20           LOAD SPACE CHAR IN A
ZF44D   STA     VIDRAM,X                ; F44D: A7 89 E0 00    
        LEAX    $01,X                   ; F451: 30 01          
        CMPX    #$0800                  ; F453: 8C 08 00        COMPARE TO END OF VIDEO RAM
        BNE     ZF45B                   ; F456: 26 03          
        LDX     #$0000                  ; F458: 8E 00 00        AFTER $0800 CONTINUE WITH $0000
ZF45B   CMPX    ,S                      ; F45B: AC E4           COMPARE X WITH LAST PUSH -> WITH D
        BNE     ZF44D                   ; F45D: 26 EE          
        STA     VIDRAM,X                ; F45F: A7 89 E0 00    
        PULS    PC,Y                    ; F463: 35 A0          

; CODE ESC N
ESC_N   LDD     CURPOS                  ; F465: FC DE 96       
        ANDA    #$07                    ; F468: 84 07           MAX IS $7FF
        TFR     D,X                     ; F46A: 1F 01          
        LDB     CURROW                  ; F46C: F6 DE 98       
ZF46F   CMPB    #$4F                    ; F46F: C1 4F          
        BEQ     ZF49D                   ; F471: 27 2A          
        CMPX    #$07FF                  ; F473: 8C 07 FF       
        BNE     ZF48C                   ; F476: 26 14          
        PSHS    X                       ; F478: 34 10          
        LDX     #$0000                  ; F47A: 8E 00 00       
        LDA     VIDRAM,X                ; F47D: A6 89 E0 00    
        PULS    X                       ; F481: 35 10          
        STA     VIDRAM,X                ; F483: A7 89 E0 00    
        LDX     #$0000                  ; F487: 8E 00 00       
        BRA     ZF49A                   ; F48A: 20 0E          
ZF48C   LEAX    $01,X                   ; F48C: 30 01          
        LDA     VIDRAM,X                ; F48E: A6 89 E0 00    
        LEAX    -$01,X                  ; F492: 30 1F          
        STA     VIDRAM,X                ; F494: A7 89 E0 00    
        LEAX    $01,X                   ; F498: 30 01          
ZF49A   INCB                            ; F49A: 5C             
        BRA     ZF46F                   ; F49B: 20 D2          
ZF49D   LDA     #$20                    ; F49D: 86 20          
        STA     VIDRAM,X                ; F49F: A7 89 E0 00    
        RTS                             ; F4A3: 39             

; MOVE CURSOR ONE CHAR LEFT ESC D
CURLEF  TST     CURROW                  ; F4A4: 7D DE 98        CHECK IF CURSOR AT FIRST ROW (0)
        BEQ     ZF4B4                   ; F4A7: 27 0B          
        DEC     CURROW                  ; F4A9: 7A DE 98       
        LDD     CURPOS                  ; F4AC: FC DE 96       
        SUBD    #$0001                  ; F4AF: 83 00 01       
        BRA     ZF4C5                   ; F4B2: 20 11          
ZF4B4   RTS                             ; F4B4: 39             

; MOVE CURSOR ONE CHAR RIGHT ESC C
CURRIG  LDA     CURROW                  ; F4B5: B6 DE 98       
        CMPA    #$4F                    ; F4B8: 81 4F           $4F=79 CHECK IF CURSOR AT LAST ROW
        BCC     ZF4B4                   ; F4BA: 24 F8          
        INC     CURROW                  ; F4BC: 7C DE 98       
        LDD     CURPOS                  ; F4BF: FC DE 96       
        ADDD    #$0001                  ; F4C2: C3 00 01       
ZF4C5   STD     CURPOS                  ; F4C5: FD DE 96       
        JMP     CURCRT                  ; F4C8: 7E F3 30       

; MOVE CURSOR ONE LINE DOWN ESC B
CURDWN  LDA     #$17                    ; F4CB: 86 17           $17 = 24 CHECK IF CURSOR AT LAST LINE
        CMPA    CURLIN                  ; F4CD: B1 DE 99       
        BLS     ZF4B4                   ; F4D0: 23 E2          
        INC     CURLIN                  ; F4D2: 7C DE 99       
        BSR     ZF4DA                   ; F4D5: 8D 03          
        JMP     CURCRT                  ; F4D7: 7E F3 30       

; DEAL WITH LINE FEED AND CURSOR DOWN
ZF4DA   LDD     CURPOS                  ; F4DA: FC DE 96       
        ADDD    #$0050                  ; F4DD: C3 00 50        $50 = DECIMAL 80
        STD     CURPOS                  ; F4E0: FD DE 96       
        LDD     MDE9A                   ; F4E3: FC DE 9A       
        ADDD    #$0050                  ; F4E6: C3 00 50       
        STD     MDE9A                   ; F4E9: FD DE 9A       
        RTS                             ; F4EC: 39             

; MOVE CURSOR ONE LINE UP ESC A
CURSUP  LDA     CURLIN                  ; F4ED: B6 DE 99       
        BEQ     ZF4B4                   ; F4F0: 27 C2          
ZF4F2   DEC     CURLIN                  ; F4F2: 7A DE 99       
        LDD     CURPOS                  ; F4F5: FC DE 96       
        SUBD    #$0050                  ; F4F8: 83 00 50        $50 = DECIMAL 80
        STD     CURPOS                  ; F4FB: FD DE 96       
        LDD     MDE9A                   ; F4FE: FC DE 9A       
        SUBD    #$0050                  ; F501: 83 00 50        $50 = DECIMAL 80
        STD     MDE9A                   ; F504: FD DE 9A       
        JMP     CURCRT                  ; F507: 7E F3 30       

; REVERSE LINE FEED ESC I
REVELF  LDA     CURLIN                  ; F50A: B6 DE 99       
        BNE     ZF4F2                   ; F50D: 26 E3          
        JSR     ZF38E                   ; F50F: BD F3 8E       
        LDA     CURLIN                  ; F512: B6 DE 99       
        BRA     ZF4F2                   ; F515: 20 DB          

; CODE ESC j
ESC_j   LDD     CURPOS                  ; F517: FC DE 96       
        STD     MDE9E                   ; F51A: FD DE 9E       
        LDD     SCORAD                  ; F51D: FC DE 94       
        STD     MDE9C                   ; F520: FD DE 9C       
        LDD     MDE9A                   ; F523: FC DE 9A       
        STD     MDEA2                   ; F526: FD DE A2       
        LDD     CURROW                  ; F529: FC DE 98       
        STD     MDEA0                   ; F52C: FD DE A0       
        RTS                             ; F52F: 39             

; CODE ESC k
ESC_k   LDD     MDE9E                   ; F530: FC DE 9E       
        STD     CURPOS                  ; F533: FD DE 96       
        LDD     MDE9C                   ; F536: FC DE 9C       
        STD     SCORAD                  ; F539: FD DE 94       
        LDD     MDEA2                   ; F53C: FC DE A2       
        STD     MDE9A                   ; F53F: FD DE 9A       
        LDD     MDEA0                   ; F542: FC DE A0       
        STD     CURROW                  ; F545: FD DE 98       
        JMP     CURCRT                  ; F548: 7E F3 30       

; ???
MF54B   PSHS    D                       ; F54B: 34 06          
        SUBA    #$20                    ; F54D: 80 20          
        CMPA    #$18                    ; F54F: 81 18          
        BCC     ZF55C                   ; F551: 24 09          
        STA     CURLIN                  ; F553: B7 DE 99       
        LDD     #MF55E                  ; F556: CC F5 5E       
        STD     GRADDR                  ; F559: FD DE 92        CHANGE JMADDR TO $F55E FOR NEXT CHAR
ZF55C   PULS    PC,D                    ; F55C: 35 86          

; ???
MF55E   PSHS    X,D                     ; F55E: 34 16          
        SUBA    #$20                    ; F560: 80 20          
        CMPA    #$50                    ; F562: 81 50          
        BCS     ZF568                   ; F564: 25 02          
        LDA     #$4F                    ; F566: 86 4F          
ZF568   STA     CURROW                  ; F568: B7 DE 98       
        LDA     CURLIN                  ; F56B: B6 DE 99       
        LDB     #$50                    ; F56E: C6 50          
        MUL                             ; F570: 3D             
        ADDD    SCORAD                  ; F571: F3 DE 94       
        STD     MDE9A                   ; F574: FD DE 9A       
        ADDB    CURROW                  ; F577: FB DE 98       
        ADCA    #$00                    ; F57A: 89 00          
        STD     CURPOS                  ; F57C: FD DE 96       
        JSR     CURCRT                  ; F57F: BD F3 30       
        LDD     #CHRSCR                 ; F582: CC F7 02       
        STD     GRADDR                  ; F585: FD DE 92        CHANGE JMADDR TO $F702 FOR NEXT CHAR
        PULS    PC,X,D                  ; F588: 35 96          

; CURSOR TO HOME (0,0) ESC H
CURHOM  LDD     SCORAD                  ; F58A: FC DE 94       
        STD     CURPOS                  ; F58D: FD DE 96       
        STD     MDE9A                   ; F590: FD DE 9A       
        CLR     CURROW                  ; F593: 7F DE 98       
        CLR     CURLIN                  ; F596: 7F DE 99       
        JMP     CURCRT                  ; F599: 7E F3 30       

; CODE ESC O
ESC_O   CLRA                            ; F59C: 4F             
        FCB     $B4                     ; F59D: B4             

; CODE ESC @
ESC_AR  LDA     #$01                    ; F59E: 86 01          
        STA     MDEA4                   ; F5A0: B7 DE A4       
        RTS                             ; F5A3: 39             

; CODE ESC q
ESC_q   CLRA                            ; F5A4: 4F             
        FCB     $B4                     ; F5A5: B4             

; CODE ESC p
ESC_p   LDA     #$80                    ; F5A6: 86 80          
        STA     MDEA5                   ; F5A8: B7 DE A5       
        RTS                             ; F5AB: 39             

; CODE ESC G
ESC_G   CLRA                            ; F5AC: 4F             
        FCB     $B4                     ; F5AD: B4             

; CODE ESC F
ESC_F   LDA     #$20                    ; F5AE: 86 20          
        STA     MDEA6                   ; F5B0: B7 DE A6       
        RTS                             ; F5B3: 39             

; CODE ESC v
ESC_v   CLR     MDEA7                   ; F5B4: 7F DE A7       
        RTS                             ; F5B7: 39             

; CODE ESC w
ESC_w   CLR     MDEA7                   ; F5B8: 7F DE A7       
        DEC     MDEA7                   ; F5BB: 7A DE A7       
        RTS                             ; F5BE: 39             

; CODE ESC L
ESC_L   LDB     #$16                    ; F5BF: C6 16          
ZF5C1   CMPB    CURLIN                  ; F5C1: F1 DE 99       
        BLT     ZF5CB                   ; F5C4: 2D 05          
        BSR     ZF5DD                   ; F5C6: 8D 15          
        DECB                            ; F5C8: 5A             
        BRA     ZF5C1                   ; F5C9: 20 F6          
ZF5CB   BSR     CARRET                  ; F5CB: 8D 77          
        JMP     ESC_l                   ; F5CD: 7E F4 1E       
ZF5D0   LDA     #$50                    ; F5D0: 86 50          
        MUL                             ; F5D2: 3D             
        ADDD    SCORAD                  ; F5D3: F3 DE 94       
        ANDA    #$07                    ; F5D6: 84 07           MAX IS $07FF
        TFR     D,X                     ; F5D8: 1F 01          
        LDB     #$50                    ; F5DA: C6 50          
        RTS                             ; F5DC: 39             

ZF5DD   PSHS    B                       ; F5DD: 34 04          
        BSR     ZF5D0                   ; F5DF: 8D EF          
ZF5E1   LDA     VIDRAM,X                ; F5E1: A6 89 E0 00    
        LEAX    $50,X                   ; F5E5: 30 88 50       
        BSR     LIMITX                  ; F5E8: 8D 0E          
        STA     VIDRAM,X                ; F5EA: A7 89 E0 00     WRITE A INTO VIDEO RAM
        LEAX    -$4F,X                  ; F5EE: 30 88 B1       
        BSR     LIMITX                  ; F5F1: 8D 05          
        DECB                            ; F5F3: 5A             
        BNE     ZF5E1                   ; F5F4: 26 EB          
        PULS    PC,B                    ; F5F6: 35 84          

; LIMIT X TO MAX $07FF
LIMITX  PSHS    D                       ; F5F8: 34 06          
        TFR     X,D                     ; F5FA: 1F 10          
        ANDA    #$07                    ; F5FC: 84 07           MAX IS $07FF
        TFR     D,X                     ; F5FE: 1F 01          
        PULS    PC,D                    ; F600: 35 86          

; CODE ESC M
ESC_M   PSHS    Y                       ; F602: 34 20          
        LDB     CURLIN                  ; F604: F6 DE 99       
ZF607   CMPB    #$18                    ; F607: C1 18           COMPARE TO 24 (LINES ?)
        BEQ     ZF610                   ; F609: 27 05          
        BSR     ZF621                   ; F60B: 8D 14          
        INCB                            ; F60D: 5C             
        BRA     ZF607                   ; F60E: 20 F7          

; ???
ZF610   BSR     CARRET                  ; F610: 8D 32          
        FCB     $BE,$DE,$94             ; F612: BE DE 94        THIS INSTRUCTION IS "LDX $DE94"
        LEAX    $0730,X                 ; F615: 30 89 07 30    
        LEAY    $50,X                   ; F619: 31 88 50       
        JSR     PUTSXY                  ; F61C: BD F4 42       
        PULS    PC,Y                    ; F61F: 35 A0          

; ???
ZF621   PSHS    B                       ; F621: 34 04          
        BSR     ZF5D0                   ; F623: 8D AB          
ZF625   LEAX    $50,X                   ; F625: 30 88 50        COMPUTE ADDRESS 80 CHAR FORWARD
        BSR     LIMITX                  ; F628: 8D CE          
        LDA     VIDRAM,X                ; F62A: A6 89 E0 00    
        LEAX    -$50,X                  ; F62E: 30 88 B0        COMPUTE ADDRESS 80 CHAR BACKWARD
        BSR     LIMITX                  ; F631: 8D C5          
        STA     VIDRAM,X                ; F633: A7 89 E0 00    
        LEAX    $01,X                   ; F637: 30 01          
        DECB                            ; F639: 5A             
        BNE     ZF625                   ; F63A: 26 E9          
        PULS    PC,B                    ; F63C: 35 84          

; CODE ESC E
ESC_E   JSR     DELSCR                  ; F63E: BD F3 22       
        JMP     CURHOM                  ; F641: 7E F5 8A       

; PERFORM CR CARRIAGE RETURN
CARRET  LDX     MDE9A                   ; F644: BE DE 9A       
        STX     CURPOS                  ; F647: BF DE 96       
        CLR     CURROW                  ; F64A: 7F DE 98       
        JMP     CURCRT                  ; F64D: 7E F3 30       
ZF650   BSR     CARRET                  ; F650: 8D F2          

; PERFORM LF LINE FEED
LINFEE  JSR     ZF4DA                   ; F652: BD F4 DA       
        LDA     CURLIN                  ; F655: B6 DE 99       
        INC     CURLIN                  ; F658: 7C DE 99       
        CMPA    #$17                    ; F65B: 81 17          
        LBEQ    ZF343                   ; F65D: 10 27 FC E2    
        JMP     CURCRT                  ; F661: 7E F3 30       
ZF664   LDD     MDE9A                   ; F664: FC DE 9A       
ZF667   ANDA    #$07                    ; F667: 84 07           MAX IS $07FF
        TFR     D,X                     ; F669: 1F 01          
        LDA     #$20                    ; F66B: 86 20          
        LDB     #$50                    ; F66D: C6 50          
ZF66F   STA     VIDRAM,X                ; F66F: A7 89 E0 00    
        LEAX    $01,X                   ; F673: 30 01          
        CMPX    #$0800                  ; F675: 8C 08 00       
        BNE     ZF67D                   ; F678: 26 03          
        LDX     #$0000                  ; F67A: 8E 00 00       
ZF67D   DECB                            ; F67D: 5A             
        BNE     ZF66F                   ; F67E: 26 EF          
        RTS                             ; F680: 39             

; PERFORM TAB HORIZONTAL TAB
HTAB    JSR     CURRIG                  ; F681: BD F4 B5       
        LDB     CURROW                  ; F684: F6 DE 98       
        CMPB    #$48                    ; F687: C1 48          
        BGT     ZF68F                   ; F689: 2E 04          
        BITB    #$07                    ; F68B: C5 07          
        BNE     HTAB                    ; F68D: 26 F2          
ZF68F   RTS                             ; F68F: 39             

; 6522 OUTPUT 2 CLOCK INIT
VIAINI  PSHS    D                       ; F690: 34 06          
        PSHS    X                       ; F692: 34 10          
        SUBA    #$42                    ; F694: 80 42          
        CMPA    #$08                    ; F696: 81 08           PARAMETER NUMBER = A-$42
        BCC     ZF6A2                   ; F698: 24 08           BRANCH IF OUT OF PARAMETER TABLE
        LDX     #MF6A6                  ; F69A: 8E F6 A6        LOAD VIA PARAMETER TABLE ADDRESS
        LDA     A,X                     ; F69D: A6 86           LOAD PARAMETER FROM TABLE
        STA     VIA_8                   ; F69F: B7 EF 88        STORE PARAMETER IN VIA
ZF6A2   PULS    X                       ; F6A2: 35 10          
        BRA     ZF6FA                   ; F6A4: 20 54           GO TO GRADDR ADDRESS RESTORE

; 6522 PARAMETERS FOR CLOCK OUTPUT 2
MF6A6   FCB     $BE,$5E,$2E,$16,$0E,$0A ; F6A6: BE 5E 2E 16 0E 0A 
        FCB     $04,$01                 ; F6AC: 04 01          

; ???
ZF6AE   PSHS    D                       ; F6AE: 34 06          
        CMPA    #$34                    ; F6B0: 81 34          
        BEQ     ZF6E6                   ; F6B2: 27 32          
        CMPA    #$35                    ; F6B4: 81 35          
        BNE     ZF6C1                   ; F6B6: 26 09          
        LDB     MDEA8                   ; F6B8: F6 DE A8       
        ANDB    #$9F                    ; F6BB: C4 9F          
        ORB     #$20                    ; F6BD: CA 20          
        BRA     ZF6F2                   ; F6BF: 20 31          
ZF6C1   CMPA    #$33                    ; F6C1: 81 33          
        BNE     ZF6FA                   ; F6C3: 26 35          
        STA     MDEA9                   ; F6C5: B7 DE A9       
        BRA     ZF6FA                   ; F6C8: 20 30          

; ???
ZF6CA   PSHS    D                       ; F6CA: 34 06          
        CMPA    #$35                    ; F6CC: 81 35          
        BEQ     ZF6ED                   ; F6CE: 27 1D          
        CMPA    #$34                    ; F6D0: 81 34          
        BNE     ZF6DD                   ; F6D2: 26 09          
        LDB     MDEA8                   ; F6D4: F6 DE A8       
        ANDB    #$60                    ; F6D7: C4 60          
        ORB     #$08                    ; F6D9: CA 08          
        BRA     ZF6F2                   ; F6DB: 20 15          
ZF6DD   CMPA    #$33                    ; F6DD: 81 33          
        BNE     ZF6FA                   ; F6DF: 26 19          
        CLR     MDEA9                   ; F6E1: 7F DE A9       
        BRA     ZF6FA                   ; F6E4: 20 14          
ZF6E6   LDB     MDEA8                   ; F6E6: F6 DE A8       
        ANDB    #$E0                    ; F6E9: C4 E0          
        BRA     ZF6F2                   ; F6EB: 20 05          
ZF6ED   LDB     MDEA8                   ; F6ED: F6 DE A8       
        ORB     #$60                    ; F6F0: CA 60          
ZF6F2   STB     MDEA8                   ; F6F2: F7 DE A8       
        LDA     #$0A                    ; F6F5: 86 0A          
        STD     CRTC                    ; F6F7: FD EC 00       
ZF6FA   LDD     #CHRSCR                 ; F6FA: CC F7 02       
        STD     GRADDR                  ; F6FD: FD DE 92        CHANGE JMADDR TO $F702 FOR NEXT CHAR
        PULS    PC,D                    ; F700: 35 86          

; PUT CHAR ON SCREEN AFTER FILTERING SPECIAL CHAR AND ESC CODE
CHRSCR  PSHS    X,D                     ; F702: 34 16          
        BITA    #$60                    ; F704: 85 60           BIT TO BIT AND OF A WITH $60
        BNE     ZF70D                   ; F706: 26 05          
        TST     MDEA9                   ; F708: 7D DE A9       
        BEQ     ZF712                   ; F70B: 27 05           JUMP TO CHAR FILTERING
ZF70D   JSR     ZF354                   ; F70D: BD F3 54        NORMAL CHAR JUMP TO PUT IT IN VIDEO RAM
        PULS    PC,X,D                  ; F710: 35 96          

; COME HERE IF CHAR ?
ZF712   CMPA    #$08                    ; F712: 81 08           COMPARE WITH CHAR BS
        BNE     ZF71B                   ; F714: 26 05          
        JSR     CURLEF                  ; F716: BD F4 A4       
        PULS    PC,X,D                  ; F719: 35 96          
ZF71B   CMPA    #$0A                    ; F71B: 81 0A           COMPARE WITH CHAR LF
        BNE     ZF724                   ; F71D: 26 05          
        JSR     LINFEE                  ; F71F: BD F6 52       
        PULS    PC,X,D                  ; F722: 35 96          
ZF724   CMPA    #$0D                    ; F724: 81 0D           COMPARE WITH CHAR CR
        BNE     ZF72D                   ; F726: 26 05          
        JSR     CARRET                  ; F728: BD F6 44       
        PULS    PC,X,D                  ; F72B: 35 96          
ZF72D   CMPA    #$09                    ; F72D: 81 09           COMPARE WITH CHAR HTAB
        BNE     ZF736                   ; F72F: 26 05          
        JSR     HTAB                    ; F731: BD F6 81       
        PULS    PC,X,D                  ; F734: 35 96          
ZF736   CMPA    #$07                    ; F736: 81 07           COMPARE WITH CHAR BELL
        BRA     ZF73A                   ; F738: 20 00          
ZF73A   CMPA    #$1B                    ; F73A: 81 1B           COMPARE WITH CHAR ES
        BEQ     CHGADD                  ; F73C: 27 02           IF ESC DETECTED CHANGE GRADDR
        PULS    PC,X,D                  ; F73E: 35 96          

; COMME HERE IF ESC DETECTED AND CHANGE GRAPHIC ROUTINE ADDRESS
CHGADD  LDD     #ESCCOD                 ; F740: CC F7 48       
ZF743   STD     GRADDR                  ; F743: FD DE 92        CHANGE GRADDR FOR NEXT CHAR
        PULS    PC,X,D                  ; F746: 35 96          

; ESC CODE FILTERING - COME HERE IF PREVIOUS CHAR WAS ESC
ESCCOD  PSHS    X,D                     ; F748: 34 16          
        CMPA    #$59                    ; F74A: 81 59           COMPARE WITH CHAR Y
        BNE     ZF753                   ; F74C: 26 05          
        LDD     #MF54B                  ; F74E: CC F5 4B       
        BRA     ZF743                   ; F751: 20 F0          
ZF753   CMPA    #$72                    ; F753: 81 72           COMPARE WITH CHAR r
        BNE     ZF75C                   ; F755: 26 05          
        LDD     #VIAINI                 ; F757: CC F6 90       
        BRA     ZF743                   ; F75A: 20 E7          
ZF75C   CMPA    #$78                    ; F75C: 81 78           COMPARE WITH CHAR x
        BNE     ZF765                   ; F75E: 26 05          
        LDD     #ZF6AE                  ; F760: CC F6 AE       
        BRA     ZF743                   ; F763: 20 DE          
ZF765   CMPA    #$79                    ; F765: 81 79           COMPARE WITH CHAR y
        BNE     SRCCOD                  ; F767: 26 05          
        LDD     #ZF6CA                  ; F769: CC F6 CA       
        BRA     ZF743                   ; F76C: 20 D5          

; COMPARE WITH TABLE TABCOD
SRCCOD  LDX     #TABCOD                 ; F76E: 8E F7 85       
ZF771   CMPA    ,X                      ; F771: A1 84          
        BEQ     ZF77D                   ; F773: 27 08          
        LEAX    $03,X                   ; F775: 30 03          
        TST     ,X                      ; F777: 6D 84          
        BNE     ZF771                   ; F779: 26 F6          
        BRA     ZF780                   ; F77B: 20 03          
ZF77D   JSR     [$01,X]                 ; F77D: AD 98 01       
ZF780   LDD     #CHRSCR                 ; F780: CC F7 02        PREPARE FOR NORMAL GRADDR FOR NEXT CHAR
        BRA     ZF743                   ; F783: 20 BE          

; TABLE DES COMMANDES GRAPHIQUES (A VERIFIER)
TABCOD  FCB     'H                      ; F785: 48             
        FDB     CURHOM                  ; F786: F5 8A          
        FCB     'C                      ; F788: 43             
        FDB     CURRIG                  ; F789: F4 B5          
        FCB     'D                      ; F78B: 44             
        FDB     CURLEF                  ; F78C: F4 A4          
        FCB     'B                      ; F78E: 42             
        FDB     CURDWN                  ; F78F: F4 CB          
        FCB     'A                      ; F791: 41             
        FDB     CURSUP                  ; F792: F4 ED          
        FCB     'I                      ; F794: 49             
        FDB     REVELF                  ; F795: F5 0A          
        FCB     'j                      ; F797: 6A             
        FDB     ESC_j                   ; F798: F5 17          
        FCB     'k                      ; F79A: 6B             
        FDB     ESC_k                   ; F79B: F5 30          
        FCB     'E                      ; F79D: 45             
        FDB     ESC_E                   ; F79E: F6 3E          
        FCB     '@                      ; F7A0: 40             
        FDB     ESC_AR                  ; F7A1: F5 9E          
        FCB     'O                      ; F7A3: 4F             
        FDB     ESC_O                   ; F7A4: F5 9C          
        FCB     'N                      ; F7A6: 4E             
        FDB     ESC_N                   ; F7A7: F4 65          
        FCB     'K                      ; F7A9: 4B             
        FDB     DELEOL                  ; F7AA: F4 01          
        FCB     'o                      ; F7AC: 6F             
        FDB     ESC_o                   ; F7AD: F4 11          
        FCB     'l                      ; F7AF: 6C             
        FDB     ESC_l                   ; F7B0: F4 1E          
        FCB     'b                      ; F7B2: 62             
        FDB     ESC_b                   ; F7B3: F4 2A          
        FCB     'J                      ; F7B5: 4A             
        FDB     DELEOS                  ; F7B6: F4 31          
        FCB     'M                      ; F7B8: 4D             
        FDB     ESC_M                   ; F7B9: F6 02          
        FCB     'L                      ; F7BB: 4C             
        FDB     ESC_L                   ; F7BC: F5 BF          
        FCB     'p                      ; F7BE: 70             
        FDB     ESC_p                   ; F7BF: F5 A6          
        FCB     'q                      ; F7C1: 71             
        FDB     ESC_q                   ; F7C2: F5 A4          
        FCB     'F                      ; F7C4: 46             
        FDB     ESC_F                   ; F7C5: F5 AE          
        FCB     'G                      ; F7C7: 47             
        FDB     ESC_G                   ; F7C8: F5 AC          
        FCB     'z                      ; F7CA: 7A             
        FDB     ESC_z                   ; F7CB: F3 66          
        FCB     'v                      ; F7CD: 76             
        FDB     ESC_v                   ; F7CE: F5 B4          
        FCB     'w                      ; F7D0: 77             
        FDB     ESC_w                   ; F7D1: F5 B8          
        FCB     $00                     ; F7D3: 00              FIN DE TABLE

        FCB     $FF,$FF,$FF,$FF,$FF,$FF ; F7D4: FF FF FF FF FF FF 
        FCB     $FF,$FF,$FF,$FF,$FF,$FF ; F7DA: FF FF FF FF FF FF 
        FCB     $FF,$FF,$FF,$FF,$FF,$FF ; F7E0: FF FF FF FF FF FF 
        FCB     $FF,$FF,$FF,$FF,$FF,$FF ; F7E6: FF FF FF FF FF FF 
        FCB     $FF,$FF,$FF,$FF,$FF,$FF ; F7EC: FF FF FF FF FF FF 
        FCB     $FF,$FF,$FF,$FF,$FF,$FF ; F7F2: FF FF FF FF FF FF 
        FCB     $FF,$FF,$FF,$FF,$FF,$FF ; F7F8: FF FF FF FF FF FF 
        FCB     $FF,$FF                 ; F7FE: FF FF          


        END     BOOTROM
