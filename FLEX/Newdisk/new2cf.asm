;*********************************************************
;*                                                       *
;* COMPACT FLASH FORMAT UTILITY                          *
;*                                                       *
;* WRITED FOR ELEKTOR EC-6809 BY PH. ROEHR 08/2024       *
;*                                                       *
;* THIS VERSION IS FOR TWO CF LOCATED AT $EE80 & $EEF0   *
;*                                                       *
;*********************************************************

MINS0   EQU     $00
MINS1   EQU     $01
SECSZ   EQU     $FF         ; 256 BYTES/SECTORS FOR FLEX (0->FF)
CFDSKN1 EQU     02          ; COMPACT FLASH 1 $EE80 FLEX DISK NUMBER
CFDSKN2 EQU     03          ; COMPACT FLASH 2 $EEF0 FLEX DISK NUMBER
SECBUF  EQU     $800        ; BUFFER FOR PREPARING SECTOR IN MEMORY
; FLEX SECTOR IS 256 BYTES BUT SECTOR IS 512 BYTES ON CF
ENDBUF  EQU     SECBUF+(SECSZ+1)*2

; STANDARD PRE-NAMED LABEL EQUATES
SYSMTH  EQU     $CC0E       ; SYSTEM DATE MONTH
SYSDAY  EQU     $CC0F       ; DAY
SYSYR   EQU     $CC10       ; YEAR
WARMS   EQU     $CD03
GETCHR  EQU     $CD15
PUTCHR  EQU     $CD18
INBUFF  EQU     $CD1B
PSTRNG  EQU     $CD1E
PCRLF   EQU     $CD24
GETFIL  EQU     $CD2D
OUTDEC  EQU     $CD39
OUTHEX  EQU     $CD3C
GETHEX  EQU     $CD42
OUTADR  EQU     $CD45
INDEC   EQU     $CD48
ASNPRM  EQU     $CC0B       ; ASN PARAMETERS

; ASCII CODE EQUATES
EOT     EQU     $04
SPC     EQU     $20

; FLEX CF PARAMETERS
CF1MTRK  EQU    $DEBA
CF2MTRK  EQU    $DEBC

; CF REGS
CF1ADDRESS  EQU     $EE80
CF2ADDRESS  EQU     $EEF0
CFDATA      EQU     $00     ; DATA PORT
CFERROR     EQU     $01     ; ERROR CODE (READ)
CFFEATURE   EQU     $01     ; FEATURE SET (WRITE)
CFSECCNT    EQU     $02     ; NUMBER OF SECTORS TO TRANSFER
CFLBA0      EQU     $03     ; SECTOR ADDRESS LBA 0 [0:7]
CFLBA1      EQU     $04     ; SECTOR ADDRESS LBA 1 [8:15]
CFLBA2      EQU     $05     ; SECTOR ADDRESS LBA 2 [16:23]
CFLBA3      EQU     $06     ; SECTOR ADDRESS LBA 3 [24:27 (LSB)]
CFSTATUS    EQU     $07     ; STATUS (READ)
CFCOMMAND   EQU     $07     ; COMMAND SET (WRITE)

        ORG     $0100

FMAXS       FCB     $00      ; MSB
MAXS        FCB     $FF      ; LSB 255 SECTOR ON TRACK 0 (0->FF BUT SECTOR 2 NOT WRITED)
                             ; 255 SECTOR ON TRACK 1 TO FF (1->FF)
FMAXT       FCB     $00      ; MSB
MAXT        FCB     $FF      ; LSB 256 TRACKS (0-FF)
TRACK       RMB     1
SECTOR      RMB     1
SEC16       RMB     2
FKFCB       RMB     4
VOLNAM      FCB     0,0,0,0,0,0,0,0,0,0,0
VOLNUM      FCB     0,0
LBA3        FCB     $E0      ; B7=1 / B6=1 FOR LBA / B5=1 / B4=0 MASTER / B3->B0 LBA27 TO LBA24
LBA2        FCB     $00      ; LBA23 TO LBA 16
LBA1        FCB     $00      ; LBA15 TO LBA8
LBA0        FCB     $00      ; LBA7 TO LBA0
MAXLBA      RMB     2
CFADDRES    RMB     2
CFSTATUL    RMB     2

        ORG     $1000

NEWCF   BRA     FORM1       ; BEGIN
VN      FCB     1           ; VERSION

OUTIN   JSR     PSTRNG      ; DISPLAY STRING
OUTIN2  JSR     GETCHR      ; GET RESPONSE
        ANDA    #$5F        ; UPPER CASE
        CMPA    #'Y'        ; COMPARE TO Y SO Z IS SET IF YES
        RTS

; PROGRAM START
FORM1   JSR     PCRLF
        JSR     GETHEX      ; GET DRIVE NUMBER FROM COMMAND LINE
        LBCS    EXIT        ; EXIT ON ERROR
        TFR     X,D
        TST     CFOK        ; IF CFOK=0 NO CF ON THE SYSTEM
        LBEQ    EXIT        ; THEN EXIT
        CMPD    #CFDSKN1    ; ENSURE COMPACT FLASH DISK 2 OR 3 ASKED
        BEQ     CONTIN0     ; CONTINUE IF DISK 3
        CMPD    #CFDSKN2     
        BEQ     CONTIN0     ; CONTINUE IF DISK 2
        LBRA    EXIT        ; EXIT IF DISK ASKED IS NOT A CF
CONTIN0 CMPB    ASNPRM      ; ENSURE COMPACT FLASH ASKED IS NOT SYSTEM DISK
        BNE     CONTIN      ; COMPARE ASN-SYSTEM TO CF DISK NUMBER
        LDX     #MSGCFS     ; IF YES DISPLAY MSG AND EXIT
        JMP     EXIT2
CONTIN  JSR     PCRLF
        CMPB    #CFDSKN1    ; IS CF 1 (FLEX DISK 2) ASKED ?
        BNE     CONTIN2     ; NO - GO TO SET CF 2 ADDRESS  
        LDX     #CF1ADDRESS ; YES - LOAD CF 1 ADDRESS 
        BRA     CONTIN3     
CONTIN2 LDX     #CF2ADDRESS ; LOAD CF 2 ADDRESS
CONTIN3 STX     CFADDRES    ; STORE
        LDA     #CFSTATUS   ; COMPUTE CF STATUS REGISTER ADDRESS (16 BITS)
        LEAX    A,X
        STX     CFSTATUL    ; STORE        
        JSR     INITCF      ; INIT COMPACT FLASH
        BEQ     CONTIN1     ; GO TO FORMAT IF OK (Z SET)
        LDX     #CFINIER    ; ERROR AFTER CF INIT - DISPLAY
        JMP     EXIT2       ; AND EXIT
CONTIN1 LDX     #CFINIOK    ; DISPLAY MSG CF INIT OK
        JSR     PSTRNG
        JSR     READCF      ; READ CF PARAMETERS
        LDX     #MSGLBA     ; DISPLAY MSG PARAM OK
        JSR     PSTRNG
        LDX     #SECBUF
        LEAX    123,X       ; GET MAX LBA IN BUFFER (MSW H)
        JSR     OUTHEX      ; DISPLAY IT
        LEAX    -1,X        ; GET MAX LBA IN BUFFER (MSW L)
        JSR     OUTHEX      ; DISPLAY IT
        LEAX    -1,X        ; GET MAX LBA IN BUFFER (LSW H)
        JSR     OUTHEX      ; DISPLAY IT
        LEAX    -1,X        ; GET MAX LBA IN BUFFER (LSW L)
        JSR     OUTHEX      ; DISPLAY IT
        JSR     PCRLF
        LDX     #SURES      ; ASK IF SURE
        LBSR    OUTIN
        LBNE    EXIT        ; EXIT IF NOT
        LDX     #SCRDS      ; ASK IF SCRATCH DISK SURE
        JSR     PSTRNG
        LBSR    OUTIN2      ; GET RESPONSE
        LBNE    EXIT        ; EXIT IF SCRATCH NOT SURE
FORM20  JSR     PCRLF
        LDX     #SECNUM     ; ASK SECTOR/TRACK NUMBER
        JSR     PSTRNG
        JSR     INBUFF
        JSR     INDEC
        BCS     FORM20
        CMPX    #$0013      ; CHECK IF >= 20
        BLE     FORM20
        CMPX    #$00FF      ; CHECK IF <= 255
        BHI     FORM20
        STX     >FMAXS
FORM30  LDX     #TRKNUM     ; ASK TRACK NUMBER
        JSR     PSTRNG
        JSR     INBUFF
        JSR     INDEC
        BCS     FORM30
        CMPX    #$0027      ; CHECK IF >= 40
        BLE     FORM30
        CMPX    #$00FF      ; CHECK IF <= 255
        BHI     FORM30
        LEAX    -1,X        ; SUBSTRACT 1 BECAUSE TRACK ARE NUMBERED 0 TO MAXT
        STX     >FMAXT
FORM40  LDX     #MNSTR      ; ASK VOLUME NAME
        JSR     PSTRNG
        JSR     INBUFF
        LDX     #FKFCB
        JSR     GETFIL
FORM27  LDX     #NUMSTR     ; ASK VOLUME NUMBER
        JSR     PSTRNG
        JSR     INBUFF
        JSR     INDEC
        BCS     FORM27
        STX     >VOLNUM
        JSR     PCRLF
        JMP     FORMAT      ; GO TO FORMAT IF OK

; EXIT ROUTINES
EXIT    LDX     #ABORTS
EXIT2   JSR     PSTRNG
        JSR     PCRLF
        JMP     WARMS       ; EXIT

; MAIN FORMATTING LOOP
FORMAT  LDX     #SECBUF     ; CLEAR BUFFER
CLRBUF  CLR     ,X+
        CMPX    #ENDBUF     ; FLEX SECTOR IS 256 BYTES BUT SECTOR IS 512 BYTES ON CF
        BNE     CLRBUF
        LDX     #INITRK     ; DISPLAY WRITE TR/SECTOR MESSAGE
        JSR     PSTRNG
        CLR     >TRACK      ; SET TRACK 0
        LDA     #MINS0      ; PREPARE FOR TRACK 0
FORM    STA     >SECTOR
FORM3   CMPA    #02         ; IF SECTOR 2 CHECK IF TRACK 0
        BNE     FORM4
        LDA     >TRACK
        BEQ     FORM5       ; IF S2 TR0 NO SECTOR WRITED
FORM4   JSR     SECHD       ; GO PREPARE SECTOR IN MEMORY
        JSR     COMPLBA     ; COMPUTE LBA FROM SECTOR AND TRACK
        JSR     WTSECT      ; COPY MEMORY BUFFER TO CF
FORM5   LDA     >SECTOR
        CMPA    MAXS        ; COMPARE TO MAXS
        BEQ     FORM50
        INC     >SECTOR
        LDA     >SECTOR
        BRA     FORM3
FORM50  LDA     >TRACK
        CMPA    MAXT        ; COMPARE TRACK TO MAXT
        BEQ     ENDFORM     ; END IF TRACK MAXT WRITED
        INC     >TRACK
        LDA     #MINS1      ; PREPARE FOR TRACK 1 AND NEXT
        BRA     FORM        ; REPEAT FOR NEXT TR
ENDFORM LDD     >LBA1       ; STORE MAX LBA
        STD     >MAXLBA
        JMP     SETSIR      ; END OF FORMATTING LOOP - GO TO SETUP SIR

; SETUP SECTOR IN MEMORY
SECHD   LDX     #SECBUF     ; POINT TO BUFFER
        LDB     >SECTOR
        CMPB    >MAXS       ; CHECK IF SECTOR IS LAST FROM TRACK
        BNE     NOLASTS     ; IF NOT LAST SECTOR CONTINUE
        LDB     >TRACK      ; IF LAST SECTOR FROM TRACK
        BEQ     SET0000     ;   CHECK IF TRACK 0
        CMPB    >MAXT       ;   CHECK IF LAST TRACK
        BNE     NOLASTT
SET0000 CLRB                ; IN LAST SECTOR FROM TRACK 0 OR LAST TRACK PUT 00-00
        STB     ,X+
        STB     ,X
        BRA     SECHDE
NOLASTT LDB     >TRACK      ; IN LAST SECTOR FROM OTHER TRACKS PUT TR+1-01
        INCB
        STB     ,X+
        LDB     #01
        STB     ,X
        BRA     SECHDE
NOLASTS LDB     >TRACK
        STB     ,X+
        CMPB    #0          ; CHECK IF TRACK 0
        BNE     NOTTR0
        LDB     >SECTOR
        CMPB    #1          ; IF TRACK 0 CHECK IF SECTOR 1
        BNE     NOTTR0
        INCB                ; IN CASE OF TR 0 SEC 1 INCREASE SECTOR POINTER ONE MORE
        BRA     GCAS
NOTTR0  LDB     >SECTOR
GCAS    INCB                ; IN GENERAL CASE ALWAYS POINT TO NEXT SECTOR
        STB     ,X
SECHDE  RTS                 ; REMAINING BYTES OF BUFFER ALREADY SET TO 0

; WRITE SECTOR TO CF
WTSECT  LDA     #$08        ; LOAD BACKSPACE
        RPT     13          ; ERASE FROM PREVIOUS
        JSR     PUTCHR
        LDX     #TRACK
        JSR     OUTADR      ; DISPLAY TRACK AND SECTOR
        LDA     #SPC
        JSR     PUTCHR      ; DISPLAY SPC
        LDX     #LBA3
        JSR     OUTADR      ; DISPLAY LBA3 AND LBA2
        LDX     #LBA1
        JSR     OUTADR      ; DISPLAY LBA1 AND LBA0
        JSR     WRITECF     ; WRITE SECTOR TO CF
        RTS

; SETUP THE SIR IN BUFFER THEN WRITE IT INTO TR 0 / SEC 3
SETSIR  LDX     #SECBUF     ; POINT TO BUFFER
CLRL    CLR     ,X+         ; CLEAR 16 FIRST BYTES
        CMPX    #SECBUF+16
        BNE     CLRL
        LDY     #VOLNAM     ; TRANSFERT 11 BYTES AS VOLUME NAME
NAML    LDB     ,Y+
        STB     ,X+         ; WRITE TO BUFFER
        CMPY    #VOLNAM+11
        BNE     NAML
        LDD     ,Y          ; LOAD TWO BYTES AS VOLUME NUMBER
        STD     ,X++        ; WRITE TO BUFFER
        LDB     #01         ; SET 01 FOR FIRST FREE TRACK AND SECTOR
        STB     ,X+
        STB     ,X+
        LDB     >MAXT       ; SET MAX TRACK AS LAST FREE TRACK
        STB     ,X+
        LDB     >MAXS       ; SET MAX SECTOR AS LAST FREE SECTOR
        STB     ,X+
        LDD     MAXLBA      ; LOAD MAX SECTORS ON CF
        SUBD    FMAXS       ; SUBSTRACT TRACK 0 SECTORS
        INCD
        STD     ,X++        ; WRITE HOW MANY FREE SECTORS ON DISK
        LDB     >SYSMTH     ; SET MONTH
        STB    ,X+
        LDB     >SYSDAY     ; SET DAY
        STB    ,X+
        LDB     >SYSYR      ; SET YEAR
        STB     ,X+
        LDB     >MAXT       ; SET MAX TRACK
        STB     ,X+
        LDB     >MAXS       ; SET MAX SECTOR
        STB     ,X
        LDA     #2          ; SIR LAY IN TR0/SEC3 WHICH IS LBA E0 00 00 02
        STA     >LBA0
        CLR     >LBA1
        JSR     WRITECF     ; WRITE SIR BUFFER TO CF
        LDX     #MSGSIR     ; DISPLAY SIR MESSAGE
        JSR     PSTRNG
        JMP     SETDIR      ; GO TO DIR SETUP

; DIR SETUP (NOT REALLY REQUIRED, FOR CLARITY ONLY)
; HERE WE HAVE JUST TO PUT 00-00 INTO LAST SECTOR
; OF TRACK 0 TO END THE DIRECTORY SECTORS CHAIN
SETDIR  LDX     #SECBUF      ; CLEAR BUFFER
CLRDIR  CLR     ,X+
        CMPX    #ENDBUF
        BNE     CLRDIR
        CLR     >TRACK      ; SET TRACK TO 0
        LDA     >MAXS
        STA     >SECTOR     ; SET SECTOR TO MAX
        JSR     COMPLBA     ; COMPUTE LBA
        JSR     WRITECF     ; WRITE SECTOR
        LDX     #MSGDIR     ; DISPLAY DIR MESSAGE
        JSR     PSTRNG

;* SAVE BOOT IN TRACK 0 SECTOR 1 (LBA 0000)
DOBOOT  LDY     #CMDADR     ; COPY BOOTLOADER INTO SECTOR BUFFER
        LDX     #SECBUF
BOOTL   LDD     ,Y++
        STD     ,X++
        CMPY    #CMDADR+512
        BNE     BOOTL
        LDD     #$0000
        STD     >LBA1       ; SET LBA 0000
        JSR     WRITECF     ; WRITE THE SECTOR
        LDX     #MSGBOOT    ; DISPLAY BOOT MESSAGE
        JSR     PSTRNG

; ALL DONE
        JSR     PCRLF
        LDX     #SECST      ; DISPLAY SECTOR NUMBER MESSAGE
        JSR     PSTRNG
        LDX     >MAXLBA
        LEAX    1,X         ; ADD ONE DUE TO TR 0 / SEC 0
        STX     >MAXLBA
        LDX     #MAXLBA
        JSR     OUTDEC      ; DISPLAY NUMBER OF SECTORS
        
; WE MUST UPDATE FLEX CF TABLE 
        LDA     MAXT
        LDB     MAXS
        LDX     CFADDRES
        CMPX    #CF1ADDRESS ; CF 1 ?
        BNE     CF2         ; NO - GO TO CF 2
        STD     CF1MTRK     ; STORE PARAMETERS
        JMP     EXIT2       ; NOW EXIT
CF2     STD     CF2MTRK     ; IF NOT CF 1 THEN CF 2 - STORE PARAMETERS
        LDX     #CMPLTE     ; SET MESSAGE FORMAT COMPLETE
        JMP     EXIT2       ; NOW EXIT

; DISPLAYED MESSAGES

SURES   FCC     "ARE YOU SURE ? "
        FCB     EOT
CFINIOK FCC     "COMPACT FLASH INITIALIZED."
        FCB     EOT
MSGLBA  FCC     "TOTAL LBA ON COMPACT FLASH (HEX) : "
        FCB     EOT
MSGCFS  FCC     "COMPACT FLASH SET AS SYSTEM DISK - CAN'T FORMAT."
        FCB     EOT
CFINIER FCC     "ERROR INITIALIZING COMPACT FLASH."
        FCB     EOT
SCRDS   FCC     "SCRATCH COMPACT FLASH ? "
        FCB     EOT
ABORTS  FCC     "FORMAT ABORTED !"
        FCB     EOT
MSGSIR  FCC     "SIR BUILD AND WRITE."
        FCB     EOT
MSGDIR  FCC     "DIR BUILD AND WRITE."
        FCB     EOT
MSGBOOT FCC     "BOOT SECTOR(S) SET."
        FCB     EOT
CMPLTE  FCC     "FORMAT COMPLETE."
        FCB     EOT
SECST   FCC     "TOTAL SECTORS = "
        FCB     EOT
MNSTR   FCC     "VOLUME NAME ? "
        FCB     EOT
NUMSTR  FCC     "VOLUME NUMBER ? "
        FCB     EOT
INITRK  FCC     "WRITE TRACK/SECTOR + LBA3-2-1-0 :              "
        FCB     EOT
SECNUM  FCC     "HOW MANY SECTORS PER TRACK (20-255) ? "
        FCB     EOT
TRKNUM  FCC     "HOW MANY TRACKS (40-255) ? "
        FCB     EOT

; INITIALISE THE CF CARD
INITCF  LDX     CFADDRES
        JSR     CMDWAIT
        LDB     #$04            ; RESET THE CF CARD
        STB     [CFSTATUL]
        JSR     CMDWAIT
        LDB     #$E0            ; CLEAR LBA3, SET MASTER & LBA MODE
        STB     CFLBA3, X
        JSR     CMDWAIT
        LDB     #$01            ; SET 8-BIT BUS-WIDTH
        STB     CFFEATURE, X
        JSR     CMDWAIT
        LDB     #$01            ; READ ONLY ONE SECTOR AT A TIME.
        STB     CFSECCNT, X
        JSR     CMDWAIT
        LDB     #$EF            ; ENABLE FEATURES
        STB     CFCOMMAND, X
        JSR     CMDWAIT
        JSR     CFERR
        RTS

; WRITE A BLOCK OF DATA FROM THE MEMORY TO CF CARD
WRITECF PSHS    Y,X,B,A
        PSHS    CC
        ORCC    #$50            ; SET INTERRUPT BITS - DISABLE IRQ FIRQ
        JSR     CMDWAIT
        LDX     CFADDRES
        LDB     >LBA0           ; LOAD THE LBA ADDRESSES WITH THE CURRENT
        STB     CFLBA0, X       ; SETTINGS BEFORE ISSUING THE WRITE COMMAND.
        JSR     CMDWAIT
        LDB     >LBA1
        STB     CFLBA1, X
        JSR     CMDWAIT
        LDB     >LBA2
        STB     CFLBA2, X
        JSR     CMDWAIT
        LDB     >LBA3
        STB     CFLBA3, X
        JSR     CMDWAIT
        LDB     #$01
        STB     CFSECCNT, X
        JSR     CMDWAIT
        LDB     #$30            ; SEND WRITE COMMAND TO THE CF CARD
        STB     CFCOMMAND, X
        LDY     #SECBUF         ; POINT TO START OF THE MEMORY BLOCK
WRLOOP  JSR     DATWAIT
        LDA     ,Y+             ; READ THE BYTE FROM THE BUFFER
        STA     CFDATA, X       ; WRITE THE DATA BYTE TO THE CF CARD.
        JSR     DATWAIT
        LDA     CFSTATUS, X
        BITA    #$08
        BNE     WRLOOP
        PULS    CC             ; RESTORE INTERRUPT
        PULS    Y,X,B,A,PC

; READ PARAMETERS FROM THE CF CARD TO MEMORY
READCF  PSHS    Y,X,B,A
        PSHS    CC
        ORCC    #$50                ; SET INTERRUPT BITS - DISABLE IRQ FIRQ
        LDX     CFADDRES
        JSR     CMDWAIT
        LDB     #$EC                ; SEND READ PARAMETERS TO THE CF CARD
        STB     CFCOMMAND, X
        LDY     #SECBUF             ; POINT TO THE START OF THE MEMORY BLOCK
RDLOOP  JSR     DATWAIT
        LDA     CFDATA, X           ; READ THE DATA BYTE
        STA     ,Y+                 ; WRITE IT TO THE BUFFER
        JSR     DATWAIT
        LDA     CFSTATUS, X
        BITA    #$08
        BNE     RDLOOP
        PULS    CC                  ; RESTORE INTERRUPT
        PULS    Y,X,B,A,PC

; COMPUTE LBA FROM TRACK AND SECTOR
COMPLBA LDA     >TRACK
        BEQ     TR00            ; CHECK IF TRACK 0
        LDB     >MAXS
        MUL                     ; MULTIPLY A.B TO A AND B (A MSB)
        PSHD    D
        CLR     >SEC16          ; SET SECTOR AS 16 BITS NUMBER INTO SEC16
        LDA     >SECTOR
        STA     SEC16+1
        PULS    D
        ADDD    >SEC16          ; ADD TO D
        DECD                    ; -1
        BRA     ENDLBA
TR00    LDB     >SECTOR         ; IN TRACK 0
        CMPB    #1              ; CHECK IF SECTOR<=1
        BLS     ENDLBA
        DECB                    ; IF SECTOR>1 AND TRACK=0 THEN LBA0=SECTOR-1
ENDLBA  STB     >LBA0
        STA     >LBA1
        RTS

; WAIT FOR CF CARD READY WHEN READING/WRITING TO CF CARD
; CHECK BUSY = 0 (BIT 7) THEN READY = 1 (BIT 6)
CMDWAIT BSR     DATWAIT
CMD1    LDB     [CFSTATUL]      ; READ THE STATUS REGISTER
        BITB    #%01000000      ; ISOLATE THE READY BIT
        BEQ     CMD1            ; WAIT FOR THE BIT TO CLEAR
        RTS

; WAIT FOR CF CARD READY WHEN READING/WRITING TO CF CARD
; CHECK FOR BUSY = 0 (BIT 7)
DATWAIT LDB     [CFSTATUL]      ; READ THE STATUS REGISTER
        BITB    #%10000000      ; ISOLATE THE BUSY BIT
        BNE     DATWAIT         ; WAIT FOR THE BIT TO CLEAR
        RTS

; ERROR INITIALISING THE CF CARD
CFERR   LDB     [CFSTATUL]
        BITB    #$01            ; ISOLATE THE ERROR BIT
        RTS                     ; RETURN Z CLEAR IF ERROR

;* COMPACT FLASH FLEX LOADER FOR ELEKTOR EC-6809
;* ADAPTED BY PH.ROEHR 04/2024

;* CONSTANTS

;* EXTERNAL LABEL EQUATES

STACK       EQU     $C080
SCTBUF      EQU     $C300
RDBOOT      EQU     $E82A
CFOK        EQU     $DEB0
CFTRK       EQU     CFOK+6
; ASNPRM      EQU     $CC0B      ; ASN PARAMETERS
STRUPDSK    EQU     $C843      ; DISK NUMBER ADDRESS FOR STARTUP.TXT
ROMLATCH    EQU     $EEB0

            ORG     $C100

CMDADR      BRA    CMDSTA

            FCB     $00,$00
            FCB     $00
FLXTRK      FCB     $3B         ; FILE START TRACK
            FCB     $38         ; FILE START SECTOR
TADR        FCB     $C1,$00     ; TRANSFER ADDRESS
LADR1       FCB     $00,$00
            FCB     $00

CMDSTA      LDS     #STACK      ; SETUP STACK
            LDB     #$01        ; SET I/O EPROM PAGE 1
            STB     ROMLATCH
            LDD     FLXTRK      ; GET FLEX.SYS TRACK & SECTOR
            STD     SCTBUF      ; SETUP STARTING TRK & SCT INTO BUFFER FOR 1ST SECTOR READ
            LDY     #SCTBUF+256

;* MESSAGES

            LDX    #MLOAD
            SWI
            FCB     $03         ; PRINT
            SWI
            FCB     $06

;* PERFORM ACTUAL BOOT LOAD

LOAD1       BSR     GETCH       ; GET A CHARACTER
            CMPA    #$02        ; DATA RECORD HEADER ?
            BEQ     LOAD2       ; SKIP IF SO
            CMPA    #$16        ; TRANSFER ADDRESS HEADER ?
            BNE     LOAD1       ; LOOP IF NEITHER
            BSR     GETCH       ; GET TRANSFER ADDRESS
            STA     TADR
            BSR     GETCH
            STA     TADR+1
            BRA     LOAD1       ; CONTINUE LOAD
LOAD2       BSR     GETCH       ; GET LOAD ADDRESS
            STA     LADR1
            BSR     GETCH
            STA     LADR1+1
            BSR     GETCH       ; GET BYTE COUNT
            TFR     A,B         ; PUT IN B
            TSTA
            BEQ     LOAD1       ; LOOP IF COUNT = 0
            LDX     LADR1       ; GET LOAD ADDRESS
LOAD3       PSHS    B,X
            BSR     GETCH       ; GET A DATA CHARACTER
            PULS    B,X
            STA     ,X+         ; PUT CHARACTER
            DECB                ; END OF DATA IN RECORD ?
            BNE     LOAD3       ; LOOP IF NOT
            BRA     LOAD1       ; GET ANOTHER RECORD

;* GET CHARACTER ROUTINE

GETCH       CMPY    #SCTBUF+256 ; OUT OF DATA ?
            BNE     GETCH4      ; GO READ CHARACTER IF NOT
            LDX     #SCTBUF     ; POINT TO BUFFER
            LDD     0,X         ; GET FORWARD LINK
            BEQ     GO          ; IF ZERO, FILE IS LOADED
            STD     CFTRK       ; STORE TRK-SEC
            JSR     RDBOOT      ; READ NEXT SECTOR
            LDY     #SCTBUF+4   ; POINT PAST LINK
GETCH4      LDA     ,Y+         ; ELSE GET A CHARACTER
            RTS

;* FILE IS LOADED, PREPARE AND JUMP TO IT

GO          LDD     #$0202      ; IN CASE OF CF BOOT
            STD     ASNPRM      ; SET S AND W DISKS AS 3
            LDA     #$02
            STA     STRUPDSK    ; CHANGE DISK 0 TO 3 FOR STARTUP.TXT
            JMP     [TADR]

MLOAD       FCC     "Loading from CF..."
            FCB     $04

            RPT     $C200-4-*
            FCB     $00

            ORG     $C200-4
            FCB     'F', 'L', 'E', 'X'

;* FILL REMAINING SPACE WITH $00

            RPT     256
            FCB     $00

;            END     CMDADR
            END     NEWCF
