
;* COMPACT FLASH FLEX LOADER FOR ELEKTOR EC-6809
;*
;* THIS VERSION BOOT FROM CF 1 $EE80 MAPPED AS DISK 2.
;*
;* ADAPTED BY PH.ROEHR 08/2024

;* CONSTANTS

;* EXTERNAL LABEL EQUATES

STACK       EQU     $C080
SCTBUF      EQU     $C300
RDBOOT      EQU     $E82A
CFOK        EQU     $DEB0
CFTRK       EQU     CFOK+6
ASNPRM      EQU     $CC0B      ; ASN PARAMETERS
STRUPDSK    EQU     $C843      ; DISK NUMBER FOR STARTUP.TXT
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
            STD     ASNPRM      ; SET S AND W DISKS AS 2
            LDA     #$02
            STA     STRUPDSK    ; CHANGE DISK NUMBER TO 2 FOR STARTUP.TXT
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

            END     CMDADR

