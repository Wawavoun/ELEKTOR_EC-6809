
;* SINGLE OR DOUBLE DENSITY LOADER FOR EC-6809

;* CONSTANTS

;* EXTERNAL LABEL EQUATES

COMREG  EQU     $EC04       ; FLOPPY CTLR
TRKREG  EQU     $EC05
SECREG  EQU     $EC06
DATREG  EQU     $EC07
DRVREG  EQU     $EC0C
STACK   EQU     $C0FF
SCTBUF  EQU     $C300

        ORG     $C100

CMDADR  BRA    CMDSTA

        FCB     $00,$00
        FCB     $00
FLXTRK  FCB     $1C         ; FILE START TRACK
        FCB     $01         ; FILE START SECTOR
DNS     FCB     $00         ; DENSITY FLAG
TADR    FCB     $C1,$00     ; TRANSFER ADDRESS
LADR1   FCB     $00,$00
        FCB     $00

CMDSTA  LDS     #STACK      ; SETUP STACK
        LDD     FLXTRK      ; GET TRANSFER ADDRESS
        STD     SCTBUF      ; SETUP STARTING TRK & SCT
        LDY     #SCTBUF+256
        
;* MESSAGES

        LDX    #MLOAD
        SWI
        FCB     $03         ; PRINT
        SWI
        FCB     $06
        
;* PERFORM ACTUAL BOOT LOAD

LOAD1   BSR     GETCH       ; GET A CHARACTER
        CMPA    #$02        ; DATA RECORD HEADER ?
        BEQ     LOAD2       ; SKIP IF SO
        CMPA    #$16        ; XFR ADDRESS HEADER?
        BNE     LOAD1       ; LOOP IF NEITHER
        BSR     GETCH       ; GET TRANSFER ADDRESS
        STA     TADR
        BSR     GETCH
        STA     TADR+1
        BRA     LOAD1       ; CONTINUE LOAD
        
LOAD2   BSR     GETCH       ; GET LOAD ADDRESS
        STA     LADR1
        BSR     GETCH
        STA     LADR1+1
        BSR     GETCH       ; GET BYTE COUNT
        TFR     A,B         ; PUT IN B
        TSTA
        BEQ     LOAD1       ; LOOP IF COUNT = 0
        LDX     LADR1       ; GET LOAD ADDRESS
LOAD3   PSHS    B,X
        BSR     GETCH       ; GET A DATA CHARACTER
        PULS    B,X
        STA     ,X+         ; PUT CHARACTER
        DECB                ; END OF DATA IN RECORD ?
        BNE     LOAD3       ; LOOP IF NOT
        BRA     LOAD1       ; GET ANOTHER RECORD

;* GET CHARACTER ROUTINE

GETCH   CMPY    #SCTBUF+256 ; OUT OF DATA ?
        BNE     GETCH4      ; GO READ CHARACTER IF NOT
        LDX     #SCTBUF     ; POINT TO BUFFER
        LDD     0,X         ; GET FORWARD LINK
        BEQ     GO          ; IF ZERO, FILE IS LOADED
        BSR     READ        ; READ NEXT SECTOR
        BNE     CMDADR      ; START OVER IF ERROR
        LDY     #SCTBUF+4   ; POINT PAST LINK
GETCH4  LDA     ,Y+         ; ELSE GET A CHARACTER
        RTS

;* FILE IS LOADED, JUMP TO IT

GO      JMP     [TADR]

;* READ SINGLE SECTOR

READ    BSR     SEEK
        LDA     #$80        ; READ CMD
SUIT1   STA     COMREG      ; APPLY CMD
        LBSR    DEL28
        CLRB
        LDX     #SCTBUF     ; LOAD DESTINATION
READ3   LDA     COMREG
        BITA    #2          ; DRQ ?
        BNE     READ5
        BITA    #1          ; BUSY ?
        BNE     READ3       ; LOOP IF YES
        TFR     A,B
        BRA     READ6
        
READ5   LDA     DATREG
        STA     ,X+
        DECB
        BNE     READ3       ; NEXT BYTE
        BSR     WAIT
READ6   BITB    #$10        ; CHECK RNF
        BEQ     READ8       ; NO, JUMP
        COM     DNS         ; CHANGE DENSITY FOR NEXT READ
READ8   BITB    #$1C        ; CHECK FOR ERROR
        RTS
 
;* ATTENTE FIN DE COMMANDE

WAIT    LDB     COMREG
        BITB    #1
        BNE     WAIT
        RTS

;* SEEK

SEEK    PSHS    A
        TST     DNS
        BEQ     SIMPLE      ; SIMPLE DENSITY

;* DOUBLE DENSITY

        LDA     #%00000001  ; DRIVE 0/DD
        CMPB    #18         ; SECTOR BY SIDE
        BLS     SEEK1       ; GO SIDE 0
        BRA     SEEK2       ; GO SIDE 1

;* SIMPLE DENSITY 

SIMPLE  LDA     #%01000001  ; DRIVE 0/SD
        CMPB    #10         ; SECTOR BY SIDE 
        BLS     SEEK1       ; SIDE 0
SEEK2   ORA     #%00010000  ; FORCE SIDE 1
SEEK1   STB     SECREG
        STA     DRVREG      ; SETS DRIVE REG
        PULS    A
        CMPA    TRKREG      ; CHANGE TRACK ?
        BEQ     DEL28       ; NO, JUMP
        STA     DATREG      ; NEW TRACK
        BSR     DEL28
        LDA     #$1B
        STA     COMREG
        BSR     DEL28
        BSR     WAIT

;* BOUCLE DELAI

DEL28   JSR     DEL14
DEL14   JSR     DEL
DEL     RTS

MLOAD FCC "Loading..."
      FCB $04

      ORG     $C200-4
      FCB     'F', 'L', 'E', 'X'

      END     CMDADR

