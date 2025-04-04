
;*   ELEKTOR EC6809 DISK DRIVER
;*   THIS VERSION IS FOR 2 CF LOCATED AT EE80 AND EEF0
;*   05/2024 PH. ROEHR

;*   I/O BOARD EPROM ROUTINES ADDRESSES

SEEK        EQU     $E800
READ        EQU     SEEK+3
WRITE       EQU     SEEK+6
VERIF       EQU     SEEK+9
RST         EQU     SEEK+12
DRV         EQU     SEEK+15
CHKRDY      EQU     SEEK+18
QUICK       EQU     SEEK+21
CHKCF       EQU     SEEK+24
RDCFSEC     EQU     SEEK+27
WRCFSEC     EQU     SEEK+30
CFSEEK      EQU     SEEK+33
CFVRFY      EQU     SEEK+36

;*  VARIOUS ADDRESSES

ROMLATCH    EQU     $EEB0
RAMLATCH    EQU     $EEC0

;*  CF REGS

CF1ADDRES   EQU     $EE80
CF2ADDRES   EQU     $EEF0
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

;*  DISK DRIVER ROUTINE JUMP TABLE

        ORG     $DE00

DREAD   JMP     READ
DWRITE  JMP     WRITE
DVERIFY JMP     VERIF
DRESTOR JMP     RST
DDRIVE  JMP     DRV
DCHECK  JMP     CHKRDY
DQUICK  JMP     QUICK
DINIT   JMP     INIT
DWARM   JMP     WARM
DSEEK   JMP     SEEK

;*   INIT    THIS ROUTINE PERFORMS ANY NECESSARY INITIALIZATION OF THE
;*           DRIVERS DURING COLD START (AT BOOT TIME). ACTUALLY, ANY
;*           OPERATION WHICH MUST BE DONE WHEN THE SYSTEM IS FIRST BOOTED
;*           CAN BE DONE HERE.
;*
;*           ENTRY - NO PARAMETERS
;*
;*           EXIT - A, B, X, Y, AND U MAY BE DESTROYED
;*
;*   ITS BETTER TO HAVE INIT HERE BECAUSE ROMLATCH = $FF AFTER RESET
;*   AND WE CAN SET IT $01 BEFORE ANY OPERATION

INIT    CLRA
        TFR     A,DP
        LDB     #$01       ; SET I/O EPROM PAGE TO 1 (DEFAULT PAGE)
        STB     ROMLATCH
        CLR     RAMLATCH   ; SET PAGINED RAM TO PAGE 0
        NOP
        JSR     CHKCF      ; CHECK IF CF PRESENT AND INIT CF

;*   WARM    PERFORMS ANY NECESSARY FUNCTIONS DURING FLEX WARMSTART. FLEX
;*           CALLS THIS ROUTINE EACH TIME IT GOES THRU THE WARM START
;*           PROCEDURE (AFTER EVERY COMMAND). AS AN EXAMPLE, SOME
;*           CONTROLLERS USE PIA'S FOR COMMUNICATION WITH THE PROCESSOR.
;*           IF FLEX IS EXITED WITH A CPU RESET, THESE PIA'S MAY ALSO BE
;*           RESET SUCH THAT THE CONTROLLER WOULD NOT FUNCTION PROPERLY
;*           UPON A JUMP TO THE FLEX WARM START ENTRY POINT. THIS ROUTINE
;*           COULD RE-INITIALIZE THE PIA WHEN THE WARM START WAS EXECUTED.
;*
;*           ENTRY - NO PARAMETERS
;*
;*           EXIT - A, B, X, Y, AND U MAY BE DESTROYED

WARM    RTS

;*   PARAM TABLE FOR ESS540 EPROM OPERATIONS AND FOR CF
;*   $DE80 -> DE91  RESERVED AS ESS540 EPROM DISK OPERATIONS
;*   $DE92 -> DEAF  RESERVED AS ESS540 EPROM DISPLAY OPERATIONS
;*   $DEB0 -> DEC3  RESERVED AS PARAMETERS FOR CF'S

        ORG     $DE80

DSKTAB   RMB     $DEB0-$DE80
CFOK     RMB     1             ; FLAG CF PRESENT TWO CF = 2 / ONE CF = 1 / NO CF = 0
CFSEL    RMB     1             ; FLEX SELECTED CF 0 = CF EE80 / 1 = CF EEF0
LBA0     RMB     1
LBA1     RMB     1
LBA2     RMB     1             ; ALWAYS 0 UNDER FLEX
LBA3     RMB     1             ; CLEAR LBA3, SET MASTER & LBA MODE -> $E0
CFTRK    RMB     1             ; TRACK ASKED BY FLEX FOR CF
CFSEC    RMB     1             ; SECTOR ASKED BY FLEX FOR CF
CFMTRK   RMB     1             ; TRACKS MAX ON SELECTED CF
CFMSEC   RMB     1             ; SECTORS PER TRACK ON SELECTED CF
CF1MTRK  RMB     1             ; TRACKS MAX ON CF1
CF1MSEC  RMB     1             ; SECTORS PER TRACK ON CF1
CF2MTRK  RMB     1             ; TRACKS MAX ON CF2
CF2MSEC  RMB     1             ; SECTORS PER TRACK ON CF2
CFADDRES RMB     2             ; SELECTED CF ADDRESS
CFSTATUL RMB     2             ; SELECTED CF STATUS ADDRESS 16 BITS
CFSEC16  RMB     2             ; SELECTED CF SECTOR NUMBER 16 BITS

;* JUMP TABLE FOR FLOPPY OPERATIONS

FLTAB   FDB     READ            ; DREAD
        FDB     WRITE           ; DWRITE
        FDB     VERIF           ; DVERIFY
;        FDB     RST             ; DRESTOR
;        FDB     DRV             ; DDRIVE
;        FDB     CHKRDY          ; DCHECK
;        FDB     QUICK           ; DQUICK
;        FDB     INIT            ; DINIT
;        FDB     WARM            ; DWARM
        FDB     SEEK            ; DSEEK

;* JUMP TABLE FOR CF OPERATIONS

CFTAB   FDB     RDCFSEC         ; DREAD
        FDB     WRCFSEC         ; DWRITE
        FDB     CFVRFY          ; DVERIFY
;        FDB     RST             ; DRESTOR - SAME FOR BOTH FLOPPY AND CF
;        FDB     DRV             ; DDRIVE - SAME FOR BOTH FLOPPY AND CF
;        FDB     CHKRDY          ; DCHECK - SAME FOR BOTH FLOPPY AND CF
;        FDB     QUICK           ; DQUICK - SAME FOR BOTH FLOPPY AND CF
;        FDB     INIT            ; DINIT - SAME FOR BOTH FLOPPY AND CF
;        FDB     WARM            ; DWARM - SAME FOR BOTH FLOPPY AND CF
        FDB     CFSEEK          ; DSEEK

;* RESERVE 3 BYTES FOR JUMP ADDRESS (2) AND JUMP PAGE NUMBER (1)
;* IN CASE OF I/O EPROM PAGE CHANGE

        RMB     3

;* DF00 - DFFF IS ASSIST-09 WORK AREA - AVOID USING THIS AREA

        END     COLDS
