
;* ELEKTOR EC6809 CONSOLE I/O DRIVER
;* 11/2023 PH. ROEHR

VIA     EQU     $EF80
CHPR    EQU     $C700       

JSWIVE  EQU     $DEAC      ; SWI3 VECTOR LOCATION
JIRQVE  EQU     $DEAE      ; IRQ VECTOR LOCATION

WTLDVI  EQU     $F033      ; GET // DATA INPUT
INIT    EQU     $F036      ; INIT OF VIA, ACIA, CRTC        
VIARDY  EQU     $F039      ; TEST // DATA READY        
JMPADR  EQU     $F03C      ; PUT CHAR ON SCREEN
GDTVIE  EQU     $F03F      ; GET // DATA INPUT WITH ECHO
ERTI    EQU     $F030      ; RETURN FROM INTERRUPT
ERTS    EQU     $F01B

COLDS   EQU     $CD00

        ORG     $D3E5

INCHN   FDB     JINCHN     ; INPUT CHARACTER WITHOUT ECHO
IHNDLR  FDB     ERTI       ; IRQ INTERRUPT HANDLER
SWIVEC  FDB     JSWIVE     ; SWI3 VECTOR LOCATION
IRQVEC  FDB     JIRQVE     ; IRQ VECTOR LOCATION
TMOFF   FDB     JTMOFF     ; TIMER OFF ROUTINE
TMON    FDB     JTMON      ; TIMER ON ROUTINE
TMINT   FDB     JTMINT     ; TIMER INITIALIZATION
MONITR  FDB     JMONIT     ; MONITOR ENTRY ADDRESS
TINIT   FDB     ERTS       ; TERMINAL INITIALIZATION
STAT    FDB     JSTAT      ; CHECK TERMINAL STATUS
OUTCH   FDB     JMPADR     ; OUTPUT CHARACTER
INCH    FDB     JINCH      ; INPUT CHARACTER WITH ECHO

        ORG     $D370

;* INPUT CHARACTER WITHOUT ECHO
JINCHN  JSR     WTLDVI   
        ANDA    #$7F       ; CLEAR PARITY BIT
        PULS    PC

;* INPUT CHARACTER WITH ECHO
JINCH   JSR     GDTVIE
        ANDA    #$7F       ; CLEAR PARITY BIT
        PULS    PC

;* CHECK VIA STATUS (IF CHAR IS WAITING Z=1, ELSE Z=0)      
JSTAT   PSHS    A
        JSR     VIARDY
        PULS    A,PC 
        
;* MONITOR ENTRY ADDRESS
JMONIT  SWI             
        FCB     $08
        
;* TIMER INITIALIZATION        
JTMINT  LDX    #VIA        
        LDA    #$40        
        STA    $0D,X       ; CLEAR INTERRUPT FLAG REGISTER        
        LDA    #$40        ; T1 FREE RUN MODE, NO OUTPUT ON PB7        
        STA    $0B,X       ; STARTS         
        LDD    #15000      ; 15 MS INTERRUPTS             
        STB    4,X         ; WRITE LOW ORDER LATCH        
        STA    5,X         ; WRITE HIGH ORDER LATCH        
        RTS        
        
;* TIMER ON ROUTINE        
JTMON   LDA    #$C0        ; ENABLE T1 IRQ        
        STA    VIA+$E        
        RTS        
        
;* TIMER OFF ROUTINE       
JTMOFF  LDA    #40         ; DISABLE T1 IRQ       
        STA    VIA+$E       
        RTS       

;* IRQ INTERCEPTION        
JIHNDL  PSHS   A           ; SAVES ON STACK
        LDA    VIA+$0D     ; GET IFR
        ASLA               ; TO CARRY
        ASLA
        BCC    NSPOOL
        LDA    #$40
        STA    $0D,X       ; CLEAR INTERRUPT FLAG REGISTER
        PULS   A           ; RESTORE STACK
        JMP    CHPR        ; JUMP TO SPOOLER
NSPOOL  PULS   A           ; RESTORE STACK
        RTI
        
;* CHANGE DEFAULT MEMEND VALUE (AS PER FAG)
        ORG     $CC2B
        FCB     $BE,$FF

        END     COLDS
