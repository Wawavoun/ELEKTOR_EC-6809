
;* ELEKTOR EC6809 DISK DRIVER
;* 11/2023 PH. ROEHR

;*EPROM ROUTINES ADDRESS

EREAD   EQU     $F006
EWRITE  EQU     $F009
ERESTO  EQU     $F00F
EDRSEL  EQU     $F012
ESEEK   EQU     $F003 
EINIT   EQU     $F01E
EVERIF  EQU     $F015
ETEMPO  EQU     $F206
 
COLDS   EQU     $CD00       ; FLEX COLD ENTRY  
PRCNT   EQU     $CC34       ; PRINTER RUNNING FLAG

;* DISK DRIVER ROUTINE JUMP TABLE

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

;*   INIT    This routine performs any necessary initialization of the
;*           drivers during cold start (at boot time). Actually, any                     
;*           operation which must be done when the system is first booted                     
;*           can be done here.                     
;*                     
;*           ENTRY - No parameters                     
;*                     
;*           EXIT - A, B, X, Y, and U may be destroyed                     

INIT    LDA     #$00
        TFR     A,DP

;*   WARM    Performs any necessary functions during FLEX warmstart. FLEX
;*           calls this routine each time it goes thru the warm start                     
;*           procedure (after every command). As an example, some                     
;*           controllers use PIA's for communication with the processor.                     
;*           If FLEX is exited with a CPU reset, these PIA's may also be                     
;*           reset such that the controller would not function properly                     
;*           upon a jump to the FLEX warm start entry point. This routine                     
;*           could re-initialize the PIA when the warm start was executed.                     
;*                     
;*           ENTRY - No parameters                     
;*                     
;*           EXIT - A, B, X, Y, and U may be destroyed                     
 
WARM    RTS
    
;*   SEEK    Seeks to the track specified in the 'A' accumulator. In    
;*           double-sided systems, this routine should also select the  
;*           correct side depending on the sector number supplied in 'B'.   
;*  
;*           ENTRY - (A) = Track Number 
;*                   (B) = Sector Number    
;*  
;*           EXIT -  (X) May be destroyed (See text)    
;*                   (A) May be destroyed (See text)    
;*                   (B) = Error condition  
;*                   (Z) = 1 if no error    
;*                       = 0 if an error    
    
SEEK    PSHS    U,Y
        LDU     #DSKTAB
        JSR     ESEEK
        PULS    U,Y
        RTS 
    
;*   READ    This routine reads the specified sector into memory at the
;*           specified address. This routine should perform a seek                     
;*           operation if necessary. A sector is 256 bytes in length.                     
;*                     
;*           ENTRY - (X) = Address in memory where sector is to be placed.                     
;*                   (A) = Track Number                     
;*                   (B) = Sector Number                     
;*                     
;*           EXIT -  (X) May be destroyed                     
;*                   (A) May be destroyed                     
;*                   (B) = Error condition                     
;*                   (Z) = 1 if no error                     
;*                       = 0 if an error                             

READ    PSHS    U,Y
        TST     PRCNT
        BEQ     READ2
        SWI3              
        NOP
READ2   ORCC    #$10       ; DISABLE IRQ    
        LDU     #DSKTAB
        JSR     EREAD
        ANDCC   #$EF       ; ENABLE IRQ
        PULS    U,Y
        RTS

;*   WRITE   This routine writes the information from the specifed memory
;*           buffer area to the disk sector specified. This routine should
;*           perform a seek operation if necessary. A sector is 256 bytes
;*           in length.
;*
;*           ENTRY - (X) = Address of 256 bytes memory buffer containing
;*                         data to be written to disk
;*                   (A) = Track Number
;*                   (B) = Sector Number
;*
;*           EXIT -  (X) May be destroyed
;*                   (A) May be destroyed
;*                   (B) = Error condition
;*                   (Z) = 1 if no error
;*                       = 0 if an error

WRITE   PSHS    U,Y
        TST     PRCNT
        BEQ     WRIT2
        SWI3
        NOP
WRIT2   ORCC    #$10       ; DISABLE IRQ            
        LDU     #DSKTAB
        JSR     EWRITE
        ANDCC   #$EF       ; ENABLE IRQ
        PULS    U,Y
        RTS

;*   VERIFY  The sector just written to the disk is to be verified to
;*           determine if there are CRC errors. No seek is required as
;*           this routine will only be called immediately after a write
;*           single sector operation.
;*
;*           ENTRY - No entry parameters
;*
;*           EXIT -  (X) May be destroyed
;*                   (A) May be destroyed
;*                   (B) = Error condition
;*                   (Z) = 1 if no error
;*                       = 0 if an error

VERIF   PSHS    A,U,Y
        TST     PRCNT
        BEQ     VERI2
        SWI3
        NOP
VERI2   ORCC    #$10       ; DISABLE IRQ
        LDU     #DSKTAB
        JSR     EVERIF
        ANDCC   #$EF       ; ENABLE IRQ
        PULS    A,U,Y
        RTS     

;*   $DE80 -> DEA9  RESERVED AS ESS540 EPROM WORK RAM
        
        ORG     $DE80
        
DSKTAB  RMB     $DE80-$DEB0
        
        ORG     $DEB0

;*   RESTORE A RESTORE OPERATION (ALSO KNOWN AS A "SEEK TO TRACK 00") IS TO
;*           BE PERFORMED ON THE SPECIFIED DRIVE. THE DRIVE IS SPECIFIED
;*           IN THE FCB POINTED TO BY THE CONTENTS OF THE X REGISTER. NOTE
;*           THAT THE DRIVE NUMBER IS THE 4TH BYTE OF THE FCB. THIS
;*           ROUTINE SHOULD SELECT THE DRIVE BEFORE EXECUTING THE RESTORE
;*           OPERATION.

;*           ENTRY - (X) = FCB ADDRESS (3,X CONTAINS DRIVE NUMBER)

;*           EXIT -  (X) MAY BE DESTROYED
;*                   (A) MAY BE DESTROYED
;*                   (B) = ERROR CONDITION
;*                   (Z) = 1 IF NO ERROR
;*                       = 0 IF AN ERROR

RST     JSR     DRV
        PSHS    U,Y
        LDU     #DSKTAB
        LDA     3,X
        JSR     ERESTO
        PULS    U,Y 
        RTS

;*   DRIVE   THE SPECIFIED DRIVE IS TO BE SELECTED. THE DRIVE IS SPECIFIED
;*           IN THE FCB POINTED TO BY THE CONTENTS OF THE X REGISTER. NOTE
;*           THAT THE DRIVE NUMBER IS THE 4TH BYTE OF THE FCB.

;*           ENTRY - (X) = FCB ADDRESS (3,X CONTAINS DRIVE NUMBER)

;*           EXIT -  (X) MAY BE DESTROYED
;*                   (A) MAY BE DESTROYED
;*                   (B) = $0F IF NON-EXISTENT DRIVE
;*                       = ERROR CONDITION OTHERWISE
;*                   (Z) = 1 IF NO ERROR
;*                       = 0 IF AN ERROR
;*                   (C) = 0 IF NO ERROR
;*                       = 1 IF AN ERROR

DRV     PSHS    U,X,Y
        LDU     #DSKTAB
        LDA     $3,X
        JSR     EDRSEL
        PULS    U,X,Y
        RTS
                       
;*   CHKRDY  CHECK FOR A DRIVE READY CONDITION. THE DRIVE NUMBER IS FOUND
;*           IN THE SPECIFIED FCB (AT 3,X). IF THE USER'S CONTROLLER TURNS
;*           THE DRIVE MOTORS OFF AFTER SOME TIME DELAY, THIS ROUTINE
;*           SHOULD FIRST CHECK FOR A DRIVE READY CONDITION AND IF IT IS
;*           NOT READY, SHOULD DELAY LONG ENOUGH FOR THE MOTORS TO COME UP
;*           TO SPEED, THEN CHECK AGAIN. THIS DELAY SHOULD BE DONE ONLY IF
;*           NOT READY ON THE FIRST TRY AND ONLY IF NECESSARY FOR THE
;*           PARTICULAR DRIVES AND CONTROLLER! IF THE HARDWARE ALWAYS
;*           LEAVES THE DRIVE MOTORS ON, THIS ROUTINE SHOULD PERFORM A
;*           SINGLE CHECK FOR DRIVE READY AND IMMEDIATELY RETURN THE
;*           RESULTING STATUS. SYSTEMS WHICH DO NOT HAVE THE ABILITY TO
;*           CHECK FOR A DRIVE READY CONDITION SHOULD SIMPLY ALWAYS RETURN
;*           A READY STATUS IF THE DRIVE NUMBER IS VALID.

;*           ENTRY - (X) = FCB ADDRESS (3,X CONTAINS DRIVE NUMBER)

;*           EXIT -  (X) MAY BE DESTROYED
;*                   (A) MAY BE DESTROYED
;*                   (B) = ERROR CONDITION
;*                   (Z) = 1 IF DRIVE READY
;*                       = 0 IF NOT READY
;*                   (C) = 0 IF DRIVE READY
;*                       = 1 IF NOT READY

CHKRDY  LDA     $03,X
        CMPA    #$02       ; CHECK DRIVE 0 -> 2
        BLS     TEST
        LDB     #$80       ; LOAD CODE ERROR AND CLEAR Z    
        ORCC    #$01       ; SET C
        RTS
;*  MAKE A LONG DELAY
TEST    LDX     #$0FFF
TEST1   LBSR    ETEMPO
        LEAX    -1,X
        BNE     TEST1
        BRA     OK
        
;*   QUICK   THIS ROUTINE PERFORMS A "QUICK" DRIVE READY CHECK. ITS
;*           FUNCTION IS EXACTLY LIKE THE CHKRDY ROUTINE ABOVE EXCEPT THAT
;*           NO DELAY SHOULD BE DONE. IF THE DRIVE DOES NOT GIVE A READY
;*           CONDITION ON THE FIRST CHECK, A NOT READY CONDITION IS
;*           IMMEDIATELY RETURNED. ENTRY AND EXIT ARE AS ABOVE.

QUICK   LDA     $03,X
        CMPA    #$02       ; CHECK DRIVE 0 -> 2
        BLS     OK
        LDB     #$80       ; LOAD CODE ERROR AND CLEAR Z    
        ORCC    #$01       ; SET C
        RTS
OK      CLRB               ; ASSUME READY - SET Z
        ANDCC   #$FE       ; CLEAR C
        RTS

        END     COLDS
