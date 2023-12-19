******************************************************************
*
* PCPUT
*    Allows transfer of files from a 6809 FLEX  file system to
*    a PC using the XMODEM protocol. Either port on the console
*    MP-S2 serial board can be used for the transfer.
*
*    Ver    Date      Desc
*    1.0  02/09/22   Mike Douglas, Initial version
*
*    Adapted for Elektor EC-6809 by Ph. Roehr 12/2023
*
******************************************************************

* FLEX equates

FCB     equ $C840       ;FLEX system FCB
EOL     equ $CC02       ;EOL delimiter from TTYSET
FMS     equ $D406       ;entry point for FMS calls
FMSCLS  equ $D403       ;close all files
WARMS   equ $CD03       ;FLEX re-entry address
GETCHR  equ $CD15       ;get character from console
PSTRNG  equ $CD1E       ;display string on console
GETFIL  equ $CD2D       ;build FCB from command line parameter
NEXTCH  equ $CD27       ;get next char from command line
RPTERR  equ $CD3F       ;report error from the FCB

* FMS equates

OPENRD  equ $01         ;open for read
FMSEOF  equ $08         ;FMS EOF error code

* Offsets into the FMS file control block (FCB)

fcbCMD  equ 0           ;FMS command
fcbERR  equ 1           ;error status
fcbEXT  equ 12          ;file name extension
fcbBIN  equ 59          ;binary/space compression flag

* XMODEM equates

PKTLEN  equ 128         ;128 byte xmodem packet length
SOH equ $01
EOT equ $04
ACK equ $06
NAK equ $15
EOF equ $1A             ;ctrl-z character
XMTRIES equ 10          ;number of xmodem retries

* MP-S serial board equates

SIOADD  equ $EF60-4     ;ACIA base address
SIORDRF equ $01         ;receive data register full
SIOTDRE equ $02         ;transmit data register empty

* Misc equates

CPU2MH  equ 0           ;2MHz CPU flag, non-zero=2MHz

SPACE   equ $20         ;ASCII space
CR      equ $0D         ;ASCII carriage return
LF      equ $0A         ;ASCII line feed
CTRLC   equ $03         ;ASCII ctrl-c

*-----------------------------------------------------------------------------
* Start of code
*-----------------------------------------------------------------------------
    org $C100

pcPut   bra start
        fcb 10          ;version 1.0
sioAddr fdb SIOADD      ;ACIA base address
cpu2Mhz fcb CPU2MH      ;2MHz CPU flag

* Validate command line file name and parameters

start   ldx #FCB        ;use the system FCB
        jsr GETFIL      ;get file name from command line
        bcs dspHelp     ;invalid, display help

        lda #OPENRD     ;FMS open for read
        sta fcbCMD,x
        jsr FMS
        bne dskErr      ;file not found

        jsr getType     ;get file type
        jsr getParm     ;get command line parameters
        bne dspHelp     ;command line error

* Prompt the user to start the XMODEM receive operation on the PC. 
*    Wait for a clear receive line for a least one second, then start
*    looking for NAK from the PC.

        ldx #mStart     ;display start file transfer prompt
        jsr PSTRNG
        lda #1          ;init xmodem block number to one
        sta blkNum
        clr prvByte     ;set previous byte <> CR

clear1  jsr rcvByte     ;loop until input clear for 1 second
        bne clear1
    
waitNak jsr rcvByte     ;wait for a NAK = receiver ready
        cmpa #NAK
        beq rdFile      ;have NAK
    
        cmpa #CTRLC     ;abort?
        bne waitNak     ;no
        bra pgmExit     ;else exit

* File transfer loop
    
rdFile  ldx #xfrBuf     ;init bufPtr->xfrBuf
        stx bufPtr
        lda #PKTLEN     ;byteCnt=bytes in an XMODEM packet
        sta byteCnt
    
rdLoop  lda prvByte     ;was previous byte a CR?
        cmpa #CR
        bne rdNotCr     ;no

        lda fWindow     ;Windows style EOL (CR/LF)?
        beq rdNotCr     ;no
        lda #LF         ;else, yes, inject an LF
        bra rdStore

rdNotCr ldx #FCB        ;issue read byte command to FMS
        jsr FMS         ;get next byte
        bne readErr     ;EOF most likely

        cmpa #CR        ;carriage return?
        bne rdStore     ;no
        ldb fLinux      ;Linux style EOL (LF only)?
        beq rdStore     ;no
        lda #LF         ;else, substitute LF for CR

rdStore ldx bufPtr      ;put byte in XMODEM buffer
        sta 0,x+
        sta prvByte     ;save as previous byte sent

        stx bufPtr
        dec byteCnt
        bne rdLoop      ;repeat for 128 bytes (PKTLEN)
    
        bsr sndPkt      ;send the XMODEM packet
        bra rdFile      ;repeat
 
* dskErr, dspHelp - Disk error or command line parameter error

dskErr  jsr RPTERR      ;let FLEX report the error
        bra pgmExit

dspHelp ldx #mHelp
        jsr PSTRNG
        jmp WARMS

* readErr - If EOF, fill the remainder of the XMODEM packet and
*    send it.

readErr lda fcbERR,x    ;is the error code EOF?
        cmpa #FMSEOF
        bne dskErr      ;no, something else is wrong
 
        ldb byteCnt     ;on packet boundary?
        cmpb #PKTLEN
        beq sndDone     ;yes, nothing to fill, send complete
    
        lda #EOF        ;else, fill with CP/M EOF character
        ldx bufPtr
    
fillLp  sta 0,x+        ;fill with EOF
        decb
        bne fillLp
    
        bsr sndPkt      ;send final packet

sndDone bsr sndEot      ;EOT to say we're done
pgmExit jsr FMSCLS      ;close open files
        jmp WARMS       ;exit to FLEX

*-----------------------------------------------------------------------------
* sndPkt - Send the 128 byte packet in xfrBuf using the XMODEM protocol.
*-----------------------------------------------------------------------------
sndPkt  lda #XMTRIES    ;init retry counter
        sta xmRetry

* Send header bytes

reSend  lda #SOH        ;1st byte is SOH
        bsr sndByte
        lda blkNum      ;2nd byte is the block number
        bsr sndByte 
        coma            ;send complement of block number
        bsr sndByte

* Send the 128 byte data portion of the packet followed by checksum

        ldx #xfrBuf     ;X->packet data
        lda #PKTLEN     ;set transfer length
        sta byteCnt
        clr chkSum      ;init checksum

pktLoop lda 0,x+        ;get next byte to send
        bsr sndByte     ;send and update checksum
        dec byteCnt     ;decrement bytes remaining
        bne pktLoop

        lda chkSum      ;send the checksum
        bsr sndByte

* All bytes sent. Wait for the response.

        bsr rcvByte     ;get the response character
        beq sndFail     ;timeout on response
    
        cmpa #ACK       ;ack received?
        bne sndFail     ;no, send failed
    
        inc blkNum      ;increment block number
        rts

* sndFail - ACK not received, decrement retry count and try again.

sndFail dec xmRetry     ;decrement retry counter
        beq xmFail      ;retries used up, failed xmodem transfer

clear2  bsr rcvByte     ;wait for 1 second of clear line
        bne clear2

        bra reSend      ;re-send the packet

* xmFail - Display failure message then restart program

xmFail  ldx #mXmdm      ;xmodem failure message
        jsr PSTRNG
        bra pgmExit

*-----------------------------------------------------------------------------
*  sndEot - Send EOT character and wait for ACK response
*-----------------------------------------------------------------------------
sndEot  lda #XMTRIES    ;init retry counter
        sta xmRetry

reEot   lda #EOT        ;send the EOT
        bsr sndByte
        bsr rcvByte     ;wait for response
        beq eotFail     ;timeout

        cmpa #ACK       ;ACK received?
        bne eotFail     ;no, EOT send failed

eotExit rts             ;otherwise, we're done.

* Timeout waiting for ACK to EOT. Decrement retry counter and try again.

eotFail dec xmRetry     ;decrement retry counter
        beq eotExit     ;just give up - xfer was probably good

clear3  bsr rcvByte     ;wait for 1 second of clear line
        bne clear3

        jmp reEot       ;try again

*-----------------------------------------------------------------------------
* sndByte - Send the byte in A through the serial port. Adds the byte 
*     to chkSum. Clobbers B.
*-----------------------------------------------------------------------------
sndByte tfr a,b         ;B=character to send
        addb    chkSum  ;update the checksum
        stb chkSum

        stx ioSaveX     ;preserve X
        ldx portAdr     ;X->serial port
        
sbLoop  ldb 0,x         ;wait for OK to transmit
        andb    #SIOTDRE
        beq sbLoop  

        sta 1,x         ;send the character
        ldx ioSaveX     ;restore X
        rts

*-----------------------------------------------------------------------------
* rcvByte - Receive a byte from the serial port with a one second
*     timeout. If a timeout occurs, zero is returned in A and the zero 
*     flag is true. Otherwise, the character is returned in A and zero 
*     flag is false. 
*-----------------------------------------------------------------------------
rcvByte ldx #30303      ;one second, 1MHz, 33 cycles per loop
        lda cpu2Mhz     ;2MHz CPU?
        beq rbLoop      ;no, 1MHz
        ldx #60606      ;else double count for 2MHz

rbLoop  stx ioSaveX     ;(6) preserve counter
        ldx portAdr     ;(6) X->serial port 
        lda 0,x         ;(4) see if a new character is present
        anda #SIORDRF   ;(2)
        bne rbRead      ;(2) new character present

        ldx ioSaveX     ;(6) restore counter
        leax -1,x       ;(5) decrement timeout
        bne rbLoop      ;(2)

        rts             ;timeout, exit with Z true
    
rbRead  lda 1,x         ;return character in A
        andcc #$FB      ;ensure Z is false
        rts

*-----------------------------------------------------------------------------
* getType - Get file type (binary or text). If binary, fcbBIN in the FCB
*   is set to $FF to tell the FMS to treat the file as binary.
*-----------------------------------------------------------------------------
getType lda #1          ;default to Windows text file
        sta fWindow
        clr fLinux
        ldx #txtTbl     ;X->table of text file types

gtLoop  lda 0,x         ;test for end of table
        beq gtBin       ;match not found, treat as binary
    
        cmpa FCB+fcbEXT ;compare 1st byte of extension
        bne gtNext      ;doesn't match

        lda 1,x         ;compare 2nd byte
        cmpa FCB+fcbEXT+1
        bne gtNext      ;doesn't match

        lda 2,x         ;compare 3rd byte
        cmpa FCB+fcbEXT+2
        beq gtExit      ;found match, leave as text file

gtNext  leax 3,x        ;move to next entry
        bra gtLoop

gtBin   lda #$FF        ;set binary mode for FMS
        sta FCB+fcbBIN
        clr fWindow     ;can't be Windows text mode
gtExit  rts

* txtTbl- Table of "strong" text file types

txtTbl  fcc "TXT"
        fcc "ASM"
        fcc "BAS"
        fcb 0           ;terminate the table

*-----------------------------------------------------------------------------
* getParm - Get command line parameters. Look for optional digit specifying
*    text file flags W or L to force a text
*    mode transfer with CR/LF ("W"indows flag) or just CR ("L"inux flag).
*    Returns zero false for invalid parameters.
*-----------------------------------------------------------------------------
getParm ldx sioAddr     ;default to console port
        leax    4,x     ;bottom port is console
        stx portAdr
    
gpLoop  jsr NEXTCH      ;get next command line character
        cmpa #CR        ;terminate with CR or EOL character 
        beq gpExit
        cmpa EOL
        beq gpExit

        cmpa    #SPACE  ;skip delimiters
        beq gpLoop
        cmpa    #',
        beq gpLoop

        cmpa #'W        ;Windows text file option?
        beq gpWin       ;yes

        cmpa #'L        ;Linux text file option?
        bne gpExit      ;no, invalid command line

        inc fLinux      ;use Linux EOL (LF)
        clr fWindow     ;not Windows EOL
        bra gpLinux

gpWin   inc fWindow     ;set Windows EOL mode
gpLinux clr FCB+fcbBIN  ;force text file mode
gpExit  rts

*-----------------------------------------------------------------------------
* Message constants 
*-----------------------------------------------------------------------------
mHelp   fcb CR,LF
        fcc "PCPUT v1.0 for Elektor EC-6809 FLEX"
        fcb CR,LF
        fcc "Ph. Roehr - 12/2023"
        Fcb CR,LF,LF
        fcc "Usage:"
        fcb CR,LF
        fcc "  PCPUT FILE.EXT [W|L]"
        fcb CR,LF,LF
        fcc "Where:"
        fcb CR,LF
        fcc "  W|L = Force transfer as 'W'indows or 'L'inux text file"
        fcb CR,LF,LF
        fcc "Defaults:"
        fcb CR,LF
        fcc "  Transfer Port: ACIA located at $EF60"
        fcb CR,LF
        fcc "  Extensions TXT, ASM, and BAS transfer as a Windows"
        fcb CR,LF
        fcc "  compatible text file. All others as a binary file."
        fcb CR,LF,EOT
    
mStart  fcc "Start XMODEM receive on the PC now..."
        fcb EOT

mXmdm   fcc "XMODEM communication failure"
        fcb CR,LF,EOT

*-----------------------------------------------------------------------------
* Data area
*-----------------------------------------------------------------------------
bufPtr  rmb 2       ;pointer into transfer buffer
byteCnt rmb 1       ;byte count remaining
prvByte rmb 1       ;previous byte read
fWindow rmb 1       ;non-zero=Windows text (CR/LF for EOL)
fLinux  rmb 1       ;non-zero=Linux text (LF for EOL)

portAdr rmb 2       ;serial port address
xmRetry rmb 1       ;xmodem retry counter
blkNum  rmb 1       ;current xmodem block number
chkSum  rmb 1       ;packet checksum
ioSaveX rmb 2       ;save X for I/O subroutines

xfrBuf  rmb PKTLEN  ;XMODEM transfer buffer

        end pcPut
