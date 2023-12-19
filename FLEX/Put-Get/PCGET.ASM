******************************************************************
*
* PCGET9
*    Allows transfer of files from a PC to a 6809 FLEX file 
*    system using the XMODEM protocol. Either port on the
*    console MP-S2 serial board can be used for the transfer.
*
*    Ver    Date      Desc
*    1.0  02/09/22   Mike Douglas, Initial version
*
*    Adapted to Elektor EC-6809 by Ph. Roehr 12/2023
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

OPENWRT equ $02         ;open for write
DELFILE equ $0C         ;delete file
EXISTS  equ $03         ;file exists error
SECLEN  equ 252         ;length of user data in sector

* Offsets into the FMS file control block (FCB)

fcbCMD  equ 0           ;FMS command
fcbERR  equ 1           ;error status
fcbFN   equ 4           ;file name
fcbEXT  equ 12          ;file name extension
fcbMAP  equ 23          ;set to $02 for random file
fcbBIN  equ 59          ;binary/space compression flag

* XMODEM equates

PKTLEN  equ 128         ;128 byte xmodem packet length
SOH     equ $01
EOT     equ $04
ACK     equ $06
NAK     equ $15
EOF     equ $1A         ;ctrl-z character
XMTRIES equ 10          ;number of xmodem retries

* MP-S2 serial board equates

SIOADD  equ $EF60-4     ;ACIA base address
SIORDRF equ $01         ;receive data register full
SIOTDRE equ $02         ;transmit data register empty

* Misc equates

CPU2MH  equ 0           ;2MHz CPU flag, non-zero=2MHz

SPACE   equ $20         ;ASCII space 
TAB     equ $09         ;ASCII tab
CR      equ $0D         ;ASCII carriage return
LF      equ $0A         ;ASCII line feed
CTRLC   equ $03         ;ASCII ctrl-c

*-----------------------------------------------------------------------------
* Start of code
*-----------------------------------------------------------------------------
        org $C100

pcGet   bra start
        fcb 10          ;version 1.0
sioAddr fdb SIOADD      ;ACIA base address
cpu2Mhz fcb CPU2MH      ;CPU 2MHz flag

* Validate command line file name and parameters

start   ldx #FCB        ;use the system FCB
        jsr GETFIL      ;get file name from command line
        bcs dspHelp     ;invalid, display help

        jsr opeWrt      ;open file for write
        bne dskErr      ;failed
    
        jsr getType     ;get file type
        jsr getParm     ;get command line parameters
        bne dspHelp     ;command line error

* Prompt the user to send the file, send NAK to tell the host we're 
*    ready for XMODEM reception.

        ldx #mStart     ;display start file transfer prompt
        jsr PSTRNG
    
        ldx #xfrBuf     ;init pointers
        stx insPtr      ;receive insert pointer
        stx remPtr      ;remove pointer
        clr mod252      ;clear modulus 252 accumulator
    
        lda #1          ;init xmodem block number to one
        sta blkNum
        lda #NAK        ;send starting NAK after purging input data
        jsr purgSio

        jsr rcvPkt      ;queue and remain one packet ahead
        bne pgmExit     ;zero length file
        lda #ACK        ;else, ack the packet
        jsr sndByte
    
* File transfer loop
    
wrtFile lda mod252      ;mod252 = total bytes received MOD 252
        adda #PKTLEN    ;add bytes received
        bcs subMod      ;>252, so subtract 252
        cmpa #SECLEN    ;>=252?
        bcs saveMod     ;no
subMod  suba #SECLEN    ;perform stepwise MOD 252 as we go along
saveMod sta mod252      ;save updated value
    
        bsr rcvPkt      ;get next XMODEM packet
        bne rcvDone     ;EOT received, all done
        bsr wrtPkt      ;else, write the packet to disk
        bra wrtFile     ;repeat

rcvDone bsr wrtLast     ;write last packet (possibly partial)
    
pgmExit lda #ACK        ;ack possible eot
        jsr sndByte
        jsr FMSCLS      ;close open files
        jmp WARMS       ;exit to FLEX
    
* dskErr, dspHelp - Disk error or command line parameter error

dskErr  jsr RPTERR      ;let FLEX report the error
        bra pgmExit

dspHelp ldx #mHelp
        jsr PSTRNG
        jmp WARMS

*-----------------------------------------------------------------------------
* wrtPkt - Write PKTLEN bytes to disk from the XMODEM buffer.
* wrtLast - Same but for the final write. For binary files, this is most
*    likely less than a full packet based on mod252.
*-----------------------------------------------------------------------------
wrtLast lda fText       ;text file?
        bne wrtPkt      ;yes, process all bytes
    
        lda #PKTLEN     ;compute number of bytes left to write
        suba mod252     ;write count = 128 minus the excess bytes       
        bra wpSave

* wrtPkt entry

wrtPkt  lda #PKTLEN     ;write 128 bytes
wpSave  sta byteCnt     ;byteCnt is num bytes to write
    
wdLoop  ldx remPtr      ;get next byte from xfrBuf
        lda 0,x+
        stx remPtr
    
        ldb fText       ;text file?
        beq wdBin       ;no

        cmpa #CR        ;ASCII CR?
        beq wdSkip      ;yes, skip it
        cmpa #EOF       ;ASCII (CP/M) EOF?
        beq wdSkip      ;yes, skip it

        cmpa #LF        ;ASCII LF?
        bne wdTab       ;no, check for tab
        lda #CR         ;substitute CR
        bra wdBin

wdTab   cmpa #TAB       ;is byte a tab?
        bne wdBin       ;no
        lda #SPACE      ;else, substitute space for tab
        
wdBin   ldx #FCB        ;X->FCB for disk I/O    
        jsr FMS         ;write the byte
        bne dskErr

wdSkip  dec byteCnt
        bne wdLoop      ;repeat for 128 bytes (typically)

        ldx remPtr      ;increment and wrap the remove pointer
        cpx #BUFEND
        bne wdAck
        ldx #xfrBuf
        stx remPtr

wdAck   lda #ACK        ;ack the packet
        jsr sndByte
        rts

*-----------------------------------------------------------------------------
* rcvPkt - Receive a 128 byte xmodem packet into xfrBuf as pointed to
*   by insPtr. Updates insPtr on exit.
*-----------------------------------------------------------------------------
nakPkt  lda #NAK        ;NAK the packet
        jsr purgSio     ;purge receive for 1s then xmit NAK

* Wait for SOH, EOT or CTRL-C

rcvPkt  lda #3          ;3 second timeout for SOH
        jsr rcvByte     
        beq nakPkt      ;timeout

        cmpa    #EOT    ;EOT to say we're done?
        bne notEot      ;no
        tsta            ;return zero false
        rts

notEot  cmpa #CTRLC     ;ctrl-c to abort?
        bne notCtlc     ;no
        tsta            ;return zero false
        rts
    
notCtlc cmpa #SOH        ;SOH received?
        bne rcvPkt      ;no, keep looking

        bsr rcv1Sec     ;get the block number (1s timeout)
        beq nakPkt      ;timeout, NAK and start over
        sta rcvBlk      ;save the received block number
    
        bsr rcv1Sec     ;get not block number
        beq nakPkt      ;timeout, NAK and start over
        sta rcvNBlk     ;save not block number

* Receive the 128 byte data portion of the packet

        ldx insPtr      ;init buffer pointer
        lda #PKTLEN     ;set transfer length
        sta byteCnt
        clr chkSum      ;init checksum

pktLoop bsr rcv1Sec     ;get data byte (1s timeout)
        beq nakPkt      ;timeout

        sta 0,x+        ;save the byte
        adda chkSum     ;update checksum
        sta chkSum

        dec byteCnt     ;decrement byte counter
        bne pktLoop     ;and repeat

* Verify the checksum

        bsr rcv1Sec     ;get data byte
        beq nakPkt      ;timeout

        cmpa chkSum     ;checksum match
        bne nakPkt      ;no, packet error

* Checksum good, verify block numbers

        lda rcvNBlk     ;verify rcvBlk = not(rcvNBlk)
        coma
        cmpa rcvBlk     ;match?
        bne nakPkt      ;no, packet error

        cmpa blkNum     ;rcvBlk = expected bklNum?
        bne nakPkt      ;no, packet error
    
        inca            ;increment expected block number
        sta blkNum
    
        cpx #BUFEND     ;at end of xfrBuf?
        bne savePtr     ;no
        ldx #xfrBuf     ;reset to start
savePtr stx insPtr
        clra            ;return zero status
        rts

*-----------------------------------------------------------------------------
* purgSio - Wait for 1 second of idle time on receive line, then transmit the
*   character passed in A (usually ACK or NAK). Clobbers B.
*-----------------------------------------------------------------------------
purgSio pshs a          ;save character to send

purge   bsr rcv1Sec     ;wait for 1 second without activity
        bne purge

        puls a          ;get back the character to send
*                       ;fall into to sndByte
                
*-----------------------------------------------------------------------------
* sndByte - Send the byte in A through the serial port. Clobbers B.
*-----------------------------------------------------------------------------
sndByte stx ioSaveX     ;preserve X
        ldx portAdr     ;X->serial port
        
sbLoop  ldb 0,x         ;wait for OK to transmit
        andb #SIOTDRE
        beq sbLoop  

        sta 1,x         ;send the character
        ldx ioSaveX     ;restore X
        rts

*-----------------------------------------------------------------------------
* rcvByte - Receive a byte from the serial port with a timeout specified
*     in seconds in A. If a timeout occurs, zero is returned in A and the
*     zero flag is true. Otherwise, the character is returned in A and zero 
*     flag is false. Clobbers A, B
*-----------------------------------------------------------------------------
rcv1Sec lda #1          ;one second timeout

rcvByte stx saveX       ;preserve X
        ldb cpu2Mhz     ;2MHz CPU?
        beq rb1Sec      ;no
        asla            ;else double count for 2MHz

rb1Sec  ldx #30303      ;one second, 1MHz, 33 cycles per loop

rbLoop  stx ioSaveX     ;(6) preserve counter
        ldx portAdr     ;(6) X->serial port
        ldb 0,x         ;(4) see if a new character is present
        andb #SIORDRF   ;(2)
        bne rbRead      ;(2) new character present

        ldx ioSaveX     ;(6) restore counter
        leax    -1,x        ;(5) decrement timeout
        bne rbLoop      ;(2)

        deca            ;decrement seconds count
        bne rb1Sec
    
        ldx saveX       ;restore X
        clra            ;exit with zero status (timeout)
        rts
    
rbRead  lda 1,x         ;A=byte received
        ldx saveX       ;restore X (non-zero)
        rts

*-----------------------------------------------------------------------------
* opeWrt - Open file for write. If the file already exists, prompt
*    the user to confirm replacement. Exits with zero true if the
*    open is successful.
*-----------------------------------------------------------------------------
opeWrt  ldx #FCB        ;X->FCB to use
        lda #OPENWRT    ;FMS open for write
        sta fcbCMD,x
        jsr FMS
        beq owExit      ;new file opened

        lda fcbERR,x    ;status code from open
        cmpa #EXISTS    ;file exists error?
        bne owExit      ;no, some other error
    
        ldx #mExists    ;"file exists, delete?"
        jsr PSTRNG
        jsr GETCHR      ;get byte from the console
        ora #$20        ;upper case letters to lower case
        cmpa #'y        ;confirmed?
        beq owDelet     ;yes, delete
        jmp WARMS       ;else exit to FLEX
    
owDelet ldx #mWork      "Working..."
        jsr PSTRNG

        ldx #FCB        ;delete the file
        lda #DELFILE
        sta fcbCMD,x
        lda fcbFN,x     ;save 1st byte of file name
        sta saveX
        jsr FMS
        bne owExit      ;delete failed
    
        lda saveX       ;restore first char of file name
        sta fcbFN,x
        lda #OPENWRT    ;FMS open for write
        sta fcbCMD,x
        jsr FMS
    
owExit  rts

*-----------------------------------------------------------------------------
* getType - Get file type (binary or text). If binary, fcbBIN in the FCB
*   is set to $FF to tell the FMS to treat the file as binary.
*-----------------------------------------------------------------------------
getType ldx #txtTbl     ;X->table of text file types
        lda #$FF        ;default to text file
        sta fText

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
        clr fText       ;not a text file
gtExit  rts

* txtTbl- Table of "strong" text file types

txtTbl  fcc "TXT"
        fcc "ASM"
        fcc "BAS"
        fcb 0           ;terminate the table

*-----------------------------------------------------------------------------
* getParm - Get command line parameters. Look for
*    the text file flags (W, L), or 'R'andom file flag.
*    Returns zero false for invalid parameters.
*-----------------------------------------------------------------------------
getParm ldx sioAddr     ;default to console port
        leax 4,x        ;bottom port is console
        stx portAdr
    
gpLoop  jsr NEXTCH      ;get next command line character
        cmpa #CR        ;terminate with CR or EOL character 
        beq gpExit
        cmpa EOL
        beq gpExit

        cmpa #SPACE     ;skip delimiters
        beq gpLoop
        cmpa #',
        beq gpLoop
    
        cmpa #'W        ;Windows text file option?
        beq gpText      ;yes

        cmpa #'L        ;Linux text file option?
        beq gpText      ;yes

        cmpa #'r        ;random file?
        bne gpExit      ;no, have bad command line
    
        lda #2          ;indicate random file
        sta FCB+fcbMAP
        clra            ;exit with zero status
        rts 

gpText  sta fText       ;set text flag true
        clr FCB+fcbBIN  ;force text file mode
gpExit  rts

*-----------------------------------------------------------------------------
* Message constants 
*-----------------------------------------------------------------------------
mHelp   fcb CR,LF
        fcc "PCGET v1.0 for Elektor EC-6809 FLEX"
        fcb CR,LF
        fcc "Ph. Roehr - 12/2023"
        fcb CR,LF,LF
        fcc "Usage:"
        fcb CR,LF
        fcc "  PCGET FILE.EXT [W|L|R]"
        fcb CR,LF,LF
        fcc "Where:"
        fcb CR,LF
        fcc "  W|L = Force transfer as 'W'indows or 'L'inux text file"
        fcb CR,LF
        fcc "  R = Treat as 'R'andom file"
        fcb CR,LF,LF
        fcc "Defaults:"
        fcb CR,LF
        fcc "  Transfer port: ACIA located at $EF60"
        fcb CR,LF
        fcc "  Extensions TXT, ASM, and BAS transfer as a Windows"
        fcb CR,LF
        fcc "  or Linux text file. All others as a binary file."
        fcb CR,LF,EOT
    
mStart  fcc "Start XMODEM send on the PC now..."
        fcb EOT

mExists fcc "File exists, erase? "
        fcb EOT

mWork   fcc "Working..."
        fcb EOT

*-----------------------------------------------------------------------------
* Data area
*-----------------------------------------------------------------------------
remPtr  rmb 2           ;xfrBuf remove pointer
byteCnt rmb 1           ;byte count remaining
mod252  rmb 1           ;byte count for last write
saveX   rmb 2           ;generic save location for X
fText   rmb 1           ;non-zero if text file

portAdr rmb 2           ;serial port address
insPtr  rmb 2           ;xfrBuf insert pointer
xmRetry rmb 1           ;xmodem retry counter
blkNum  rmb 1           ;current xmodem block number
chkSum  rmb 1           ;packet checksum
rcvBlk  rmb 1           ;block number received
rcvNBlk rmb 1           ;NOT block number received
ioSaveX rmb 2           ;save X for I/O subroutines

xfrBuf  rmb 2*PKTLEN    ;XMODEM transfer buffer
BUFEND  equ *           ;end of buffer address

    end pcGet
