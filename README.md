# ELEKTOR-EC-6809
-1985- ELEKTOR 6809 2 BOARDS FLEX COMPUTER

Original informations (schematics, bom etc...) published by Elektor about this computer are in ELEKTOR COMPUTING n° 3.

See here but in dutch or german only : http://retro.hansotten.nl/6502-sbc/elektuur-junior/elektor-junior-literature/elektor-books/

For french version see in Elektor n°100 10/1986 here : https://github.com/Wawavoun/ELEKTOR_EC-6809/tree/main/Elektor

ESS540 is the boot rom. Base address is $F000.
This eprom could need modification depending the screen you use (6845 table start at $F310). The eprom files are with the original table from Elektor. Mine is in a separate text file.
Also double density drive routines are for 16 sectors /track/side. Usually Flex use 18 sectors /track/side. Put $12 into $F1EE and $F1EF for change to 18 sectors by side.
Only drive 0 to 2 are supported.
The Flex loading routine in eprom load TR0/S1 and then check if the "FLEX" string is at $C1FC before starting the bootloader, if not no boot at all.

The ESS541 (Assist-09 monitor, base address $F800) eprom switch serial port to 2400 baud. Put NOP ($12) at $F905 and $F906 to avoid this and stay at 9600 baud.

I leave the original value. Change it if you want.

PCB include the Gerber files in zip archive if you want build the computer.
Included is a modified bus board with a power supply connector and a reset pushbutton.

WARNING : CPU gerber could need debug. I found an original empty board and did not build mine starting from those gerber !

FDC+VDU gerber are fully debugged.

FLEX and NEWDISK are "do it myself" adaptations. The original version was lost.

The printer driver send printed datas to the serial port. Use 9600 8N2 parameter and dont forget to wire and use rts/cts.
The PCPUT and PCGET commands has been adapted to the system. I use it with Minicom, same parameters.

In case you have information about the original Flex software coming with this computer please take contact.

11/2023

Philippe

This computer and all the eprom contents are Elektor creations.

Everything here is provided "AS IS" under GNU GPG V3.

If possible I will support but depending of my free time and interrest. 

If you use something still mention I am the author / adapter and send me a postcard from your country.
