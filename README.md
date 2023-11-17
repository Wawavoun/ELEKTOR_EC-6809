# ELEKTOR-EC-6809
-1985- ELEKTOR 6809 2 BOARDS FLEX COMPUTER

ESS540 is the boot rom.

This eprom could need modification depending the screen you use (6845 table start at $310).

The eprom is with the original table from Elektor.

Mine is in a separate text file.

Also drive routines are for 16 sectors /track/side. Usually Flex use 18 sectors /track/side.

Again I leave the original value. Change it if you want.

The Flex loading routine in eprom load TR0/S1 and then check if the "FLEX" string is at $C1FC
before starting the bootloader, if not no boot.

Only drive 0 to 2 are supported.

ESS541 is the monitor rom (ASSIST-09).

ESS542 is the characters generator rom.

PCB include the Gerber files in zip archive if you want build the computer.
Included is a modified bus board with a power supply connector and a reset pushbutton.

FLEX and newdisk are "do it myself" adaptations. The original version was lost.

In case you have information about this software please take contact.


11/2023
Philippe
