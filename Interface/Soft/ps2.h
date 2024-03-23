/*
 * This code is based of of the original at:
 * https://www.engineersgarage.com/embedded/arduino/how-to-interface-ps2-keyboard-arduino
 * Portions (C) Ajish Alfred
 *
 * However, the code published at that URL has a number of bugs. One of the
 * bugs should have prevented the code from functioning properly at all, so
 * I am uncertain which version of code was used to shoot the video.
 *
 * Regardless, a number of fixes / changes / updates have been made to the
 * code as it was originally published:
 *
 * - Bit counting/shifting in original keyboard ISR has been fixed. Correct
 *   scan codes are now received.
 * - Added actual parity checking of data received from keyboard
 * - Capture self-test result code after reset is sent to keyboard, rather
 *   than passing it back to calling code as a keystroke
 * - 8 missing scan codes (\|;:'") have been added to ASCII conversion
 * - Numlock state + LED handling added
 * - Scroll Lock state + LED handling added
 * - Added ASCII output for control characters (CTRL-A, CTRL-B, etc)
 * - Added code outputs for F1-F12,PSCN,PAUSE
 *
 * I'm sure there are bug still lurking in here...this could use a re-write
 * with a proper state machine.
 * 
 * Portions (C) 2019 Mike Smith
 */

#ifndef PS2_H
#define PS2_H


void kbd_send_command(uint8_t);
void kbd_reset();
void kbd_set_lights(uint8_t);
void kbd_init(void);
uint8_t kbd_available();
uint8_t kbd_read_status();
uint8_t kbd_read();
uint8_t esc_mode; /* declared here because used in nano-ps2.c and into ps2.c - Esc key send 0x1B (1) or 0x8B (0) */

/*
 * PS2 keyboard codes to check for certain keys. These are returned by
 * kbd_read()
 */

#define PS2_KC_BKSP    0x80
#define PS2_KC_UP      0x81
#define PS2_KC_DOWN    0x82
#define PS2_KC_LEFT    0x83
#define PS2_KC_RIGHT   0x84
#define PS2_KC_PGDN    0x85
#define PS2_KC_PGUP    0x86
#define PS2_KC_END     0x87
#define PS2_KC_HOME    0x88
#define PS2_KC_INS     0x89
#define PS2_KC_DEL     0x8A
#define PS2_KC_ESC     0x8B
#define PS2_KC_CLON    0x8C
#define PS2_KC_CLOFF   0x8D
#define PS2_KC_NUMLON  0x8E
#define PS2_KC_NUMLOFF 0x8F
#define PS2_KC_SCLON   0x90
#define PS2_KC_SCLOFF  0x91

#define PS2_KC_F1      0xA0
#define PS2_KC_F2      0xA1
#define PS2_KC_F3      0xA2
#define PS2_KC_F4      0xA3
#define PS2_KC_F5      0xA4
#define PS2_KC_F6      0xA5
#define PS2_KC_F7      0xA6
#define PS2_KC_F8      0xA7
#define PS2_KC_F9      0xA8
#define PS2_KC_F10     0xA9
#define PS2_KC_F11     0xAA
#define PS2_KC_F12     0xAB

#define PS2_KC_PSCR    0xB0
#define PS2_KC_PAUSE   0xB1



#endif /* PS2_H */
