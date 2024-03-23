/*
 * This code is based of of the original at:
 * https://www.engineersgarage.com/embedded/arduino/how-to-interface-ps2-keyboard-arduino
 * Portions (C) Ajish Alfred
 *
 * However, the code published at that URL has a number of bugs. One of the
 * bugs should have prevented the code from functioning properly at all, so
 * I am uncertain which version of the code was used to shoot the video.
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


#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>
#include "defines.h"
#include <util/delay.h>
#include "util.h"
#include "serialio.h"
#include "ps2.h"

/*
 * Arduino Nano connecctions:
 * PS/2 keyboard CLK pin --> D2
 * PS/2 kyeboard DATA pin --> D3
 * No external pull up resistors required.
 */


/* variables used in ISR during keyboard data receive */
static volatile uint8_t kbd_char_buffer;
static volatile uint8_t kbd_buffer;
static volatile uint8_t kbd_bit_count;

/* variables for keyboard state - TRUE/FALSE */
static volatile uint8_t kbd_shift;
static volatile uint8_t kbd_ctrl; 
static volatile uint8_t kbd_alt;  
static volatile uint8_t kbd_extend;
static volatile uint8_t kbd_release;
static volatile uint8_t kbd_caps_lock;
static volatile uint8_t kbd_num_lock; 
static volatile uint8_t kbd_scroll_lock;

/* used in command transmission to keyboard */
static volatile uint8_t cmd_in_progress;
static volatile int     cmd_count;
static          uint8_t cmd_value;
static volatile uint8_t cmd_ack_value;
static          uint8_t cmd_parity;
static volatile uint8_t cmd_ack_byte_ok;


/* calculate odd parity bit needed for byte */
uint8_t odd_parity(uint8_t val)
{
  int i, count = 1;  // start with 0 for even parity
  for (i=0; i<8; i++)
    {
      if (val&1) count++;
      val = val>>1;
    }

  return count & 1; // bottom bit of count is parity bit
}


/* send a command byte to the keyboard */
void kbd_send_command(uint8_t val)
{
  // stop interrupt routine from receiving characters so that we can use it
  // to send data instead
  cmd_in_progress = TRUE;
  cmd_count       = 0;

  // set up the byte to shift out and initialise the ack bit
  cmd_value      = val;
  cmd_ack_value  = 1;    // the kbd will clear this bit on receiving the byte
  cmd_parity     = odd_parity(val);

  // set the data pin as an output, ready for driving
  setBit(PORTD,PD3);
  setBit(DDRD,PD3);

  // drive clock pin low - this is going to generate the first
  // interrupt of the shifting out process
  setBit(DDRD,PD2);
  clearBit(PORTD,PD2);

  // wait at least one clock cycle (in case the kbd is mid transmission)
  //delayMicroseconds(60);
  _delay_loop_2(960); /* about 60us at 16MHz */

  // set up the 0 start bit
  clearBit(PORTD,PD3);

  // let go of clock - the kbd takes over driving the clock from here
  setBit(PORTD,PD2);
  clearBit(DDRD,PD2);

  // wait for interrupt routine to shift out byte, parity and receive ack bit
  while (cmd_ack_value!=0) ;

  // switch back to the interrupt routine receiving characters from the kbd
  cmd_in_progress = FALSE;
}



/* sends a reset command to the keyboard and re-initialises all the control */
/* variables within the PS2Keybaord code. */
void kbd_reset()
{
  uint16_t count=0;
  uint8_t key;
  
  kbd_send_command(0xFF);   // send the kbd reset code to the kbd: 3 lights
                            // should flash briefly on the kbd

  // reset all the global variables
  kbd_buffer        = 0;
  kbd_char_buffer   = 0;
  kbd_bit_count       = 0;
  kbd_shift         = FALSE;
  kbd_ctrl          = FALSE;
  kbd_alt           = FALSE;
  kbd_extend        = FALSE;
  kbd_release       = FALSE;
  kbd_caps_lock     = FALSE;
  kbd_num_lock      = FALSE;
  kbd_scroll_lock   = FALSE;
  cmd_in_progress   = FALSE;
  cmd_count         = 0;
  cmd_value         = 0;
  cmd_ack_value     = 1;

  /* wait up to 500 ms for 'AA' self-test byte after reset */
  while( count < 500 )
    {
      if( kbd_available() )
	{
	  key=kbd_read();
	  if( key == 0xAA ) break;
	}
      _delay_ms(1);
      count++;
    }
}


/* set keyboard lights: bit_2=caps_lock, bit_1=num_lock, bit_0=scroll_lock */
void kbd_set_lights(uint8_t val)
{
  // When setting the lights with the 0xED command the keyboard responds
  // with an "ack byte", 0xFA. This is NOT the same as the "ack bit" that
  // follows the succesful shifting of each command byte.

  cmd_ack_byte_ok = FALSE;   // initialise the ack byte flag
  kbd_send_command(0xED);    // send the command byte
  while (!cmd_ack_byte_ok) ; // ack byte from keyboard sets this flag
  kbd_send_command(val);     // now send the data
}


/* The ISR for the external interrupt on pin D2, falling edge */
ISR(INT0_vect)
{
  int value;

  value = PIND & BV(3) ? 1 : 0; /* read the data pin */

  /********** TRANSMIT SECTION **********/
  
  // This is the code to send a byte to the keyboard. Actually its 12 bits:
  // a start bit, 8 data bits, 1 parity, 1 stop bit, 1 ack bit (from the kbd)
  if (cmd_in_progress)
    {
      cmd_count++;
      switch (cmd_count)
	{
	case 1: /* start bit - always a 0 */
	  clearBit(PORTD,PD3);
	  break;
	  
	case 2:  /* send data bits 0-7, LSB to MSB */
	case 3:
	case 4:
	case 5:
	case 6:
	case 7:
	case 8:
	case 9:
	  if( cmd_value&1 )
	    setBit(PORTD,PD3);
	  else
	    clearBit(PORTD,PD3);
	  cmd_value = cmd_value>>1;
	  break;

	case 10: /* transmit parity bit */
	  if( cmd_parity )
	    setBit(PORTD,PD3);
	  else
	    clearBit(PORTD,PD3);
	  break;

	case 11: /* send stop bit. always a 1 */
	  // release the data pin, so stop bit actually relies on pull-up
	  // for the next bit (ack bit)
	  setBit(PORTD,PD3);
	  clearBit(DDRD,PD3);
	  break;

	case 12: // ack bit - driven by the kbd, so we read its value
	  //cmd_ack_value = digitalRead(kbd_data_pin);
	  cmd_ack_value = PIND & BV(3) ? 1 : 0; 
	  cmd_in_progress = FALSE;  // done shifting out, byte sent
	}
      
      return; // don't fall through to the receive section of the ISR
    }

  /************ RECEIVE SECTION ************/
  
  /* Bits are read when clock line is low - this is why ISR is set to falling*/
  /* edge trigger. */

  /* First bit, start bit is always 0. It happens with kbd_bit_count = 0 */

  /* For bits 1-8 (the data byte itself), shift in bits from LSB to MSB */
  if(kbd_bit_count > 0 && kbd_bit_count < 9 )
    {
      kbd_buffer |= (value << (kbd_bit_count - 1));

#ifdef SCANCODE_DEBUG
      uart0_putc('|');
      uart0_putc( value ? '1' : '0' );
      uart0_putc('|');
      put_uint8_hex(kbd_buffer);
      uart0_putc('\n'); uart0_putc('\r');
#endif
    }

  kbd_bit_count++; // keep track of shift-in position

  /* bit 10 is the parity bit */
  if(kbd_bit_count == 10)
    {
      if( value != odd_parity(kbd_buffer) )
	{
	  kbd_buffer=0; /* parity bad - dump data */
	}
    }

  /* stop bit - always 1. this code ignores value of stop bit */
  if(kbd_bit_count == 11)
    {
      /* a complete character has been received at this point */
      
      //put_string("|"); put_uint8_hex(kbd_buffer); put_string("|\n\r");
      //put_string( kbd_shift ? "shift on\n\r" : "shift off\n\r" );
      //put_string( kbd_ctrl ? "ctrl on\n\r" : "ctrl off\n\r" );
      //put_string( kbd_alt ? "alt on\n\r" : "alt off\n\r" );
      
      switch (kbd_buffer)
	{
	case 0xF0:
	  { // key release char
	    kbd_release = TRUE;
	    kbd_extend = FALSE;
	    break;
	  }
	  
	case 0xFA: // command acknowlegde byte
	  { 
	    cmd_ack_byte_ok = TRUE;
	    break;
	  }

	case 0xE0: // extended char set
	case 0xE1:
	  { 
	    kbd_extend = TRUE;
	    break;
	  }

	case 0x12: // left shift
	case 0x59: // right shift
	  { 
	    kbd_shift = kbd_release? FALSE : TRUE;
	    kbd_release = FALSE;
	    break;
	  }

	case 0x11: // alt key (right alt is extended 0x11)
	  { 
	    kbd_alt = kbd_release? FALSE : TRUE;
	    kbd_release = FALSE;
	    break;
	  }

	case 0x14: // ctrl key (right ctrl is extended 0x14)
	  {
	    kbd_ctrl = kbd_release? FALSE : TRUE;
	    kbd_release = FALSE;
	    break;
	  }

	case 0x58: // caps lock key
	  { 
	  if (!kbd_release)
	    {
	      kbd_caps_lock = kbd_caps_lock? FALSE : TRUE;
	      
	      // allow caps lock code through to enable light on and off
	      kbd_char_buffer = kbd_buffer;
	    }
	  else
	    {
	      kbd_release = FALSE;
	    }
	  break;
	  }

	case 0x77: // num lock key
	  { 
	  if (!kbd_release)
	    {
	      kbd_num_lock = kbd_num_lock ? FALSE : TRUE;
	      
	      // allow caps lock code through to enable light on and off
	      kbd_char_buffer = kbd_buffer;
	    }
	  else
	    {
	      kbd_release = FALSE;
	    }
	  break;
	  }

	case 0x7E: // scroll lock key
	  { 
	  if (!kbd_release)
	    {
	      kbd_scroll_lock = kbd_scroll_lock ? FALSE : TRUE;
	      
	      // allow caps lock code through to enable light on and off
	      kbd_char_buffer = kbd_buffer;
	    }
	  else
	    {
	      kbd_release = FALSE;
	    }
	  break;
	  }

	default:
	  { // a real key
	    if (kbd_release) // although ignore if its just released
	      { 
		kbd_release = FALSE;
	      }
	    else
	      { // real keys go into char_buffer
		kbd_char_buffer = kbd_buffer;
	      }
	  }
	}
      
      kbd_buffer = 0;
      kbd_bit_count = 0;
    }
}


/*
 * Starts the keyboard "service" by registering the external interrupt.
 * setting the pin modes correctly and driving those needed to high.
 */
void kbd_init(void)
{
  // Prepare the global variables
  //kbd_data_pin       = dataPin;
  kbd_buffer = 0;
  kbd_char_buffer    = 0;
  kbd_bit_count     = 0;
  kbd_shift         = FALSE;
  kbd_ctrl          = FALSE;
  kbd_alt           = FALSE;
  kbd_extend        = FALSE;
  kbd_release       = FALSE;
  kbd_caps_lock     = FALSE;
  kbd_num_lock      = FALSE;
  kbd_scroll_lock   = FALSE;
  cmd_in_progress   = FALSE;
  cmd_count         = 0;
  cmd_value         = 0;
  cmd_ack_value     = 1;

  /* Set PD2 & PD3 as inputs */
  DDRD &= ~(1 << PD2);
  DDRD &= ~(1 << PD3);
  
  /* enable internal pull up resistors */
  PORTD |= (1 << PD2);
  PORTD |= (1 << PD3);
  
  EICRA = EICRA | (1 << ISC01);  /* set bits 1:0  to 10 (falling edge) */
  EICRA = EICRA & ~(1 << ISC00);

  EIMSK = EIMSK | (1 << INT0);   /* enable INT0 (D2) */
  
  sei(); /* enable interrupts */
}


/*
 * Returns true if there is a char to be read
 */
uint8_t kbd_available()
{
  return kbd_char_buffer != 0;
}



/*
 * Returns the status of the ctl/alt/shift/scrl-num-caps-lock
 * Note that shift and *lock are handled within the
 * code (and the return value from read() is already modified), but
 * being able to read them separately can be useful.
 * This routine is optional BUT MUST ONLY be read after available() has returned
 * true and BEFORE read() is called to retrieve the character. Reading it after
 * the call to read() will return unpredictable values.
 */
uint8_t kbd_read_status()
{
  return ((kbd_scroll_lock<<5) |
	  (kbd_num_lock<<4) |
	  (kbd_caps_lock<<3) |
	  (kbd_shift<<2) |
	  (kbd_alt<<1) |
	  kbd_ctrl);
}


/*
 * Returns the char last read from the keyboard. If the user has pressed two
 * keys between calls to this method, only the later one will be availble. Once
 * the char has been read, the buffer will be cleared.
 * If there is no char availble, 0 is returned.
 */
uint8_t kbd_read()
{
  uint8_t result;
  uint8_t lights = 0;
  
  // read the raw data from the keyboard
  result = kbd_char_buffer;
  
  // Use a switch for the code to character conversion.
  // This is fast and actually only uses 4 bytes per line
  switch (result)
    {
    case 0x1C: result = 'a'; break;
    case 0x32: result = 'b'; break;
    case 0x21: result = 'c'; break;
    case 0x23: result = 'd'; break;
    case 0x24: result = 'e'; break;
    case 0x2B: result = 'f'; break;
    case 0x34: result = 'g'; break;
    case 0x33: result = 'h'; break;
    case 0x43: result = 'i'; break;
    case 0x3B: result = 'j'; break;
    case 0x42: result = 'k'; break;
    case 0x4B: result = 'l'; break;
    case 0x3A: result = 'm'; break;
    case 0x31: result = 'n'; break;
    case 0x44: result = 'o'; break;
    case 0x4D: result = 'p'; break;
    case 0x15: result = 'q'; break;
    case 0x2D: result = 'r'; break;
    case 0x1B: result = 's'; break;
    case 0x2C: result = 't'; break;
    case 0x3C: result = 'u'; break;
    case 0x2A: result = 'v'; break;
    case 0x1D: result = 'w'; break;
    case 0x22: result = 'x'; break;
    case 0x35: result = 'y'; break;
    case 0x1A: result = 'z'; break;
      
      // note that caps lock only used on a-z
    case 0x41: result = kbd_shift? '<' : ','; break;
    case 0x49: result = kbd_shift? '>' : '.'; break;
    case 0x4A: result = kbd_shift? '?' : '/'; break;
    case 0x54: result = kbd_shift? '{' : '['; break;
    case 0x5B: result = kbd_shift? '}' : ']'; break;
    case 0x4E: result = kbd_shift? '_' : '-'; break;
    case 0x55: result = kbd_shift? '+' : '='; break;
    case 0x29: result = ' '; break;
      
    case 0x45: result = kbd_shift? ')' : '0'; break;
    case 0x16: result = kbd_shift? '!' : '1'; break;
    case 0x1E: result = kbd_shift? '@' : '2'; break;
    case 0x26: result = kbd_shift? '#' : '3'; break;
    case 0x25: result = kbd_shift? '$' : '4'; break;
    case 0x2E: result = kbd_shift? '%' : '5'; break;
    case 0x36: result = kbd_shift? '^' : '6'; break;
    case 0x3D: result = kbd_shift? '&' : '7'; break;
    case 0x3E: result = kbd_shift? '*' : '8'; break;
    case 0x46: result = kbd_shift? '(' : '9'; break;

    case 0x0E: result = kbd_shift? '~' : '`'; break;
    case 0x5D: result = kbd_shift? 0x7C : 0x5C; break;
    case 0x4C: result = kbd_shift? ':' : ';'; break;
    case 0x52: result = kbd_shift? 0x22 : 0x27; break;
	
    case 0x0D: result = '\t'; break;
    case 0x5A: result = 0x0D; break;
    case 0x66: result = PS2_KC_BKSP;  break;
    case 0x69: result = !kbd_num_lock ? PS2_KC_END   : '1'; break;
    case 0x6B: result = !kbd_num_lock ? PS2_KC_LEFT  : '4'; break;
    case 0x6C: result = !kbd_num_lock ? PS2_KC_HOME  : '7'; break;
    case 0x70: result = !kbd_num_lock ? PS2_KC_INS   : '0'; break;
    case 0x71: result = !kbd_num_lock ? PS2_KC_DEL   : '.'; break;
    case 0x72: result = !kbd_num_lock ? PS2_KC_DOWN  : '2'; break;
    case 0x73: result = '5'; break;
    case 0x74: result = !kbd_num_lock ? PS2_KC_RIGHT : '6'; break;
    case 0x75: result = !kbd_num_lock ? PS2_KC_UP    : '8'; break;
    case 0x76: result = PS2_KC_ESC; break;
    case 0x79: result = '+'; break;
    case 0x7A: result = !kbd_num_lock ? PS2_KC_PGDN  : '3'; break;
    case 0x7B: result = '-'; break;
    case 0x7D: result = !kbd_num_lock ? PS2_KC_PGUP  : '9'; break;
      
    case 0x7C:
      if( kbd_extend )
	{
	  result = PS2_KC_PSCR;
	}
      else
	{
	  result = '*'; 
	}
      break;
      
    case 0x05: result = PS2_KC_F1; break;
    case 0x06: result = PS2_KC_F2; break;
    case 0x04: result = PS2_KC_F3; break;
    case 0x0C: result = PS2_KC_F4; break;
    case 0x03: result = PS2_KC_F5; break;
    case 0x0B: result = PS2_KC_F6; break;
    case 0x83: result = PS2_KC_F7; break;
    case 0x0A: result = PS2_KC_F8; break;
    case 0x01: result = PS2_KC_F9; break;
    case 0x09: result = PS2_KC_F10; break;
    case 0x78: result = PS2_KC_F11; break;
    case 0x07: result = PS2_KC_F12; break;

    case 0x58:
    case 0x77:
    case 0x7E:
      // setting the keyboard lights is done here. Ideally it would be done
      // in the interrupt routine itself and the key codes associated wth
      // caps lock key presses would never be passed on as characters.
      // However it would make the interrupt routine very messy with lots
      // of extra state associated with the control of a caps_lock
      // key code causing a cmd byte to transmit, causing an ack_byte to
      // be received, then a data byte to transmit. Much easier done here.
      // The downside, however, is that the light going on or off at the
      // right time relies on the calling program to be checking for
      // characters on a regular basis. If the calling program stops
      // polling for characters at any point pressing the caps lock key
      // will not change the state of the caps lock light while polling
      // is not happening.
      if( !kbd_extend ) /* if not an exteded key... */
	{
	  if( result == 0x58 )
	    {
	      result = kbd_caps_lock ? PS2_KC_CLON : PS2_KC_CLOFF;
	    }
	  if( result == 0x77 )
	    {
	      result = kbd_num_lock ? PS2_KC_NUMLON : PS2_KC_NUMLOFF;
	    }
	  if( result == 0x7E )
	    {
	      result = kbd_scroll_lock ? PS2_KC_SCLON : PS2_KC_SCLOFF;
	    }
	  
	  lights = 0;
	  if( kbd_caps_lock ) lights |= 0x04;
	  if( kbd_num_lock ) lights |= 0x02;
	  if( kbd_scroll_lock ) lights |= 0x01;
	  kbd_set_lights(lights);
	}
      else
	{
	  if( result == 0x77 )
	    result = PS2_KC_PAUSE;
	}
      break;

      // Reset the shift counter for unexpected values, to get back into sink
      // This allows for hot plugging a keyboard in and out
    default:
      _delay_ms(500); // but wait a bit in case part way through a shift
      kbd_bit_count  = 0;
      kbd_shift      = FALSE;
      kbd_ctrl       = FALSE;
      kbd_alt        = FALSE;
      kbd_extend     = FALSE;
      kbd_release    = FALSE;
      kbd_caps_lock  = FALSE;

  } // end switch(result)


  // shift a-z chars here (less code than in the switch statement)
  if (((result>='a') && (result<='z')) &&
      ((kbd_shift && !kbd_caps_lock) ||
       (!kbd_shift && kbd_caps_lock)))
    {
      result = result + ('A'-'a');
    }

  /* take care of control characters here */
  if( kbd_ctrl )
    {
      if( (result>='a') && (result<='z') )
	{
	  result = result - 96;
	}
      else if( (result >='A') && (result<='Z') )
	{
	  result = result - 64;
	}
      else if( result == '@' )
	{
	  result = 0;
	}
    }
  
  // done with the character
  kbd_char_buffer = 0;

  return(result);
}
