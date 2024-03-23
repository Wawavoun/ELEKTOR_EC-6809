/* Copyright (c) 2015-2019, Mike Smith
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * o  Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * o  Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>
#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/eeprom.h>
#include "defines.h"
#include <util/delay.h>
#include "util.h"
#include "uart.h"
#include "ioinit.h"
#include "wd.h"
#include "serialio.h"
#include "ps2.h"

#define SIGNATURE_BYTE   0x1C /* used to detect uninitialized eeprom */

/* ---------------------------------------------------------------------------
 * Global Variables
 * ------------------------------------------------------------------------- */

uint8_t strobe_type; /* 0=negative  1=positive */
uint8_t strobe_length; /* in ms */
uint8_t output_logic; /* 0=negative(inverted) 1=positive */
uint8_t prop_delay; /* propagation delay in us */
uint8_t back_mode;   /* Backspace sends 0=ctrl-h (0x08) or 1=del (0x7f) */




/*----------------------------------------------------------------------------
 * FUNCTION: write_config
 * PARAMS  : None
 * RETURNS : None
 * CALLS   : eeprom_update_byte()
 * DESCR   : Store the signature byte and tunable values into on-board
 *           EEPROM.
 * --------------------------------------------------------------------------*/
void write_config(void)
{
  eeprom_update_byte((uint8_t *)0,SIGNATURE_BYTE);
  eeprom_update_byte((uint8_t *)1,strobe_type);
  eeprom_update_byte((uint8_t *)2,strobe_length);
  eeprom_update_byte((uint8_t *)3,output_logic);
  eeprom_update_byte((uint8_t *)4,prop_delay);
  eeprom_update_byte((uint8_t *)5,back_mode);
}


/*----------------------------------------------------------------------------
 * FUNCTION: print_config
 * PARAMS  : None
 * RETURNS : None
 * CALLS   : put_string()
 * DESCR   : Dump configuration to serial terminal
 * --------------------------------------------------------------------------*/
void print_config(void)
{
  put_string("\n\r");
  put_string("(1) Strobe Type  ............. : ");
  strobe_type ? put_string("POS\n\r") : put_string("NEG\n\r");
  put_string("(2) Strobe Length ............ : ");
  put_uint8_nz(strobe_length);
  put_string("ms\n\r");
  put_string("(3) Output Logic ............. : ");
  output_logic ? put_string("POS\n\r") : put_string("NEG\n\r");
  put_string("(4) Propagation Delay ........ : ");
  put_uint8_nz(prop_delay);
  put_string("us\n\r");
  put_string("(5) Backspace sends .......... : ");
  back_mode ? put_string("DEL (0x7F)\n\r") :
    put_string("BACKSPACE (0x08)\n\r");

  put_string("\n\r");
}


/*----------------------------------------------------------------------------
 * FUNCTION: get_config
 * PARAMS  : None
 * RETURNS : None
 * CALLS   : put_string(), eeprom_read_byte(), write_config()
 * DESCR   : Read tunable configuration values from EEPROM. If a valid
 *           configuration record is not present, set values to defaults and
 *           save an initial copy to EEPROM.
 * --------------------------------------------------------------------------*/
void get_config(void)
{
  uint8_t value;
  
  /* check for signature byte in eeprom */
  value=eeprom_read_byte((uint8_t *)0);
  if( value != SIGNATURE_BYTE )
    {
      /* memory is uninitialized */
      put_string("No config in memory. Setting defaults.\n\r");
      strobe_type=1; /*positive*/
      strobe_length=5; /* 5ms */
      output_logic=1; /* positive logic */
      prop_delay=100; /* 100us */
      back_mode=0; /* send backspace */
      write_config();
    }
  else
    {
      /* memory is initialized - read values */
      strobe_type=eeprom_read_byte((uint8_t *)1);
      strobe_length=eeprom_read_byte((uint8_t *)2);
      output_logic=eeprom_read_byte((uint8_t *)3);
      prop_delay=eeprom_read_byte((uint8_t *)4);
      back_mode=eeprom_read_byte((uint8_t *)5);
    }
}


/*----------------------------------------------------------------------------
 * FUNCTION: configure
 * PARAMS  : None
 * RETURNS : None
 * CALLS   : put_string(), print_config(), uart_available(), uart_getc(),
 *           uart0_flush()
 * DESCR   : Allow end user to change system tunable values from the serial
 *           terminal.
 * --------------------------------------------------------------------------*/
void configure(void)
{
  uint8_t u;
  uint16_t val;
  
  while(1)
    {
      put_string("Configuration Menu\n\r");
      print_config();
      put_string("[1-5], [S]ave, [D]efaults, [E]xit: ");
      while(!uart_available());
      u=uart_getc();
      switch(u)
	{
	case '1':
	  put_string("1\n\rStrobe Type [P]ositive or [N]egative: ");
	  while(!uart_available());
	  u=uart_getc();
	  if(u=='p'||u=='P')
	    {
	      put_string("Positive\n\r");
	      strobe_type=1;
	    }
	  if(u=='n'||u=='N')
	    {
	      put_string("Negative\n\r");
	      strobe_type=0;
	    }
	  put_string("\n\r");
	  break;
	case '2':
	  put_string("2\n\rStrobe Length in ms (1-255): ");
	  uart0_flush(); scanf("%d",&val);
	  if(val>=1 && val<=255) strobe_length=val;
	  break;
	case '3':
	  put_string("3\n\rOutput Logic [P]ositive or [N]egative: ");
	  while(!uart_available());
	  u=uart_getc();
	  if(u=='p'||u=='P')
	    {
	      put_string("Positive\n\r");
	      output_logic=1;
	    }
	  if(u=='n'||u=='N')
	    {
	      put_string("Negative\n\r");
	      output_logic=0;
	    }
	  put_string("\n\r");
	  break;
	case '4':
	  put_string("4\n\rPropagation delay in us (0-255): ");
	  uart0_flush(); scanf("%d",&val);
	  if(val<=255) prop_delay=val;
	  put_string("\n\r\n\r");
	  break;
	case '5':
	  put_string("5\n\rBackspace sends [B]ackspace (0x08) or [D]elete (0x7f): ");
	  while(!uart_available());
	  u=uart_getc();
	  if(u=='b'||u=='B')
	    {
	      put_string("Backspace\n\r");
	      back_mode=0;
	    }
	  if(u=='d'||u=='D')
	    {
	      put_string("Delete\n\r");
	      back_mode=1;
	    }
	  put_string("\n\r");
	  break;
	case 's':
	case 'S':
	  put_string("Saving...\n\r\n\r");
	  write_config();
	  break;
	case 'd':
	case 'D':
	  put_string("Defaults\n\r");
	  strobe_type=1; /*positive*/
	  strobe_length=5; /* 5ms */
	  output_logic=1; /* positive logic */
	  prop_delay=100; /* 100us */
	  back_mode=0; /* send backspace */
	  break;
	case 'e':
	case 'E':
	  put_string("Exit\n\r");
	  return;
	  break;
	}
    }
}


/*----------------------------------------------------------------------------
 * FUNCTION: set_strobe
 * PARAMS  : None
 * RETURNS : None
 * CALLS   : setBit(), clearBit()
 * DESCR   : Set the stobe pin, following the logic sense set in strobe_type
 * --------------------------------------------------------------------------*/
void set_strobe(void)
{
  if( strobe_type )
    {
      setBit(PORTB,PB5);
    }
  else
    {
      clearBit(PORTB,PB5);
    }
}


/*----------------------------------------------------------------------------
 * FUNCTION: clear_strobe
 * PARAMS  : None
 * RETURNS : None
 * CALLS   : setBit(), clearBit()
 * DESCR   : Set the stobe pin, following the logic sense set in strobe_type
 * --------------------------------------------------------------------------*/
void clear_strobe(void)
{
  if( strobe_type )
    {
      clearBit(PORTB,PB5);
    }
  else
    {
      setBit(PORTB,PB5);
    }
}


/*----------------------------------------------------------------------------
 * FUNCTION: write_io_byte
 * PARAMS  : byte to write to 8 bit output port
 * RETURNS : None
 * CALLS   : setBit(), clearBit()
 * DESCR   : Write the value byte to the 8 bit output port. The bits are split
 *           across PC0-5,PD6,PD7
 * --------------------------------------------------------------------------*/
void write_io_byte(uint8_t byte)
{
  PORTC = PORTC & 0xC0;
  PORTC |= (byte & 0x3F);

  if(byte & 0x40)
    {
      setBit(PORTD,PD6);
    }
  else
    {
      clearBit(PORTD,PD6);
    }
  if(byte & 0x80)
    {
      setBit(PORTD,PD7);
    }
  else
    {
      clearBit(PORTD,PD7);
    }
}


/*----------------------------------------------------------------------------
 * FUNCTION: send_byte
 * PARAMS  : byte to write to 8 bit output port
 * RETURNS : None
 * CALLS   : _delay_loop_2(), write_io_byte(), set_strobe(), clear strobe()
 * DESCR   : Write byte to output port, following these steps:
 *           1. Write byte value to 8 bit port
 *           2. Wait for prop_delay uS
 *           3. Assert strobe
 *           4. Wait for strobe_length ms
 *           5. Clear strobe
 *           6. Clear output byte
 * --------------------------------------------------------------------------*/
void send_byte(uint8_t byte)
{
  uint8_t loop;
  uint16_t cycles;

  /* if inverted logic, invert bits in output */
  if( !output_logic ) byte = ~byte;

  /* set output */
  write_io_byte(byte);
  
  if( prop_delay )
    {
      /* NANO runs at 16Mhz. This means there are 16 CPU cycles per uS */
      /* so prop_delay*16 = number of needed CPU cycles */
      /* _delay_loop_2() is 4 CPU cycles per iteration */
      cycles=(prop_delay*16)/4;
      _delay_loop_2(cycles);
    }
  
  /* do strobe */
  set_strobe();
  for(loop=0;loop<strobe_length;loop++)
    _delay_ms(1);
  clear_strobe();

  /* clear output */
  if( output_logic )
    {
      write_io_byte(0x00);
    }
  else
    {
      write_io_byte(0xFF);
    }
}


/*----------------------------------------------------------------------------
 * FUNCTION: init_output
 * PARAMS  : None
 * RETURNS : None
 * CALLS   : clear_strobe(), write_io_byte()
 * DESCR   : Set D13, PC0-PC5, PD6-PD7 to outputs. Set 8 bit output port to 
 *           '0', and clear strobe
 * --------------------------------------------------------------------------*/
void init_output(void)
{
  /* set strobe (D13) to output */
  DDRB |= _BV(PB5);
  clear_strobe();
  
  /* set PC0-PC5 to output (bits 0-5) */
  DDRC |= 0x3F;
  /* set PD6,PD7 to output (bits 6-7) */
  DDRD |= 0xC0;


  if( output_logic )
    {
      write_io_byte(0x00);
    }
  else
    {
      write_io_byte(0xFF);
    }
}


/*----------------------------------------------------------------------------
 * FUNCTION: test_mode
 * PARAMS  : None
 * RETURNS : None
 * CALLS   : set_strobe, clear_strobe(), write_io_byte(), put_string(),
 *           uart_getc(), uart_available(), uart0_flush(), scanf()
 * DESCR   : Allow the end user to test output port and strobe connections
 *           under program control. This helps when wiring/testing a cable.
 * --------------------------------------------------------------------------*/
void test_mode(void)
{
  uint8_t c;
  uint16_t val;
  
  put_string("Test\n\r");
  
  c=0;
  while( !( (c=='e') || (c=='E') ) )
    {
      put_string("[S]et Strobe, [C]lear Strobe, [W]rite Value, [E]xit: ");
      while(!uart_available());
      c=uart_getc();
      switch(c)
	{
	case 's':
	case 'S':
	  put_string("Set Strobe\n\r");
	  set_strobe();
	  break;
	case 'c':
	case 'C':
	  put_string("Clear Strobe\n\r");
	  clear_strobe();
	  break;
	case 'w':
	case 'W':
	  put_string("Write Value\n\r");
	  put_string("Enter Value (does not invert!) [0-255]: ");
	  uart0_flush(); scanf("%d",&val);
	  put_string("\n\r");
	  write_io_byte( val );
	  break;
	}
    }

  put_string("\n\rReturning to normal operation...\n\r");
  
  clear_strobe();
  /* clear output */
  if( output_logic )
    {
      write_io_byte(0x00);
    }
  else
    {
      write_io_byte(0xFF);
    }  
}


/*----------------------------------------------------------------------------
 * FUNCTION: is_printable
 * PARAMS  : Byte to check for 'printability'
 * RETURNS : 1 if byte can be displayed/output, 0 if not
 * CALLS   : None
 * DESCR   : Check if byte can be meaningfully output on terminal
 * --------------------------------------------------------------------------*/
uint8_t is_printable(uint8_t byte)
{
  switch( byte )
    {
    case 13: return 1; /* return */
    case 10: return 1; /* lf */
    case 8: return 1; /* backspace */
    case 127: return 1; /* delete */
    }
  if( (byte>=' ')&&(byte<='~') )
    return 1;
  
  return 0;
}


/*----------------------------------------------------------------------------
 * FUNCTION: print_banner
 * PARAMS  : None
 * RETURNS : None
 * CALLS   : put_string()
 * DESCR   : Print instruction banner on serial console
 * --------------------------------------------------------------------------*/
void print_banner(void)
{
  put_string("Characters typed on this console will be sent to host via parallel\n\r");
  put_string("interface.\n\r\n\r");
  put_string("CTRL-D = toggle display mode    CTRL-R = PS/2 reset\n\r");
  put_string("CTRL-T = test mode              CTRL-Y = change configuration\n\r\n\r");
}


/* ---------------------------------------------------------------------------
 * Principle of Operation:
 * Input from keyboard is mapped to ASCII value. 
 * ASCII value is placed on PC0-PC5,PD6,PD7 (inverted if output logic = 0).
 * Wait prop_delay microseconds
 * Stobe goes active high (or low) for strobe_length milliseconds
 * Strobe goes inactive
 * PC0-PC5,PD6,PD7 goes back to 0 (or 0xFF if output logic = 0)
 *
 * CTRL-D toggles display of hex values for inputs
 * CTRL-T enters test mode for output port and strobe
 * CTRL-R resets the PS/2 keyboard
 * CTRL-Y changes tunable values
 * ------------------------------------------------------------------------ */
int main (void)
{
  uint8_t c,u,display_values;

  /* Initialize watchdog, set up LED port, connect AVR Libc to UART */
  io_init();

  put_string("PS/2 --> Parallel ASCII Adapter v0.01\n\r");
  put_string("Initializing PS/2 keyboard...\n\r");
  
  kbd_init();  /* initialize PS/2 keybaord */
  kbd_reset(); /* should see 3 flashes */

  put_string("Loading configuration...\n\r");
  get_config();

  print_config();

  /* initialize strobe and output bits */
  init_output();
  
  print_banner();

  display_values=0;
  while(1)
    {
      if(kbd_available())
	{
	  c=kbd_read();
	  if( c == PS2_KC_BKSP )
	    {
	      if( back_mode )
		{
		  c=0x7F;
		}
	      else
		{
		  c=0x08;
		}
	    }
	  if( is_printable(c) )
	    {
	      uart0_putc(c);
	      if( c == 0x0d ) uart0_putc(10);
	      if( display_values )
		{
		  put_string("[0x");
		  put_uint8_hex(c);
		  put_string("]");
		}
	    }
	  else
	    {
	      put_string("[0x");
	      put_uint8_hex(c);
	      put_string("]");
	    }
	  send_byte(c);
	}
      if(uart_available())
	{
	  u=uart_getc();
	  if( u==0x12 /*ctrl-r*/ )
	    {
	      put_string("\n\rHard PS/2 Reset (should flash)...\n\r");
	      kbd_reset();
	      print_banner();
	    }
	  else if( u==0x04 /*ctrl-d*/ )
	    {
	      display_values=1-display_values;
	      display_values ? put_string("<display on>") :
		put_string("<display off>");
	    }
	  else if( u==0x19 /*ctrl-y*/)
	    {
	      configure();
	      clear_strobe();
	      /* clear output */
	      if( output_logic )
		{
		  write_io_byte(0x00);
		}
	      else
		{
		  write_io_byte(0xFF);
		}
	      print_config();
	      print_banner();
	    }
	  else if( u==0x14 /*ctrl-t*/)
	    {
	      test_mode();
	      print_banner();
	    }
	  else
	    {
	      uart0_putc(u);
	      send_byte(u);
	    }
	}
    }

  return (0);
}






