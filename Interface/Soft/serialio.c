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
#include "defines.h"
#include <util/delay.h>
#include "uart.h"
#include "stream.h"
#include "serialio.h"


/* ---------------------------------------------------------------------------
   This module contains functions to output to or input from the UART, without
   using the AVRLIB C library.
   ---------------------------------------------------------------------------
*/


void put_string(char *str)
{
  while( *str != 0 )
    {
      uart0_putc(*str++);
    }
}

void put_integer(uint32_t value, uint32_t divisor)
{
  uint8_t place_value;

  while( divisor != 0 )
    {
      place_value = value / divisor;
      uart0_putc( '0' + place_value );
      value = value - (place_value * divisor);
      divisor = divisor / 10;
    }
}

void put_integer_nz(uint32_t value, uint32_t divisor)
{
  uint8_t place_value = 0;

  while( (divisor != 0) && (place_value == 0) )
    {
      place_value = value / divisor;
      divisor = divisor / 10;
    }

  if( divisor == 0 )
    {
      uart0_putc( '0' + place_value );
    }
  else
    {
      divisor*=10;
      put_integer(value,divisor);
    }
}

void put_uint8(uint8_t byte)
{
  put_integer((uint32_t)byte, 100);
}

void put_uint8_nz(uint8_t byte)
{
  put_integer_nz((uint32_t)byte, 100);
}

void put_uint16(uint16_t number)
{
  put_integer((uint32_t)number,10000);
}

void put_uint16_nz(uint16_t number)
{
  put_integer_nz((uint32_t)number,10000);
}

void put_uint32(uint32_t number)
{
  put_integer((uint32_t)number,1000000000);
}

void put_uint32_nz(uint32_t number)
{
  put_integer_nz((uint32_t)number,1000000000);
}

char nibble[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 
		  'B', 'C', 'D', 'E', 'F' };

void put_uint8_hex(uint8_t byte)
{
  char n1,n2;

  n2=byte & 0x0F;
  n1=(byte >> 4) & 0x0F;

  uart0_putc( nibble[ (int)n1 ] );
  uart0_putc( nibble[ (int)n2 ] );
}

void put_uint32_hex(uint32_t word)
{
  char byte1,byte2,byte3,byte4;

  byte4=word & 0x000000FF;
  byte3=(word >> 8) & 0x000000FF;
  byte2=(word >> 16) & 0x000000FF;
  byte1=(word >> 24) & 0x000000FF;

  put_uint8_hex( byte1 );
  put_uint8_hex( byte2 );
  put_uint8_hex( byte3 );
  put_uint8_hex( byte4 );
}

