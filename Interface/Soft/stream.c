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

/* ---------------------------------------------------------------------------
   This module contains functions that initialize the AVRLIB C library 
   interface with the UART.
   ---------------------------------------------------------------------------
*/


/* prototypes */
int put(char, FILE *);
int get(FILE *);

/* set up stream for STDIO for use with AVRLIB */
FILE uart_str = FDEV_SETUP_STREAM(put, get, _FDEV_SETUP_RW);

/* The function passed as put shall take two arguments, the first a character 
 * to write to the device, and the second a pointer to FILE, and shall return
 * 0 if the output was successful, and a nonzero value if the character could
 * not be sent to the device.
 */
int put(char c, FILE *f)
{
  uart0_putc(c);

  return 0;
}

/* The function passed as get shall take a pointer to FILE as its single 
 * argument, and return one character from the device, passed as an int type. 
 * If an error occurs when trying to read from the device, it shall return 
 * _FDEV_ERR. If an end-of-file condition was reached while reading from the 
 * device, _FDEV_EOF shall be returned.
 */
int get(FILE *f)
{
  int c;

  c = UART_NO_DATA;
  while( c == UART_NO_DATA )
    {
      c=uart0_getc();
    }

  /*if( c == UART_NO_DATA )
    return EOF;*/

  c = c & 0x00FF; /* mask off last uart code in upper byte */

  uart0_putc(c); /* echo the character */

  return c;
}

