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

#include<avr/io.h>
#include<avr/interrupt.h>
#include "wd.h"

/* --------------------------------------------------------------------------
   Watchdog functions. These are only safe with interrupts turned off. 
   ------------------------------------------------------------------------ */


/*----------------------------------------------------------------------------
 * FUNCTION: watchdog_reset
 * PARAMS  : None
 * RETURNS : None
 * CALLS   : None
 * DESCR   : Reset the on-chip watchdog timer
 * --------------------------------------------------------------------------*/
void watchdog_reset(void) 
{
  __asm__ __volatile__ (
			"wdr\n"
			);
}


/*----------------------------------------------------------------------------
 * FUNCTION: watchdog_config
 * PARAMS  : Watchdog timeout (see wd.h for constants)
 * RETURNS : None
 * CALLS   : None
 * DESCR   : Set the on-chip watchdog timer to the timeout value X
 * --------------------------------------------------------------------------*/
void watchdog_config(uint8_t x) 
{
  WDTCSR = _BV(WDCE) | _BV(WDE);
  WDTCSR = x;
}


/*----------------------------------------------------------------------------
 * FUNCTION: watchdog_reset_chip
 * PARAMS  : None
 * RETURNS : None
 * CALLS   : watchdog_config()
 * DESCR   : Reset the chip by setting watchdog to 16ms, then doing busy loop
 * --------------------------------------------------------------------------*/

void watchdog_reset_chip(void)
{
  watchdog_config(WATCHDOG_16MS);
  while(1);
}
