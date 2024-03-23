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
#include "wd.h"


/*----------------------------------------------------------------------------
 * FUNCTION: io_init
 * PARAMS  : None
 * RETURNS : None
 * CALLS   : cli(), sei(), watchdog_reset(), watchdog_config(), uart_init()
 * DESCR   : Turn off the system watchdog, set the LED pin to output, 
 *           configure the on-chip UART to SERIAL_BITRATE, then enable 
 *           interrupts. Once uart is initialized, connect STDOUT/STDIN/STDERR
 *           to UART send/receive routines.
 * --------------------------------------------------------------------------*/
void io_init( void )
{
  /* disable interrupts */
  cli();

  /* Per Atmel datasheet:
   * Note: If the Watchdog is accidentally enabled, for example by a runaway 
   * pointer or brown-out condition, the device will be reset and the Watchdog
   * Timer will stay enabled. If the code is not set up to handle the Watchdog,
   * this might lead to an eternal loop of time-out resets. To avoid this 
   * situation, the application software should always clear the Watchdog 
   * System Reset Flag (WDRF) and the WDE control bit in the initialization 
   * routine, even if the Watchdog is not in use.
   */
  /* reset and disable the watchdog timer */
  watchdog_reset();
  watchdog_config(WATCHDOG_OFF);

  /* setup up LED output pin */
  DDRB |= _BV(LED_PIN); /* set direction of B5 to output */

  /* set UART baud rate, and enable interrupts */
  uart_init(UART_BAUD_SELECT(SERIAL_BITRATE,F_CPU));
  sei();

  /* redirect STDOUT, STDIN, and STDERR to use stream we set up */
  /* see stream.c */
  stderr = stdout = stdin = &uart_str;
}
