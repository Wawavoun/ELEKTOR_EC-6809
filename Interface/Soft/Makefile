TARGET=atmega328p
PROGTARG=m328p
LIBS=
PROJECT=nano-ps2
#CFLAGS=-ffunction-sections
CFLAGS=-g -Os -ffunction-sections -Wall -std=gnu99
LDFLAGS=-Wl,-gc-sections
OBJS=uart.o stream.o serialio.o wd.o ioinit.o ps2.o

$(PROJECT).hex: $(PROJECT).elf
	avr-objcopy -j .text -j .data -O ihex $(PROJECT).elf $(PROJECT).hex

$(PROJECT).elf: $(PROJECT).o $(OBJS)
	avr-gcc $(CFLAGS) -mmcu=$(TARGET) $(LDFLAGS) -Wl,-Map,$(PROJECT).map -o $(PROJECT).elf $(PROJECT).o $(OBJS) $(LIBS)

$(PROJECT).o: $(PROJECT).c

uart.o: uart.c uart.h

stream.o: stream.c stream.h uart.h

wd.o: wd.c wd.h

serialio.o: serialio.c serialio.h stream.h uart.h

ioinit.o: ioinit.c ioinit.h

ps2.o: ps2.c ps2.h

.c.o:
	avr-gcc $(CFLAGS) -I. -mmcu=$(TARGET) -c $<

clean:
	rm -f *~ *.o *.core $(PROJECT).o $(PROJECT).elf $(PROJECT).hex $(PROJECT).map $(OBJS)

burn:
	avrdude -c arduino -p m328p -b 57600 -P /dev/ttyUSB0 -U flash:w:$(PROJECT).hex
