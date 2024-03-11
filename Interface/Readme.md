This interface board can be connected to the parallel port of the Elektor EC-6809 and provide :
- a PS2 keyboard interface managed by an Arduino Nano and supporting english or french layout.
  The keyboard parallel output is connected to port A of the 6522 VIA.
- a DS3231 rtc i2c port (connected to port B of the VIA pins 0 and 1).
- another 4 pins i2c port with supply voltage choice between 3,3 V or 5 V.
- a SPI port (connected to PB2, PB3, PB4 and PB5).

![Interface](https://github.com/Wawavoun/ELEKTOR_EC-6809/assets/94134401/703ab4df-3d6d-4109-94e1-23eab823f316)

BOM is simple :
- C1    100µF / 25V
- C2    100nF ceramic
- D1    1N4148
- R2    330R
others parts are just connectors...
