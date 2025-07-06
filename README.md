# WonderSwan Karnak mapper Test V0.1.0 (20250706)

This is a Karnak mapper test program for Bandai WonderSwan (Color/Crystal) & Benesse PocketChallenge V2.

## To Do

Write twice read once.

Write once read twice.

Check writing to timer reg between adpcm writes.

## How to use

Load the ROM in an emulator or use the bootfriend (bfb) file to test on your WonderSwan.
The program will go through all the tests and then write "Ok".
You use the X1-X4 to navigate the menus, A to select an option, B to go back.

## Building

I use nasm <https://nasm.us/> by running "nasm -f bin -o KarnakTest.bfb KarnakTest.asm".

## Controls

Use WS X1-X4 to navigate the menus. A to select/continue failed test, B to go back/skip failed test.

## The Tests

If a test fails it will write out the ADPCM value written and the current index
(row) plus the previous value and index (since the lowest bit can not be read
out), also the tested PCM value and expected PCM value. Continuing the test
will fail all remaining tests since the results are dependant on previous
results.

## How it works

Bit #7 of IO address 0xD6 turns on/off ADPCM and a timer, setting the bit to 0
resets the ADPCM output to 0x80 (accumulator is 0x100) (and probably odd/even &
index). The lower 7 bits are the timer value ((val + 1) * 2), in 384kHz
(cartridge clocks). Reading 0xD6 returns the value written to 0xD6.

After an ADPCM reset the output value is 0x80
(accumulator is 0x100), the index is 0 and the top nybble is decoded first.

ADPCM values (nybbles) are written to IO address 0xD8, decoded PCM samples can
then be read from 0xD9. Every other write uses the top/bottom nybble, so you
write the same byte twice and read samples after each write. Reading IO address
0xD8 returns the last written value. Writing to 0xD9 doesn't seem to do
anything.

The actual ADPCM algorithm is the same as the NEC upd775x chips (but without
any format/headers handling). Though one thing I haven't seen documented (or it
is specific to the Karnak chip) is the saturation handling, the accumulator is
10bits with the middle 8bits output except when values are 0x200-0x2FF they are
output as 0xFF and when values are 0x300-0x3FF they are output as 0x00, the
accumulator is not saturated only the output.

## Credits

Fredrik Ahlström

Twitter @TheRealFluBBa

<https://github.com/FluBBaOfWard/KarnakTest>
