# ada-chip

A CHIP-8 emulator written in Ada (as an exercise for learning Ada). Supports (*I believe*) the entire instruction set from the ["SUPER-CHIP specification from 1991 (but without the additional opcodes that provide extended functionality)"](https://en.wikipedia.org/wiki/CHIP-8#Opcode_table).

## Building

Use [Alire](https://alire.ada.dev/) to build ada-chip: `alr build`.

## Running

Just pass the CHIP-8 ROM that you would like to run as an argument: `ada_chip game.ch8`.

## Configuration

*(TODO: allow command line settings)*

In `src/ada_chip.adb`, `Steps_Per_Frame` can be configured for the number of CPU cycles to run between render updates. Different ROMs seem to benefit from faster (e.g. 16) to slower (e.g. 8) settings.

Input controls follow the CHIP-8 spec which are keys A through F and 0 through 9.

## Issues

Some ROMs do not work well, particularly with rendering; however, other emulators appear to have the same problems. Please only report an issue if you can prove that this emulator is doing something incorrectly.

## Useful links

* [kripod/chip8-roms](https://github.com/kripod/chip8-roms): ROMs to try out.
* [CHIP-8 Technical Reference](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM)
* [CHIP-8 Wikipedia entry](https://en.wikipedia.org/wiki/CHIP-8)