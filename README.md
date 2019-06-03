[![Build Status](https://travis-ci.org/wernerschram/scasm.svg?branch=master)](https://travis-ci.org/wernerschram/scasm)
[![codecov](https://codecov.io/gh/wernerschram/scasm/branch/master/graph/badge.svg)](https://codecov.io/gh/wernerschram/scasm)

# Absynt
Absynt is an assembler written in scala. The name stands for something like assembler b syntax, the reader is free to fill
in what the b stands for.

It currently includes an x86 assembler, with support for 8086, i386 and x64 instructions, and an ARM assembler with support
for A32 instructions. Neither implements the full set of instructions.

## examples
The project includes the following example projects:
- [Raspberry PI boot sector](examples/arm/bootRpi/src/main/scala/org/werner/absynt/arm/examples/BootRpi):
  A bootsector for the raspberry PI based on the [example on osdev.org](http://wiki.osdev.org/Raspberry_Pi_Bare_Bones) which prints "hello
  world!" to the serial console.
- [An X86 boot sector](examples/x86/bootFlag/src/main/scala/org/werner/absynt/x86/examples/bootFlag):
  This project produces a boot sector that shows the dutch flag (red, white and blue) in mode 0x13 (VGA 320x200x256 colors). 
  Because (MS-,PC-,Free)DOS allows direct interaction with hardware, and a com file has the same memory layout as a boot sector, 
  this example can also be used as an executable that can be run from DOS (or dosbox).
- [An 32bit X86 hello world elf executable](examples/x86/helloWorld32bit/src/main/scala/org/werner/absynt/x86/examples/helloWorld32):
  This project produces a 32bit x86 elf executable that uses linux int 0x80 to print hello world on stdout.
- [An 64bit X86 hello world elf executable](examples/x86/helloWorld64bit/src/main/scala/org/werner/absynt/x86/examples/helloWorld64):
  This project produces a 64bit x86 elf executable that uses linux syscall to print hello world on stdout.
