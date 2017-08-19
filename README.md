[![Build Status](https://travis-ci.org/wernerschram/scasm.svg?branch=master)](https://travis-ci.org/wernerschram/scasm)
[![Coverage Status](https://coveralls.io/repos/github/wernerschram/scasm/badge.svg)](https://coveralls.io/github/wernerschram/scasm)

# Scasm
Scasm is an assembler writen in scala. It currently includes an x86 assembler, with support for 8086, i386 and x64 instuctions, 
and an ARM assembler with support for A32 instuctions. Neither implements the full set of instuctions. My initial goal is to
implement a simple boot sector for both x86 and ARM. The project currently supports labeled branches, but not labeled pointers, 
which makes it hard to implement something usefull with it at the moment.

## examples
The project currently includes 2 example projects:
- [Raspberry PI boot sector](https://github.com/wernerschram/scasm/tree/master/examples/arm/bootRpi/src/main/scala/examples/assembler/arm): 
  A bootsector for the raspberry PI based on the [example on osdev.org](http://wiki.osdev.org/Raspberry_Pi_Bare_Bones). If you decompile 
  the output (commandline for the gnu disassembler is included as a comment at the end of Boot.scala) you can compare the code to expected 
  output.
- An X86 boot sector: this project produces a boot sector that shows the dutch flag (red, white and blue) in mode 0x13 (VGA 320x200x256 colors). 
  Because (MS-,PC0,Free)DOS allows all the hardware interaction this example uses and a com file has the same memory layout as a boot sector, 
  this example can also be used as an executable.
