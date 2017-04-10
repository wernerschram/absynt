# Scasm
Scasm is an assembler writen in scala. It currently includes an x86 assembler, with support for 8086, i386 and x64 instuctions, 
and an ARM assembler with support for A32 instuctions. Neither implements the full set of instuctions. My initial goal is to
implement a simple boot sector for both x86 and ARM. The project currently supports labeled branches, but not labeled pointers, 
which makes it hard to implement something usefull with it at the moment.

## examples
The project currently includes 2 example projects:
- [Raspberry PI boot sector](https://github.com/wernerschram/scasm/tree/master/examples/arm/bootRpi/src/main/scala/examples/assembler/arm): 
  A bootsector for the raspberry PI based on the [example on osdev.org](http://wiki.osdev.org/Raspberry_Pi_Bare_Bones). It currently lacks
  support for loading the pointer to the "Hello world" string. If you decompile the output (commandline for the gnu disassembler is 
  included as a comment at the end of Boot.scala) you can compare the code to expected output.
- An X86 boot sector: This project has mostly been scrapped during refactoring and hasn't been reimplemented yet. It currently mostly 
  serves as a scratchpad to quickly decompile the output.
