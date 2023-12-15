; Small test case as second stage for bootloader

#include "../labels/boot.inc"

* = $200
entry:
	jsr printimm
	.byte "Hello world",10,13,0

loop:
	jsr printimm
	.byte "All work and no play  ",0
	bra loop

