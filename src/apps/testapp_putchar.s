; Test app for mtos kernel - writes some output one character at a time

#include "mtos_syscalls.s"

* = $200
entry:

	sta 0

	clc
	adc #$f0
	sta 1

	lda 0

	sec
	
wrapdown:
	sbc #26
	bcs wrapdown

	adc #26+64

loop:
	jsr delay

	SYSCALL_PUTCHAR
	bra loop

delay:
	ldx 1
	ldy #$ff
delayloop:
	jsr myrts
	inx : bne delayloop
	iny : bne delayloop
myrts:
	rts

