; Test app for mtos kernel - counts down and then exits

#include "mtos_syscalls.s"

* = $200
entry:
	lda #9

loop:
	pha
	clc : adc #48
	SYSCALL_PUTCHAR
	pla

	jsr delay

	dec
	bne loop

	SYSCALL_EXIT
	stp

delay:
	ldx #0
	ldy #0
delayloop:
	jsr myrts
	inx : bne delayloop
	iny : bne delayloop
myrts:
	rts

