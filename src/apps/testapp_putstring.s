; Test app for mtos kernel - writes a string repeatedly

#include "mtos_syscalls.s"

* = $200
entry:

loop:
	;jsr delay
	
	lda #<message
	ldy #>message
	SYSCALL_PUTSTRING

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

message:
	.byte "Hello World  ",0

