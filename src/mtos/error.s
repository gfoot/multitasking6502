; Kernel error reporting
;
; Some errors are fatal, others only result in user process termination.

error_badsyscall:
.(
	ldx #$ff : txs
	jsr printimm
	.byte "ERROR: brkhandler: bad syscall number ",0
	jsr printhex
	jsr printimm
	.byte " by process ",0
	lda zp_prevprocess : jsr printhex
	jsr printimm
	.byte 13,10,0

	jsr process_kill
	jmp scheduler_run
.)

error_norunnableprocesses:
.(
	ldx #$ff : txs
	jsr printimm
	.byte "ERROR: scheduler: no runnable processes",13,10,0

	stp
.)


