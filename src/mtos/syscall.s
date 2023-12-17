; Kernel syscall handling
;
; Syscalls are numbered starting with zero, and are indexed into a jump table.
;
; On entry to a syscall handler:
;
;   PID = 0;              caller's PID is in zp_prevprocess
;   A = 2*syscall number; caller's A is in var_saveda
;   X = 2*syscall number
;   Y is the caller's
;   C = high bit of syscall number, not used for dispatch
;
; On exit:
;
;   var_saveda, X, and Y may be used to return data
;   Carry set means the syscall was blocked, e.g. would wait for I/O or buffer space
;   A can be clobbered

.(

syscalljumptable:
	.word /* 00 */ syscall_noop
	.word /* 01 */ syscall_yield
	.word /* 02 */ syscall_putchar
	.word /* 03 */ syscall_getchar

syscalljumptablesize = *-syscalljumptable


syscall_noop:
.(
	clc
	rts
.)

syscall_yield:
.(
	sec
	rts
.)

syscall_putchar:
.(
	lda var_saveda
	jsr serialio_putchar    ; sets carry if buffer got full

	; The carry is already set if blocked, we can just return in that state
	rts
.)

syscall_getchar:
.(
	jsr serialio_getchar    ; sets carry if buffer was empty
	sta var_saveda          ; return the character (if any) to the user process in A

	; The carry is already set if blocked, we can just return in that state
	rts
.)


&syscall:
.(
	; On entry:
	;
	;   PID = 0;         caller's PID is in zp_prevprocess
	;   A is undefined;  caller's A is in var_saveda
	;   X is the syscall number
	;   Y is caller's
	;   SP is two lower than caller's (our return address was pushed)

	; Ideally here we'd re-enable interrupts as soon as we can, as servicing a system call is not high priority.
	; But that's an improvement for later.
	 
	; The system call number was passed in X - double it, bounds-check it, and dispatch the call
	txa : asl : tax
	cpx #syscalljumptablesize
	bcs badsyscall

	jmp (syscalljumptable,x)

badsyscall:
	lsr
	jmp error_badsyscall
.)

.)

