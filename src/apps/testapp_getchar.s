; Test app for mtos kernel - reads input and echos it back with a translation

#include "mtos_syscalls.s"

* = $200
entry:

	sta 0

loop:
	SYSCALL_GETCHAR
	clc : adc 0
	SYSCALL_PUTCHAR
	bra loop

