; Test app for mtos kernel - just loops calling brk(0)

#include "mtos_syscalls.s"

* = $200
entry:
	SYSCALL(0)
	bra entry
