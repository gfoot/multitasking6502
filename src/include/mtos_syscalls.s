; mtos syscall interface

; Blocking syscall
;
; If the syscall blocks, it returns with N set, and this client code repeats the syscall until it succeeds
#define SYSCALL(n) \
	.(            :\
	again:        :\
		ldx #n    :\
		brk : brk :\
		bmi again :\
	.)

; Nonblocking syscall
;
; For syscalls that never block, this is slightly more efficient client code.
;
; For other syscalls, this is also useful if the client wants to handle the blocking itself.
#define SYSCALLNB(n) \
	ldx #n          :\
	brk : brk

#define SYSCALL_NOOP SYSCALLNB(0)
#define SYSCALL_YIELD SYSCALLNB(1)
#define SYSCALL_PUTCHAR SYSCALL(2)
#define SYSCALL_GETCHAR SYSCALL(3)

