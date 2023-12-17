; mtos syscall interface

#define SYSCALL(n) brk : .byte n

#define SYSCALL_NOOP SYSCALL(0)
#define SYSCALL_YIELD SYSCALL(1)
#define SYSCALL_PUTCHAR SYSCALL(2)

