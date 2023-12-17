; Kernel umbrella file - includes the other parts of the kernel, and decides where they go in the address space available
;
; The 32K address space from $8000 upwards is shared with I/O and the pagetable.  I/O is from $8400-$87FF.  The pagetable
; occupies the first 1K of each 4K block, i.e. $8000-$8400, $9000-$9400, etc.
;
; During initialisation we can use pagetable addresses, but after the system is up they're needed for managing processes'
; memory paging.
;
; We could start earlier than this, at least for initialisation code that's not needed after system startup is complete, 
; but $8800 is the lowest address we can use in the long term.
;
; Free regions are:
;    $8800-$8FFF
;    $9400-$9FFF
;    $A400-$AFFF
;    etc
;
; We can pick and choose what we use these for, and if we run out we can also use paged RAM for code or data other than
; the interrupt handlers.
;
; For now the kernel image begins at $8800, but this could be reduced as low as $8200 for startup code.
;
; $f400 upwards is mostly used for variable storage - kernel state, process arrays, etc


* = $8800

#include "defs.s"

#include "mtos/defs.s"

#include "init.s"
#include "print.s"
#include "scheduler.s"
#include "syscall.s"
#include "error.s"
#include "mm.s"

#if *>$9000
#error overflow
#endif
segment8freebytes = $9000-*
#print segment8freebytes


.dsb $9400-*,0

#include "serialfs.s"
#include "serialio.s"

#if *>$a000
#error overflow
#endif
segment9freebytes = $a000-*
#print segment9freebytes

