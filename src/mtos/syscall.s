; Kernel syscall handling
;
; Syscalls are numbered starting with zero, and are indexed into a jump table.
;
; On entry to a syscall handler:
;
;   PID = 0;              caller's PID is in zp_prevprocess
;   A = syscall number;   caller's A is in var_saveda
;   X is undefined;       caller's X is is var_savedx
;   Y is the caller's
;   zp_ptr points to the BRK instruction's argument, via LP1
;
; LP1 is mapped for reads to support zp_ptr.  Nearby reads that cross a page
; boundary will require a new mapping.  Writes are not mapped automatically.
;
; On exit:
;
;   var_saveda, var_savedx, and Y may be used to return data
;   A and X can be clobbered

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

	bcs blocked             ; try again later

	rts
.)

syscall_getchar:
.(
	jsr serialio_getchar    ; sets carry if buffer was empty

	bcs blocked             ; try again later

	sta var_saveda          ; need to move this so that it gets returned to the calling process properly
	rts
.)


blocked:
.(
	; Blocked syscalls rewind the user process so that the syscall is repeated next time the process is scheduled.
	; The process is put to the back of the run queue as well.  In future these processes should be removed from 
	; the run queue, and added back when the I/O is in a suitable state.

	; The mechanism here is like in 'syscall' itself, but we go to a different depth as there's an extra
	; return address on the stack now.

	ldx zp_prevprocess
	lda PT_LP0W,x               ; read process's LP 0 write mapping

	sta PT_LP1R : sta PT_LP1W   ; set our LP 1 read and write mappings to the same page

	; Here we use "inx" so that it wraps properly and we avoid issues with SP > $FC
	tsx
	inx : inx : inx : inx     ; SP => X and advance it to point at the PCL from the interrupt frame

	; Subtract two from the PC
	sec : lda LP1 + $100,x : sbc #2 : sta LP1 + $100,x
	inx : lda LP1 + $100,x : sbc #0 : sta LP1 + $100,x

	; Ensure the carry is set so the process gets preempted
	sec
	rts
.)


&syscall:
.(
	; On entry:
	;
	;   PID = 0;         caller's PID is in zp_prevprocess
	;   A is undefined;  caller's A is in var_saveda
	;   X is caller's, and has been saved to var_savedx
	;   Y is caller's
	;   SP is two lower than caller's (our return address was pushed)

	; Ideally here we'd re-enable interrupts as soon as we can, as servicing a system call is not high priority.
	; But that's an improvement for later.
	 
	; We need to determine the type of system call and act accordingly.  At this stage 
	; the A register was saved in var_saveda, and the user process is still selected.
	; We'll select process 0 so we can use our own stack, and map the right PP into LP 1
	; to read the user stack from there.  Then we'll have a user pointer to the BRK instruction,
	; and may need to map another page to access it.

	ldx zp_prevprocess
	lda PT_LP0W,x               ; read process's LP 0 write mapping

	sta PT_LP1R                 ; set our LP 1 read mapping to the same page

	; Here we use "inx" so that it wraps properly and we avoid issues with SP > $FC
	tsx
	inx : inx : inx : inx     ; SP => X and advance it to point at the PCL from the interrupt frame

	; Copy the PC to zp_ptr, subtracting 1 as we go
	sec : lda LP1 + $100,x : sbc #1 : sta zp_ptr
	inx : lda LP1 + $100,x : sbc #0

	; If zp_ptr is in a different LP, we need to map that one now
	cmp #$10 : bcc ismapped

	; The right page is not mapped, map it now

	tax                       ; save the high byte for now

	and #$f0                  ; extract LP number
	bpl a15clear : ora #2     ; set bit 1 if bit 7 is set (addr needs to be 1,A14,A13,A12,0,0,A15,RWB,PID7,PID6,...)
a15clear:
	ora #$80                  ; set bit 7
	sta zp_ptr2+1
	lda zp_prevprocess
	sta zp_ptr2

	lda (zp_ptr2)             ; Read the process's write mapping for the LP that zp_ptr is in
	sta PT_LP1R               ; Apply it to our LP 1 (read)

	txa                       ; Restore zp_ptr's high byte

ismapped:
	; The right page is mapped now - set zp_ptr's LP portion to 1
	and #$0f : ora #$10 : sta zp_ptr+1

	; Read the value after BRK on the stack
	lda (zp_ptr)

	; Double it and jump via the jump table
	asl
	tax
	cpx #syscalljumptablesize    ; but not if it's too large
	bcs badsyscall

	jmp (syscalljumptable,x)

badsyscall:
	lsr
	jmp error_badsyscall
.)

.)

