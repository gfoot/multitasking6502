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
	.word /* 04 */ syscall_exit
	.word /* 05 */ syscall_putstring
	.word /* 06 */ syscall_mapmemory

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

syscall_exit:
.(
	lda zp_prevprocess
	jsr process_kill
	jmp scheduler_run
.)

syscall_putstring:
.(
	; Y = high byte of address containing string
	; var_saveda = low byte of address containing string

	ldx #16     ; Print at most this many characters per syscall

pageloop:
	; Get PT base for LP containing target address
	tya : jsr get_prevprocess_pp_for_addr

	; Write kernel's LP1 mapping to point to the same PP
	sta PT_LP1R : sta PT_LP1W

	; Read the next character, using an LP1 pointer
	lda var_saveda : sta zp_ptr
	tya : and #$0f : ora #>LP1 : sta zp_ptr+1

byteloop:
	lda (zp_ptr)
	beq endofstring

	; Print the character
	jsr video_putchar

	; A character was printed, so advance the pointer in the Y and A (var_saveda) registers
	inc var_saveda : bne advanced
	iny
advanced:

	; Have we printed too many characters already this syscall?  If so, return with carry set so we
	; get called again
	sec
	dex
	beq return

	; Advance zp_ptr, and if it doesn't cross a page boundary, loop back and print another character
	inc zp_ptr : bne byteloop
	inc zp_ptr+1
	lda zp_ptr+1 : cmp #>LP2 : bcc byteloop   ; loop unless it has entered LP2

	; We've crossed a logical page boundary, and need to map a new page
	bra pageloop

return:
	rts

endofstring:
	clc       ; don't call again
	rts
.)

syscall_putstringx:
.(
	; Y = high byte of address containing string
	; var_saveda = low byte of address containing string

	; Before doing much, see if there's any buffer space - if not, we can't print anything right now
	ldx zp_serial_out_head
	inx
	cpx zp_serial_out_tail

	beq return  ; If there's no space, just return.  C is set, so blocking behaviour will be triggered.

	ldx #16     ; Print at most this many characters per syscall

pageloop:
	; Get PT base for LP containing target address
	tya : jsr get_prevprocess_pp_for_addr

	; Write kernel's LP1 mapping to point to the same PP
	sta PT_LP1R : sta PT_LP1W

	; Read the next character, using an LP1 pointer
	lda var_saveda : sta zp_ptr
	tya : and #$0f : ora #>LP1 : sta zp_ptr+1

byteloop:
	lda (zp_ptr)
	beq endofstring

	; Print the character
	jsr serialio_putchar    ; sets carry if buffer got full
	bcs return

	; A character was printed, so advance the pointer in the Y and A (var_saveda) registers
	inc var_saveda : bne advanced
	iny
advanced:

	; Have we printed too many characters already this syscall?  If so, return with carry set so we
	; get called again
	sec
	dex
	beq return

	; Advance zp_ptr, and if it doesn't cross a page boundary, loop back and print another character
	inc zp_ptr : bne byteloop
	inc zp_ptr+1
	lda zp_ptr+1 : cmp #>LP2 : bcc byteloop   ; loop unless it has entered LP2

	; We've crossed a logical page boundary, and need to map a new page
	bra pageloop

return:
	rts

endofstring:
	clc       ; don't call again
	rts
.)


syscall_mapmemory:
.(
	; saveda = page to map into

	; get PT base address for this page
	lda var_saveda
	lsr : ror : ror : ror
	bcc a15notset
	ora #4
a15notset:
	sec
	ror

	; get ready to set the PT mapping
	sta zp_ptr+1
	lda zp_prevprocess : sta zp_ptr

	; set write mapping
	lda #$f0 : sta (zp_ptr)

	; set read mapping
	lda zp_ptr+1 : ora #1 : sta zp_ptr+1
	lda #$f0 : sta (zp_ptr)

	; return success
	sec
	rts	
.)


get_prevprocess_pp_for_addr:
.(
	; Look up the previous process's PP mapping for an LP
	;
	; On entry, address high byte is in A
	;
	; On exit, A contains the PP number that the last active process would use for reads from that address

	and #$f0       ; A15-A12 are in bits 7-4
	bpl a15clear
	ora #$02       ; move A15 from bit 7 to bit 1
a15clear:
	ora #$81       ; set bit 7 and bit 0 (RWB)
	
	; Form address of pagetable entry for this LP in that process
	sta zp_temp+1
	lda zp_prevprocess : sta zp_temp

	; Read PP mapping
	lda (zp_temp)

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

