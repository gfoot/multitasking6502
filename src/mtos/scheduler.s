; Kernel scheduler module
;
; Manages scheduling of processes
;
; Premepting takes place through IRQ.  Stubborn processes will cause a RESET, at which point we kill them.
; System calls are made through BRK.

.(

&scheduler_init:
.(
	; Install the IRQ and RESET handlers
	lda #<resethandler : sta $fffc
	lda #>resethandler : sta $fffd
	lda #<irqhandler : sta $fffe
	lda #>irqhandler : sta $ffff

	; Initialise the process status list.  We support up to 256 processes, and track data about them in 256-byte tables.
	; The process status list stores 0 for empty slots, 1 for runnable processes.
	ldx #0
initloop:
	stz var_process_status,x
	inx
	bne initloop

	rts
.)

resethandler:
.(
	; A CPU reset means a process blocked interrupts for a long time.  We will handle the interrupt first, and then come back 
	; and kill the process.
	
	; Switch to process 0
	stz PID

	; Handle the interrupt
	jsr irqhandler2

	; Kill the process
	lda zp_prevprocess
	jsr process_kill

	; Schedule something else to run
	jmp scheduler_run
.)

irqhandler:
.(
	; This could be an IRQ or a BRK.  We can check the stack to find out which.
	; There's no need to be reentrant here, but while the active process is still selected,
	; we mustn't write to zero page or the stack.
	sta var_saveda
	pla : pha

	; Switch to process 0
	stz PID

	; Is it a BRK?
	and #$10
	bne isbrk

	; Handle the interrupt
	jsr irqhandler2
	
	; Was it preempted?  If so pick another process
	bcs preempted

resume:
	; If not, return to the same process
	lda zp_prevprocess : sta PID
	lda var_saveda
	bit ENDSUPER
	rti

isbrk:
	stx var_savedx
	jsr syscall
	ldx var_savedx
	bcc resume      ; resume current process if carry clear, otherwise fall through

preempted:
	; Save the process's context and run the scheduler.  The process's flags and return 
	; address are already on its stack.  We free up the Y register, and use it to index 
	; into the process register arrays.

	phy   ; save Y temporarily

	ldy zp_prevprocess

	lda var_saveda : sta var_process_regs_a,y  ; restore A's value and save it
	txa : sta var_process_regs_x,y             ; save X's value
	pla : sta var_process_regs_y,y             ; restore Y's value and save it
	tsx : txa : sta var_process_regs_sp,y      ; save the stack pointer too

	;jmp scheduler_run
.)
; fall through, do not move
&scheduler_run:
.(
	; Pick a runnable process, and restore its context
	ldy zp_prevprocess
	ldx #0
loop:
	iny
	lda var_process_status,y
	bne found
	inx
	bne loop

	jmp error_norunnableprocesses

found:
	jsr printimm
	.byte "scheduler: running process ",0
	tya : jsr printhex
	jsr printimm
	.byte 13,10,0

	sty zp_prevprocess : sty PID

	ldx var_process_regs_sp,y : txs
	lda var_process_regs_a,y : sta var_saveda
	ldx var_process_regs_x,y
	lda var_process_regs_y,y : tay

	; Reset the preemption timer
	lda var_t1ch : sta VIA_T1CH

	lda var_saveda
	bit ENDSUPER
	rti
.)


irqhandler2:
.(
	; lda ACIA_STAT : bpl notserial

	; Handle serial receive interrupts here

notserial:
	lda VIA_IFR : bpl notvia
	and VIA_IER ; ignore masked interrupts

	; Handle other VIA interrupts here, e.g. T2 ($20) is serial transmit interrupt
	; CB1/CB2/SR may be used for PS/2 or SD cards

	; Re-check if preempted.  Time spent in the ISR also counts against the preemption timer,
	; it's not easy to pause it during the ISR.
	bit VIA_IFR : bvs preempted

notvia:

	clc   ; no process change is needed
	rts

preempted:
	lda #$40 : sta VIA_IFR  ; clear the interrupt
	sec   ; a process change is needed
	rts
.)

&process_kill:
.(
	; Kill a process, PID is in A
	;
	; We remove it from the process_status table, and free any pages it had mapped

	jsr printimm
	.byte "scheduler: killing process ",0
	jsr printhex
	jsr printimm
	.byte 13,10,0

	tax
	stz var_process_status,x
	
	sta zp_ptr
	lda #$80 : sta zp_ptr+1
loop:
	lda (zp_ptr) : cmp #$ff : bne notmappedwrite

	jsr mm_unref

	lda #$ff : sta (zp_ptr)

notmappedwrite:

	inc zp_ptr+1
	lda (zp_ptr) : cmp #$ff : bne notmappedread

	jsr mm_unref

	lda #$ff : sta (zp_ptr)

notmappedread:

	lda zp_ptr+1 : clc : adc #$0f : sta zp_ptr+1
	bne loop

	rts
.)

.)

