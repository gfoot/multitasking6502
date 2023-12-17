; Kernel scheduler module
;
; Manages scheduling of processes
;
; Premepting takes place through IRQ.  Stubborn processes will cause a RESET, at which point we kill them.
; System calls are made through BRK.

#define SCHEDULER_DEBUG 2

scheduler_init:
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

	stz zp_runqueue_head
	stz zp_runqueue_tail

	rts
.)

resethandler:
.(
	; A CPU reset means a process blocked interrupts for a long time.  We will handle the interrupt first, and then come back 
	; and kill the process.
	
	; Switch to process 0
	stz PID

#if SCHEDULER_DEBUG > 1
	ldx #$ff : txs

	jsr printimm
	.byte "RESET during ",0
	lda zp_prevprocess
	jsr printhex
	jsr printimm
	.byte 13,10,0
#endif

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

	; Is it a BRK?  We also capture the interrupt-disable bit here.  The BNE still 
	; works because the only way we'd get here with interrupts disabled is if it was 
	; a BRK anyway
	and #$14 : bne isbrk

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

	; syscalls with interrupts disabled are, broadly-speaking, not allowed.  The problem
	; with allowing them is that the syscall enters supervisor mode which prevents the 
	; watchdog timer from killing the process if it spends too long with interrupts 
	; disabled.  So we take the harsher option here.  If there's ever a compelling reason
	; to allow processes to disable interrupts, and if there's also a compelling reason
	; why they should be allowed to make syscalls from that state, then other solutions
	; can be considered - but this is simple and efficient.
	and #4 : bne killit

	stx var_savedx
	jsr syscall
	ldx var_savedx
	bcs preempted    ; if carry was set then we should select another process to run
	bra resume       ; resume current process if carry clear

killit:
	lda zp_prevprocess
	jsr process_kill
	jmp scheduler_run

preempted:
	; Save the process's context and run the scheduler.  The process's flags and return 
	; address are already on its stack.  We free up the Y register, and use it to index 
	; into the process register arrays.

	phy   ; save Y temporarily

	lda zp_prevprocess

	; Put this process at the back of the runqueue
	ldy zp_runqueue_head
	sta var_runqueue,y
	iny : sty zp_runqueue_head

	tay

	lda var_saveda : sta var_process_regs_a,y  ; restore A's value and save it
	txa : sta var_process_regs_x,y             ; save X's value
	pla : sta var_process_regs_y,y             ; restore Y's value and save it
	tsx : txa : sta var_process_regs_sp,y      ; save the stack pointer too

	;jmp scheduler_run
.)
; fall through, do not move
scheduler_run:
.(
	; Pick a runnable process, and restore its context

again:
	ldy zp_runqueue_tail
	cpy zp_runqueue_head : beq noprocesses

	lda var_runqueue,y
	iny : sty zp_runqueue_tail

	; Check this process is runnable.  At the moment it's either runnable or dead, and dead processes
	; just get skipped and left out of the queue.
	tay : lda var_process_status,y : beq again

#if SCHEDULER_DEBUG > 2
	jsr printimm
	.byte "scheduler: running process ",0
	tya : jsr printhex
	jsr printimm
	.byte 13,10,0
#endif

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

noprocesses:
	jmp error_norunnableprocesses

.)


irqhandler2:
.(

.(
	; Check for serial receive interrupt
	; lda ACIA_STAT : bpl afterserialreceive

	; Handle serial receive interrupt here

afterserialreceive:
.)

	; If it's not a VIA interrupt then skip to the end
	lda VIA_IFR : bpl notvia

	; Apply the current mask before processing VIA interrupt sources
	and VIA_IER

	; Handle other VIA interrupts here, e.g. T2 ($20) is serial transmit interrupt
	; CB1/CB2/SR may be used for PS/2 or SD cards

.(
	; Check for serial transmit interrupt (T2 on the VIA)
	rol : rol : rol : bcc afterserialtransmit
	
	; Send a serial character, if the buffer is not empty; otherwise mask this interrupt source

	phy

	ldy zp_serial_out_tail
	cpy zp_serial_out_head
	beq empty
	
	; Increment the pointer
	iny : sty zp_serial_out_tail

	; Transfer the data byte to the serial port and restart the timer
	lda var_serial_out_buffer,y : sta ACIA_DATA
	stz VIA_T2CH

	ply

	bra afterserialtransmit

empty:
	; If the buffer is empty, disable this interrupt source without clearing the interrupt, so that
	; next time a character is printed we can reenable it and have the interrupt fire immediately.
	ldy #$20 : sty VIA_IER

	ply

afterserialtransmit:
.)

.(
	; Last, check if preempted.  Time spent in the ISR also counts against the preemption timer,
	; it's not easy to pause it during the ISR.
	bit VIA_IFR : bvc afterpreempted

	lda #$40 : sta VIA_IFR  ; clear the interrupt
	sec   ; a process change is needed
	rts
afterpreempted:
.)

notvia:

	clc   ; no process change is needed
	rts

.)

process_kill:
.(
	; Kill a process, PID is in A
	;
	; We remove it from the process_status table, and free any pages it had mapped

#if SCHEDULER_DEBUG > 1
	jsr printimm
	.byte "scheduler: killing process ",0
	jsr printhex
	jsr printimm
	.byte 13,10,0
#endif

	tax
	stz var_process_status,x
	
	sta zp_ptr
	lda #$80 : sta zp_ptr+1
loop:
	lda (zp_ptr) : cmp #$ff : beq notmappedwrite

	jsr mm_unref

	lda #$ff : sta (zp_ptr)

notmappedwrite:

	inc zp_ptr+1  ; e.g. $8000 => $8100

	lda (zp_ptr) : cmp #$ff : beq notmappedread

	jsr mm_unref

	lda #$ff : sta (zp_ptr)

notmappedread:

	lda zp_ptr+1 : clc : adc #$0f : sta zp_ptr+1  ; e.g. $8100 => $9000

	bne loop   ; stop when the address wraps to zero

	rts
.)

process_new:
.(
	; Allocate a new process.  This finds a free PID and returns it in A.

	phx

	ldx #1   ; PID 0 is not allowed
loop:
	lda var_process_status,x
	beq foundpid
	inx
	bne loop

	jsr printimm
	.byte "ERROR: process_new: no more process IDs", 0
	stp

foundpid:
	txa
	plx
	rts
.)


process_addtorunqueue:
.(
	; Add the process in A to the run queue.  We assume it's not already there.
	phx

	ldx zp_runqueue_head
	sta var_runqueue,x
	inx : stx zp_runqueue_head

	tax
	lda #1 : sta var_process_status,x

	txa
	plx
	rts
.)

