; Kernel serial I/O module
;
; Just output for now
;
; We have print.s as well but that does blocking operations, and is used by kernel code.  User code needs 
; to go through buffers and interrupts.


serialio_init:
.(
	stz zp_serial_out_head
	stz zp_serial_out_tail
.)


serialio_putchar:
.(
	; Print the character in A.  If the buffer is full we're kinda screwed because interrupts are
	; probably disabled, so this needs better handling at some point.
	
	stz zp_temp    ; marker that gets bits set when the output buffer was full at some point

	phy
	
again:
	; Load and advance the head pointer
	ldy zp_serial_out_head
	iny

	; Is there space in the buffer?
	cpy zp_serial_out_tail
	beq wait

	; Write the byte to the buffer and update the head pointer
	sta var_serial_out_buffer,y
	sty zp_serial_out_head

	; We could consider sending it straight away, but it's probably easier to just ensure the 
	; relevant interrupt source is enabled and let the interrupt fire when we return to user mode
	ldy #$a0 : sty VIA_IER

	ply

	ror zp_temp     ; set the carry if the buffer was full at some point
	rts

wait:
	; The buffer is full.  Ideally we'd pause the user process until there's space - we could probably
	; reduce its return address by two, so that resuming it would replay the system call, and mark it
	; as blocked waiting for output.  Then in addition, when output space became available, we'd need 
	; to unblock processes that are waiting for it.  I can think of smart and dumb ways to do this, I
	; think dumb is probably going to be best.
	; 
	; Until that functionality exists though, we can spin processing interrupts, hoping it will send
	; a character so we can put this one in the buffer.  There are probably all sorts of deadlocks
	; possible here.

	inc zp_temp       ; mark that the buffer was full, so we can return with carry set

	pha               ; irqhandler2 corrupts A but we need its value
	jsr irqhandler2
	pla

	bra again
.)

