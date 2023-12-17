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
	stz zp_serial_in_head
	stz zp_serial_in_tail
.)


serialio_putchar:
.(
	; Print the character in A.  If the buffer is full we're kinda screwed because interrupts are
	; probably disabled, so this needs better handling at some point.
	
	phy
	
	; Load and advance the head pointer
	ldy zp_serial_out_head
	iny

	; Is there space in the buffer?
	cpy zp_serial_out_tail
	beq full

	; Write the byte to the buffer and update the head pointer
	sta var_serial_out_buffer,y
	sty zp_serial_out_head

	; We could consider sending it straight away, but it's probably easier to just ensure the 
	; relevant interrupt source is enabled and let the interrupt fire when we return to user mode
	ldy #$a0 : sty VIA_IER

	ply

	clc
	rts

full:
	ply
	sec
	rts
.)


serialio_getchar:
.(
	; Get a character, if there is one.  The character is returned in A, with X zero; or X=$FF if no
	; characters were available
	
	phy

	ldy zp_serial_in_tail
	cpy zp_serial_in_head : beq empty

	iny : sty zp_serial_in_tail
	lda var_serial_in_buffer,Y

	clc
	ply
	rts

empty:
	sec
	ply
	rts
.)

