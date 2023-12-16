; Kernel serial filing system
;
; At the moment this just allows blocking loads of data

serialfs_load_imm:
.(
	; A = physical page number (unused at the moment)
	; Y = high byte of target address
	; X = low byte of target address
	;
	; "filename" is in memory after the jsr call
	;
	; On exit, A is unaffected and YYXX is the next free address after the loaded data

	zp_pp = zp_temp
	zp_addr = zp_temp+1
	zp_checksum = zp_temp+3

	sta zp_pp
	stx zp_addr
	sty zp_addr+1
	
	lda #1 : jsr printchar
	lda #'L' : jsr printchar
	
	tsx
	clc
	lda $101,x : adc #1 : sta zp_ptr
	lda $102,x : adc #0 : sta zp_ptr+1

loop:
	lda (zp_ptr) : beq loopend
	jsr printchar
	inc zp_ptr : bne loop
	inc zp_ptr+1 : bra loop
loopend:

	lda zp_ptr : sta $101,x
	lda zp_ptr+1 : sta $102,x

	lda #2 : jsr printchar

	; We expect a series of "2" (load) commands, then a "0" (end) command.
cmdloop:
	jsr getchar
	cmp #2 : beq loaddata
	cmp #0 : beq done
	stp

loaddata:
	; Load a block of 256 bytes.  If we need to do anything to prepare in advance, then we 
	; should do it before echoing back the load command (2) otherwise the server will start
	; sending data before we're ready.
	
	jsr printchar    ; echo back the command code (2)

	ldy #0
	stz zp_checksum : stz zp_checksum+1
	clc
loaddataloop:
	jsr getchar
	sta (zp_addr),y
	adc zp_checksum : sta zp_checksum
	adc zp_checksum+1 : sta zp_checksum+1
	iny
	bne loaddataloop

	lda zp_checksum : jsr printchar
	lda zp_checksum+1 : jsr printchar

	inc zp_addr+1
	bra cmdloop

done:
	jsr printchar    ; echo back the command code (0)

	lda zp_pp
	ldx zp_addr
	ldy zp_checksum
	rts
.)

