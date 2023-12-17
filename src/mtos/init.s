; Kernel init module
;
; Initialisation routines

.(
zp_physpage = zp_temp


init:
.(
	jsr printimm
	.byte 13,10,"mtos kernel starting",13,10,0

	; The system has already been bootstrapped to some extent, but we'll make sure some things are just the way we want them.

	sei : cld : ldx #$ff : txs

	; ROM is no longer mapped, and LP 0 should be mapped to some RAM, but let's do that again to make sure.

	stz VIA_PORTANH   ; set PID=0
	stz PT_LP0W       ; map PID 0 LP0 writes to PP0
	stz PT_LP0R       ; map PID 0 LP0 reads to PP0

	; Change some VIA settings
	lda #$60 : sta VIA_ACR       ; Continuous interrupts from T1, T2 counting pulses on PB6
	lda #$c0 : sta VIA_IER       ; Enable T1 interrupts for preemptive multitasking

	stz VIA_T1CL
	lda #>1024     ;  preemption timer
	sta var_t1ch
	sta VIA_T1CH

	jsr measurepagedram
	jsr testpagedram
	jsr clearpagedram

	jsr serialio_init

	jsr mm_init
	jsr scheduler_init

	jsr debugspawnprocess
	jsr debugspawnprocess
	jsr debugspawnprocess
	jsr debugspawnprocess

	jmp scheduler_run
.)


measurepagedram:
.(
	; PP 0 is mapped to LP 0.
	;
	; Find the first PP, above 0, which can be written and read without affecting PP 0

	ldx #$ff : stx $fff    ; marker

	ldx #1
loop:
	stx PT_LP1W : stx PT_LP1R   ; Select this PP for reading and writing by LP 1

	; Store something nonzero in the page, and read it back to check the page has memory backing it
	stx $1fff : cpx $1fff : bne nomemory

	; Store zero in the page, and check it stuck
	stz $1fff : ldy $1fff : bne nomemory

	; Read back the marker from PP 0, to check whether the physical memory has wrapped
	ldy $fff : beq wrapped

	inx
	bne loop

nomemory:
wrapped:
	; Whatever X is, we have that many pages of 4K each in the system
	dex : stx zp_lastphyspage

	; Add 1 and carry
	clc
	txa : adc #1 : sta zp_ptr
	lda #0 : rol

	; Calculate the amount in K
	asl zp_ptr : rol
	asl zp_ptr : rol
	sta zp_ptr+1

	; Print it in decimal
	ldx #<zp_ptr : jsr printdecu16

	jsr printimm
	.byte "K found",13,10,0
	
	rts
.)


testpagedram:
.(
	jsr printimm
	.byte "Testing paged RAM... ",0

	; Logical page 0 is used for zero page and stack, so we won't change that - it is also using 
	; PP 0 which we will also not touch.
	;
	; For the rest, we want to fill the entire memory space with a distinctive pattern, different 
	; in every PP, and then go back and check that the pattern still exists in every PP.
	;
	; We will use LP 1 throughout.
	
	jsr printimm
	.byte "init",0

	ldx #1

initpageloop:
	stx zp_physpage
	stx PT_LP1W : stx PT_LP1R   ; select this PP for reading and writing by LP 1

	stz zp_ptr
	ldx #$10

initsubpageloop:
	stx zp_ptr+1

	txa
	clc : adc #51 : adc zp_physpage

	ldy #0
initbyteloop:
	sta (zp_ptr),y : cmp (zp_ptr),y : bne initfail
	adc #13
	iny
	bne initbyteloop

	inx
	cpx #$20 : bne initsubpageloop

	ldx zp_physpage
	cpx zp_lastphyspage
	beq test

	inx
	bra initpageloop

initfail:
	jsr printimm
	.byte " initfail ",13,10,0
	jsr printregs
	jsr printzp16
	stp

test:
	jsr printimm
	.byte ", test",0

	ldx #1

testpageloop:
	stx zp_physpage
	stx PT_LP1W : stx PT_LP1R   ; select this PP for reading and writing by LP 1

	stz zp_ptr
	ldx #$10

testsubpageloop:
	stx zp_ptr+1

	txa
	clc : adc #51 : adc zp_physpage

	ldy #0
testbyteloop:
	cmp (zp_ptr),y : bne testfail
	adc #13
	iny
	bne testbyteloop

	inx
	cpx #$20 : bne testsubpageloop

	ldx zp_physpage
	cpx zp_lastphyspage
	beq testcomplete

	inx
	bra testpageloop

testfail:
	jsr printimm
	.byte " testfail ",13,10,0
	jsr printregs
	jsr printzp16
	stp

testcomplete:

	jsr printimm
	.byte ", OK",13,10,0

	rts
.)


clearpagedram:
.(
	jsr printimm
	.byte "Clearing paged RAM... ",0

	ldx #0
	ldy #2 ; start at $200 to avoid overwriting page zero or the stack

initpageloop:
	stx zp_physpage
	stx PT_LP1W        ; select this PP for writing by LP 1

	stz zp_ptr

	tya                ; first subpage to clear
	clc : adc #$10     ; add offset to LP 1
	tax

initsubpageloop:
	stx zp_ptr+1

	ldy #0
	tya
initbyteloop:
	sta (zp_ptr),y
	iny
	bne initbyteloop

	inx
	cpx #$20 : bne initsubpageloop

	ldx zp_physpage
	cpx zp_lastphyspage
	beq done
	inx
	bra initpageloop

done:
	jsr printimm
	.byte "OK",13,10,0

	rts
.)


debugspawnprocess:
.(
	; Spawn a process for testing
	;
	; We need to allocate some memory, write some code into it, set up a return stack frame, and add it to the process list

	; Allocate a page of memory
	jsr mm_alloc
	bcc allocok

	jsr printimm
	.byte "ERROR: debugspawnprocess: allocation failed", 0
	stp

allocok:
	jsr printimm
	.byte "debugspawnprocess: got page ",0
	jsr printhex
	jsr printimm
	.byte 13,10,0

	tay    ; PP allocation in Y

	; Allocate a process ID
	jsr process_new

	jsr printimm
	.byte "debugspawnprocess: got PID ",0
	jsr printhex
	jsr printimm
	.byte 13,10,0

	tax    ; PID	
	tya    ; PP

	sta PT_LP0W,x : sta PT_LP0R,x  ; set process up to read and write this page
	jsr mm_ref : jsr mm_ref        ; reference the page twice

	sta PT_LP1W : sta PT_LP1R      ; Let us also access the page via LP1

	; Load code into the page
	phx

	tya                            ; A = PP number (not used at the moment)
	ldx #$00                       ; X = low byte of target address
	ldy #>LP1 + $200               ; Y = high byte of target address, ours is in LP1

	jsr serialfs_load_imm
	.byte "testapp_putchar", 0     ; filename of code to load

	plx
	
	; Write a stack frame into the page
	lda #$02 : sta LP1 + $1ff
	lda #$00 : sta LP1 + $1fe
	php : pla : and #$fb : sta LP1 + $1fd
	
	; Set up the initial registers
	txa
	sta var_process_regs_a,x
	sta var_process_regs_x,x
	sta var_process_regs_y,x

	lda #$fc
	sta var_process_regs_sp,x

	; Set the process as runnable
	txa
	jsr process_addtorunqueue

	rts
.)

.)

