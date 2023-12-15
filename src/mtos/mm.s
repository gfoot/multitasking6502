; Kernel memory manager
;
; This tracks the degree of use of each page of physical memory (PP).  Pages with refcount of zero are
; free for use by new allocations.


mm_init:
.(
	; Set all page reference counts to zero
	ldx #0
loop:
	stz var_pagerefcounts_lo,x
	stz var_pagerefcounts_hi,x
	inx
	bne loop

	; Mark page 0 as referenced, as the kernel is using it
	inc var_pagerefcounts_lo,x

	; Mark pages beyond the installed RAM as unavailable
	ldx zp_lastphyspage
	lda #$ff
loop2:
	inx
	beq loop2done
	sta var_pagerefcounts_hi,x
	bra loop2
loop2done:

	; Unmap all pages except those for PID 0
	ldx #$80                  ; X = LP reference ($80, $81, $82, $83, $90, $91, ...)
	stz zp_ptr
unmappagesloop:
	stx zp_ptr+1
	ldy #1                    ; Y = PID, start with 1
	lda #$ff                  ; FF = not mapped
unmappagesinnerloop:
	sta (zp_ptr),y
	iny
	bne unmappagesinnerloop
	inx                       ; $80 => $81 => $82 => ...

	txa
	and #$04                  ; check for $x4
	beq unmappagesloop

	txa
	clc
	adc #$0c                  ; increment to $90, $a0, etc
	tax
	bcc unmappagesloop        ; stop when we wrap

	rts
.)


mm_alloc:
.(
	; Find a spare page.  Returns the page in A with carry clear, or carry set with A undefined if no pages were available.

	phx
	ldx #0
loop:
	lda var_pagerefcounts_lo,x
	ora var_pagerefcounts_hi,x
	beq found

	inx
	bne loop

	sec
	plx
	rts

found:
	txa

	jsr mm_clearpage

	clc
	plx
	rts
.)

mm_ref:
.(
	; Add a reference to a page - this should happen every time a process maps the page
	;
	; A = PP number

	phx
	tax

	inc var_pagerefcounts_lo,x
	bne return

	inc var_pagerefcounts_hi,x

return:
	txa
	plx
	rts
.)

mm_unref:
.(
	; Remove a reference from a page - e.g. when a process unmaps a page or is killed
	; 
	; A = PP number

	phx
	tax

	dec var_pagerefcounts_lo,x
	beq maybezero
	bpl maybecarry

return:
	plx
	txa
	rts

maybecarry:
	lda var_pagerefcounts_lo,x
	cmp #$ff
	bne return

	dec var_pagerefcounts_hi,x
	bra return

maybezero:
	lda var_pagerefcounts_hi,x
	bne return

	; The page is no longer referenced.  If any special action needs to be taken, it can be done here.
	; But we clear pages when they're allocated, so this is probably fine.
	bra return
.)


mm_clearpage:
.(
	; Clear a physical page to all zeros
	
	; Use LP3 for this
	sta PT_LP3W

	pha : phx : phy

	stz zp_temp
	ldx #>LP3

	ldy #0
	tya
pageloop:
	stx zp_temp+1
byteloop:
	sta (zp_temp),y
	iny
	bne byteloop
	inx
	cpx #>LP4
	bne pageloop

	ply : plx : pla
	rts
.)

