; Video interface module
;
; The video interface provides an 80x30 character text display, written by any
; write to physical pages $F0-$F7.  The text framebuffer has a stride of 128,
; and there's no text attribute support, it's just monochrome text.
;
; There's also no hardware scrolling.
;
; The routines here provide kernel output support.  It is possible that user 
; processes could also be allowed able to directly map the framebuffer into 
; their address spaces.
;
; We track a text cursor location, and all output occurs at that location.


video_init:
.(
	pha

	; Map video memory at LPVID
	lda #$f0 : sta PT_LPVW : sta PT_LPVR
	
	jsr video_clearscreen

	stz zp_video_cursoraddr
	lda #>LPVID : sta zp_video_cursoraddr+1

	pla
	rts
.)


video_putchar:
.(
	; Put a character, interpreting some control codes to move the cursor around
	cmp #14 : bcs video_putchar_raw
	cmp #8 : bcc video_putchar_raw

	pha : phx
	sec : sbc #8
	asl : tax : jsr dojump
	plx : pla
	rts

dojump:
	jmp (jumptable,x)

jumptable:
	.word video_cursor_left  ; 8
	.word video_cursor_right ; 9
	.word video_cursor_down  ; 10
	.word video_cursor_up    ; 11
	.word video_clearscreen  ; 12
	.word video_cursor_cr    ; 13
.)


video_putchar_raw:
.(
	; Put a character, but don't interpret control codes
	sta (zp_video_cursoraddr)

	; We need a slight delay here.  Worst case timing:
	;
	; VGA clock:       /   /   /   /   /   /   /   /   /   /   /   /   /   /   /   /
	;         C:       3   4   5   6   7   0   1   2   3   4   5   6   7   0   1   2
	; Write check:            X                               X
	; Worst case WE end: ======|
	; Write op takes place:                                    ============
    ; Delay period:            \___________________________________________/
	;
	; We need to delay after a write long enough for the worst case response time of 
	; the VGA circuit, which is about 11 VGA clock cycles, or about 280ns.  At 8MHz 
	; that is 2-3 cycles.  A single nop is fine, it wastes two cycles, and then the 
	; opcode fetch for the next innstruction takes up the remaining cycle.
	nop

	; jmp video_cursor_right
.)
; fall through
video_cursor_right:
.(
	; Move the cursor right, wrapping to the next line and scrolling if necessary
	pha
	inc zp_video_cursoraddr
	lda zp_video_cursoraddr
	and #80 : cmp #80
	pla
	bcs video_cursor_newline   ; carry can only be set if we reached column 80
	rts
.)

video_cursor_cr:
.(
	pha
	lda zp_video_cursoraddr
	and #$80
	sta zp_video_cursoraddr
	pla
	rts
.)

video_cursor_newline:
.(
	jsr video_cursor_cr
	;jmp video_cursor_down
.)
; fallthrough
video_cursor_down:
.(
	pha
	lda zp_video_cursoraddr
	eor #$80
	sta zp_video_cursoraddr
	beq carry	

	pla
	rts

carry:
	lda zp_video_cursoraddr+1
	inc
	cmp #>LPVID + 30*128
	beq needscroll

	sta zp_video_cursoraddr+1
	pla
	rts

needscroll:
	; move the cursor up a row again
	lda zp_video_cursoraddr : ora #$80 : sta zp_video_cursoraddr

	pla

	; scroll the screen contents (fall through)
	;jmp video_scroll
.)
; fall through
video_scroll:
.(
	; Scroll the display up one line
	pha : phx : phy

	lda #>LPVID
	sta zp_video_ptr1+1
	sta zp_video_ptr2+1
	inc
	sta zp_video_ptr3+1

	stz zp_video_ptr1
	lda #$80 : sta zp_video_ptr2
	stz zp_video_ptr3

	ldx #14 ; 14 loops of two rows each
rowloop:
	ldy #79
colloop:
	lda (zp_video_ptr2),y : sta (zp_video_ptr1),y
	lda (zp_video_ptr3),y : sta (zp_video_ptr2),y
	dey : bpl colloop

	lda zp_video_ptr3+1 : sta zp_video_ptr2+1 : sta zp_video_ptr1+1
	inc : sta zp_video_ptr3+1
	
	dex : bne rowloop

	; Copy the 29th row from the 30th and wipe the 30th
	ldy #79
lastrowcolloop:
	lda (zp_video_ptr2),y : sta (zp_video_ptr1),y
	lda #0 : sta (zp_video_ptr2),y
	dey : bpl lastrowcolloop

	ply : plx : pla
	rts
.)

video_cursor_up:
.(
	pha
	lda zp_video_cursoraddr
	eor #$80
	sta zp_video_cursoraddr
	bne carry

	pla
	rts

carry:
	lda zp_video_cursoraddr+1
	dec
	cmp #>LPVID
	beq needscroll

	sta zp_video_cursoraddr+1
	pla
	rts

needscroll:
	; Backwards scrolling isn't supported yet, so just prevent the movement

	lda zp_video_cursoraddr
	eor #$80
	sta zp_video_cursoraddr
	pla
	rts
.)

video_cursor_left:
.(
	pha
	lda zp_video_cursoraddr
	sec : sbc #1
	bvs wrapped

	sta zp_video_cursoraddr
	pla
	rts

wrapped:
	and #$80 : ora #79
	sta zp_video_cursoraddr

	bmi wrapped2         ; need decrement of high byte as well

	pla
	rts

wrapped2:
	lda zp_video_cursoraddr+1
	dec
	cmp #>LPVID
	bcc needscroll

	sta zp_video_cursoraddr+1
	pla
	rts

needscroll:
	; Backwards scrolling isn't supported, just block the motion
	lda #79 : sta zp_video_cursoraddr
	pla
	rts
.)


video_clearscreen:
.(
	pha : phx : phy

	lda #>LPVID
	sta zp_video_ptr1+1
	sta zp_video_ptr2+1

	stz zp_video_ptr1
	lda #$80 : sta zp_video_ptr2

	lda #0

	ldx #15 ; 15 times 2 rows
rowloop:
	ldy #79
colloop:
	sta (zp_video_ptr1),y
	sta (zp_video_ptr2),y
	dey : bpl colloop

	inc zp_video_ptr1+1
	inc zp_video_ptr2+1
	dex : bne rowloop

	ply : plx : pla
	rts
.)


