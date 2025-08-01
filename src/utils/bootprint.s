; Routines for printing message during bootup
;
; This is used both from ROM and from paged RAM


printinit:
	; Initialise video text output
	lda #$80 : sta videoprintptr
	lda #$7e : sta videoprintptr+1
	stz videoprintdisable
	rts

video_printchar_raw:
.(
	pha
	sta (videoprintptr)
	inc videoprintptr
	lda videoprintptr
	cmp #80
	beq scroll
	cmp #80+$80
	beq scroll
	pla
	rts
scroll:
	pla
	jmp video_scroll
.)

video_printchar:
.(
	bit videoprintdisable : bmi skip
	cmp #13 : beq cr
	cmp #10 : beq video_scroll

	sta (videoprintptr)

	inc videoprintptr
skip:
	rts

cr:
	lda #$80 : sta videoprintptr
	lda #13
	rts
.)

video_scroll:
.(
	phy

	lda #$70 : sta videoprintptr+1 : sta videoprintptr2+1
	stz videoprintptr : lda #$80 : sta videoprintptr2

scrollloop2:
	ldy #79
scrollloop:
	lda (videoprintptr2),y : sta (videoprintptr),y
	dey
	bpl scrollloop

	lda videoprintptr : beq even

	stz videoprintptr : inc videoprintptr+1 : sta videoprintptr2

	bra scrollloop2

even:
	stz videoprintptr2 : inc videoprintptr2+1 : lda #$80 : sta videoprintptr

	lda videoprintptr2+1 : cmp #$7f : bne scrollloop2

	ldy #79 : lda #0
clearlineloop:
	sta $7e80,y
	dey
	bpl clearlineloop

	lda #10

	ply
	rts
.)


&printchar:
	jsr video_printchar

&serial_printchar:
.(
	pha

	; wait for TDR empty, by waiting for T2 to expire
	lda #$20
wait:
	bit VIA_IFR : beq wait

	; Write the character and restart T2
	pla
	sta ACIA_DATA
	stz VIA_T2CH

	rts
.)


&getchar:
	; Wait for RDR full
	lda ACIA_STAT : and #8 : beq getchar

	; Read the character
	lda ACIA_DATA

	rts

&printimm:
	pha : phx : phy

	tsx
	clc
	lda $104,x : adc #1 : sta printptr
	lda $105,x : adc #0 : sta printptr+1

	jsr printmsgloop

	lda printptr : sta $104,x
	lda printptr+1 : sta $105,x

	ply : plx : pla
	rts

&printmsg:
	stx printptr
	sty printptr+1

printmsgloop:
	ldy #0
	lda (printptr),y
	beq endprintmsgloop
	jsr printchar
	inc printptr
	bne printmsgloop
	inc printptr+1
	bra printmsgloop

endprintmsgloop:
	rts

&printhex:
	pha
	ror : ror : ror : ror
	jsr printnybble
	pla
&printnybble:
.(
	pha
	and #15
	cmp #10
	bmi skipletter
	adc #6
skipletter:
	adc #48
	jsr printchar
	pla
	rts
.)

&printspace:
	pha
	lda #' '
	jsr printchar
	pla
	rts

&printnewline:
	jsr printimm
	.byte 13,10,0
	rts


