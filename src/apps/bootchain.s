; Stage 1.5 bootloader, which lives in paged RAM and just loads the true second stage into private RAM

#include "defs.s"

zp_ptr = $00
printptr = $80
videoprintptr = $82
videoprintdisable = $84

* = $200

entry:
	; Disable the ROM now
	lda #1
	sta VIA_PORTB

	; Map video memory to page 7 read and write
	ldy VIA_PORTANH
	lda #$f0 : sta $f000,y : sta $f100,y

	; Initialise video text output
	lda #$00 : sta videoprintptr
	lda #$72 : sta videoprintptr+1
	stz videoprintdisable

	jsr printimm
	.byte "Stage 1.5",13,10,0

	ldy VIA_PORTANH
	lda $8000,y
	ldx $8100,y

	bne warning
	cmp #0 : bne warning
	cpy #0 : beq nowarning

warning:
	jsr printimm
	.byte "WARNING - boot ROM didn't set PID or paging correctly!  ",0

	jsr printhex : jsr printspace
	txa : jsr printhex : jsr printspace
	tya : jsr printhex : jsr printnewline

	; Clearly reads work, so we can fix the problem by writing the current read page to the read and write
	; entries for process 0 page 0, and writing the PID as 0.  We'll still be in the wrong physical page 
	; for now.
	stx $8000 : stx $8100 : stz VIA_PORTANH

	; May need to remap video memory too in that case
	lda #$f0 : sta $f000 : sta $f100

nowarning:

	jsr privateramtest

	jsr printimm
	.byte "Loading second stage...",13,10,0

	; Disable video display output during loading
	dec videoprintdisable

bootloop:
	jsr printimm
	.byte 1,"boot002",2,0

cmdloop:
	jsr getchar
	jsr printchar

	dec : beq readaddr
	dec : beq loaddata
	dec : beq execute
	bra bootloop

loaddata:
	ldy #0
	stz 2 : stz 3 : stz 4
	clc
loaddataloop:
	jsr getchar
	sta (0),y
	tax
	adc 2 : sta 2
	adc 3 : sta 3
	txa : eor (0),y : beq ok
	sta 4
ok:
	iny
	bne loaddataloop

	lda 4 : beq ok2
	inc 2

ok2:
	lda 2 : jsr printchar
	lda 3 : jsr printchar

	inc 1
	
	bra cmdloop

execute:
	lda #3 : sta VIA_DDRB : sta VIA_PORTB
	jmp (0)


readaddr:
	; Read an address and store it at 0,1
	jsr getchar : sta 0
	jsr getchar : sta 1
	lda 0 : jsr printchar
	lda 1 : jsr printchar
	bra cmdloop


privateramtest:
.(
	jsr printimm
	.byte "Checking private RAM works... ",0

	; The upper 32K minus the 1K I/O region at $8400-$87FF is private RAM or pagetable - we can test them both.
	; So we want to test $8000-$83FF, and $8800-$FFFF.  To make things easier we will loop over all addresses
	; but skip the I/O region in the test loop.
	
	ldx #$80
	stz zp_ptr
initpageloop:
	stx zp_ptr+1

	txa : clc : adc #51 : clc

	ldy #1
initbyteloop:
	sta (zp_ptr),y : cmp (zp_ptr),y : bne initfail
	adc #13
	iny
	bne initbyteloop

	inx
	beq initcomplete
	cpx #$84 : bne initpageloop
	ldx #$88 : bra initpageloop

initfail:
	jsr printimm
	.byte "initfail ",13,10,0
	jsr printregs
	stp

initcomplete:

	ldx #$80
testpageloop:
	stx zp_ptr+1

	txa : clc : adc #51 : clc

	ldy #1
testbyteloop:
	cmp (zp_ptr),y : bne testfail
	adc #13
	iny
	bne testbyteloop

	inx
	beq testcomplete
	cpx #$84 : bne testpageloop
	bra testcomplete

testfail:
	jsr printimm
	.byte "testfail ",13,10,0
	jsr printregs
	stp
	
testcomplete:

	jsr printimm
	.byte "OK",13,10,0

	rts

printregs:
	pha
	jsr printhex : jsr printspace
	txa : jsr printhex : jsr printspace
	tya : jsr printhex : jsr printspace
	pla
	rts
.)


video_printchar:
.(
	bit videoprintdisable : bmi skip
	cmp #13 : beq cr
	cmp #10 : beq nl

	sta (videoprintptr)

	inc videoprintptr
skip:
	rts

cr:
	lda videoprintptr : and #$80 : sta videoprintptr
	lda #13
	rts

nl:
	clc
	lda videoprintptr : adc #$80 : sta videoprintptr
	lda videoprintptr+1 : adc #0 : sta videoprintptr+1
	lda #10
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


top:
#print top-entry

