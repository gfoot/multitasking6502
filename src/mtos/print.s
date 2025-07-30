; Kernel print module
;
; Routines to print things to serial output


#include "utils/print.s"


printregs:
	; Print the values of the registers (A, X, Y)
	pha
	jsr printhex : jsr printspace
	txa : jsr printhex : jsr printspace
	tya : jsr printhex : jsr printspace
	pla
	rts


printzp16:
.(
	; Print a hex dump of the first 16 bytes of zero page

	pha : phx

	ldx #0
loop:
	lda 0,x : jsr printhex : jsr printspace
	inx
	cpx #$10 : bne loop

	plx : pla
	rts
.)


.(

&printchar:
	; Print the character in A
	jsr video_putchar

&serial_putchar:
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
	; Read a character into A

	; Wait for RDR full
	lda ACIA_STAT : and #8 : beq getchar

	; Read the character
	lda ACIA_DATA

	rts

&printimm:
	; Print the null-terminated string that follows the jsr to this routine

	pha : phx : phy

	tsx
	clc
	lda $104,x : adc #1 : sta zp_printptr
	lda $105,x : adc #0 : sta zp_printptr+1

	jsr printmsgloop

	lda zp_printptr : sta $104,x
	lda zp_printptr+1 : sta $105,x

	ply : plx : pla
	rts

&printmsg:
	; Print a message pointed at by Y and X

	stx zp_printptr
	sty zp_printptr+1

printmsgloop:
	ldy #0
	lda (zp_printptr),y
	beq endprintmsgloop
	jsr printchar
	inc zp_printptr
	bne printmsgloop
	inc zp_printptr+1
	bra printmsgloop

endprintmsgloop:
	rts

.)

