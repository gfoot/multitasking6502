; Second test program that checks that the basic RAM interfaces work and then 
; soak-tests the paged RAM

.(

#include "defs.s"


printptr = $80


* = $ROMBASE
	.dsb $f400-*, 0

entry:
	ldx #$ff : txs
	sei : clv : cld : cpx #0
	txa : tay

	; Initialise the VIA

	; Port A - all outputs, initially low
	stz VIA_PORTANH
	lda #$ff : sta VIA_DDRA

	; Port B - bit 0 is an output, initially low
	stz VIA_PORTB
	lda #1 : sta VIA_DDRB

	; T2CL - set T2 to count down from 10*16
	lda #160 : sta VIA_T2CL : stz VIA_T2CH

	; ACR - enable T2 counting pulses on PB6
	lda #$20 : sta VIA_ACR

	; PCR - enable pulse output on CA2
	lda #$0a : sta VIA_PCR

	; Interrupts - disable all for now
	lda #$7f : sta VIA_IER : sta VIA_IFR


	; Initialise the ACIA
	lda #$0b : sta ACIA_CMD    ; no parity, interrupts off, RTS asserted
	lda #$1e : sta ACIA_CTRL   ; 9600 8-x-1

	; Set up some zero page and stack so that we can use subroutines
	stz VIA_PORTANH
	stz $8000
	stz $8100

	jsr printimm
	.byte "Hello world",10,13,0

	stp

&printchar:
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

nmi:
	ldx #$fe : stx VIA_PORTANH
	jsr printimm
	.byte "NMI", 0
	stp

irq:
	ldx #$fd : sta VIA_PORTANH
	jsr printimm
	.byte "IRQ", 0
	stp

top:
	.dsb $fffa-*, $00
#print top-entry
#print *-top

vectors:
	.word nmi
	.word entry
	.word irq
.)

