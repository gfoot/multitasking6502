; Quick test program for multitasking computer
;
; Only requires ROM, VIA, ACIA - no RAM

.(

VIA_BASE = $8600

#include "utils/via.s"


ACIA_BASE = $8500

ACIA_DATA = ACIA_BASE
ACIA_STAT = ACIA_BASE+1
ACIA_CMD = ACIA_BASE+2
ACIA_CTRL = ACIA_BASE+3


* = $ROMBASE
	.dsb $f400-*, 0

entry:
	ldx #$ff : txs
	sei : clv : cld : cpx #0
	txa : tay

	; Initialise the VIA

	; Port A - all outputs, initially $55
	lda #$55 : sta VIA_PORTANH
	lda #$ff : sta VIA_DDRA

	; Port B - bit 0 is an output, initially low
	stz VIA_PORTB
	lda #1 : sta VIA_DDRB

	; T2CL - set T2 to count down from 10
	lda #160 : sta VIA_T2CL

	; ACR - enable T2 counting pulses on PB6
	lda #$20 : sta VIA_ACR

	; PCR - enable pulse output on CA2
	lda #$0a : sta VIA_PCR

	; Interrupts - disable all for now
	lda #$7f : sta VIA_IER : sta VIA_IFR


	; Initialise the ACIA
	lda #$0b : sta ACIA_CMD    ; no parity, interrupts off, RTS asserted
	lda #$1e : sta ACIA_CTRL   ; 9600 8-x-1


	; Count up on VIA port A
	ldx #$ff : stx VIA_DDRA
loop:
	inx
	bmi loopend
	stx VIA_PORTANH
	lda ACIA_STAT
	and #8
	beq nodata
	lda ACIA_DATA
	eor #32
	sta ACIA_DATA
	bra yesdata
nodata:
	stx ACIA_DATA
yesdata:
	lda #0 : tay
delay:
	iny : bne delay
	inc : bne delay
	bra loop

loopend:
	; Disable ROM
	stx VIA_PORTANH          ; no handshake, stay in super mode
	lda #1 : sta VIA_PORTB   ; disable ROM
	nop
	nop

	; Trigger a switch to user mode, for test purposes
	stx VIA_PORTA       ; write with handshake to queue up the user mode switch
	rti                 ; the switch should take place just after the opcode fetch

nmi:
	ldx #$fe : stx VIA_PORTANH
	stp

irq:
	ldx #$fd : sta VIA_PORTANH
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

