; Multitasking system boot ROM
; 
; Checks that RAM is working, then loads a second stage from serial link to a PC

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

	; Port A - all outputs, initially 0 (PID)
	lda #$00 : sta VIA_PORTANH
	lda #$ff : sta VIA_DDRA

	; Port B - bit 0 is an output, initially low (inverted ROMEN)
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


	; Map video memory to page 7 read and write
	ldy VIA_PORTANH
	lda #$f0 : sta $f000,y : sta $f100,y

	; Wipe all video memory
	ldy #0 : tya
clearscreenloop:
	sta $7000,y : sta $7100,y : sta $7200,y : sta $7300,y
	sta $7400,y : sta $7500,y : sta $7600,y : sta $7700,y
	sta $7800,y : sta $7900,y : sta $7a00,y : sta $7b00,y
	sta $7c00,y : sta $7d00,y : sta $7e00,y : sta $7f00,y
	iny
	bne clearscreenloop

	; Copy welcome message to video memory
	ldy #welcomemsg_end-welcomemsg_start-1
welcomemsgloop:
	lda welcomemsg_start,y
	sta $7000,y
	lda welcomemsg2_start,y
	sta $7100,y
	dey
	bpl welcomemsgloop


	; Print the first message
	ldx #bootmsg_initmessage_start-bootmsg

initprintloop:
	lda #$20 ; T2's bit
	bit VIA_IFR : beq initprintloop
	lda bootmsg,x : sta ACIA_DATA
	stz VIA_T2CH

	inx
	cpx #bootmsg_initmessage_end-bootmsg : beq initmessagedone
	cpx #bootmsg_zeropagetest_end-bootmsg : beq zeropagetestmessagedone
	cpx #bootmsg_passed_end-bootmsg : beq passedmessagedone
	cpx #bootmsg_failed_end-bootmsg : beq failmessagedone
	bra initprintloop

initmessagedone:

	; Switch to process 0 and set up a read/write mapping for its page 0
	stz VIA_PORTANH : stz PT_BASE : stz PT_BASE+$100

	; Start the RAM test
	ldx #bootmsg_zeropagetest_start-bootmsg : bra initprintloop

zeropagetestmessagedone:
	ldx #0
	lda #17
zeropagetestinitloop:
	clc : adc #13 : sta $000,x
	clc : adc #17 : sta $100,x

	tay
	adc #61 : sta $200,x : sta $300,x
	sta $400,x : sta $500,x : sta $600,x : sta $700,x
	sta $800,x : sta $900,x : sta $a00,x : sta $b00,x
	sta $c00,x : sta $d00,x : sta $e00,x : sta $f00,x
	tya

	inx : bne zeropagetestinitloop

	lda #17
zeropagetestloop:
	clc : adc #13 : cmp $000,x : bne zeropagetestfail
	clc : adc #17 : cmp $100,x : bne zeropagetestfail
	inx : bne zeropagetestloop

	ldx #bootmsg_passed_start-bootmsg : jmp initprintloop

zeropagetestfail:
	ldx #bootmsg_failed_start-bootmsg : jmp initprintloop

failmessagedone:
	stp

passedmessagedone:
	bra loadsecondstage

bootmsg:
bootmsg_initmessage_start:
	.byte 13,10,10,"===",13,10,10,"gfoot's multitasking computer bootstrap ROM",13,10,10
bootmsg_initmessage_end:
bootmsg_zeropagetest_start:
	.byte "Checking ZP/stack... "
bootmsg_zeropagetest_end:
bootmsg_passed_start:
	.byte "OK",13,10
bootmsg_passed_end:
bootmsg_failed_start:
	.byte "FAILED"
bootmsg_failed_end:


loadsecondstage:
	jsr printimm
	.byte "Loading second stage...",13,10,0

bootloop:
	jsr printimm
	.byte 1,"boot001",2,0

cmdloop:
	jsr getchar
	jsr printchar

	dec : beq readaddr
	dec : beq loaddata
	dec : beq execute
	bra bootloop

loaddata:
	ldy #0
	stz 2 : stz 3
	clc
loaddataloop:
	jsr getchar
	sta (0),y
	adc 2 : sta 2
	adc 3 : sta 3
	iny
	bne loaddataloop

	lda 2 : jsr printchar
	lda 3 : jsr printchar

	inc 1
	
	bra cmdloop

execute:
	jmp (0)


readaddr:
	; Read an address and store it at 0,1
	jsr getchar : sta 0
	jsr getchar : sta 1
	lda 0 : jsr printchar
	lda 1 : jsr printchar
	bra cmdloop


welcomemsg_start:
	.byte "gfoot's multitasking computer bootstrap ROM"
welcomemsg_end:
welcomemsg2_start:
	.byte "Loading from serial...                     "


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

