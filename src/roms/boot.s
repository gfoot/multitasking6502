; Multitasking system boot ROM
; 
; Stage 0
; 
; Initialises some hardware
; Checks that RAM is working
; Loads Stage 1 from serial link to a PC, or from ROM

.(

#include "utils/bootdefs.s"


* = $ROMBASE

firststage_target = $0200
firststage_start:
	.bin 0,0,"../bin/apps/bootchain.bin"
firststage_end:

#print $8400-firststage_end

#if *>$8400
#error overflow 8400
#endif

	.dsb $8800-*, $00

secondstage_start:
	.bin 0,0,"../bin/apps/kernel.bin"
secondstage_end:

#print $f400-secondstage_end

#if *>$f4000
#error overflow f400
#endif

	.dsb $f400-*, 0

entry:
	ldx #$ff : txs
	sei : clv : cld : cpx #0
	txa : tay

	; Initialise the VIA

	; Port A - all outputs, initially 0 (PID)
	stz VIA_PORTANH
	lda #$ff : sta VIA_DDRA

	; Port B - bit 0 and 1 are outputs, initially low (inverted ROMEN, and ROMW)
	stz VIA_PORTB
	lda #3 : sta VIA_DDRB

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


	; Print the first message
	ldx #bootmsg_initmessage_start-bootmsg

initprintmsg:
	ldy #0

initprintloop:
.(
	lda #$20 ; T2's bit
	bit VIA_IFR : beq initprintloop
	lda bootmsg,x : sta ACIA_DATA
	stz VIA_T2CH

	cmp #10 : beq nl
	jmp notnl

nl:
	; Scroll the screen
	ldy #79
scrollloop:
	lda $7080,y : sta $7000,y
	lda $7100,y : sta $7080,y
	lda $7180,y : sta $7100,y
	lda $7200,y : sta $7180,y
	lda $7280,y : sta $7200,y
	lda $7300,y : sta $7280,y
	lda $7380,y : sta $7300,y
	lda $7400,y : sta $7380,y
	lda $7480,y : sta $7400,y
	lda $7500,y : sta $7480,y
	lda $7580,y : sta $7500,y
	lda $7600,y : sta $7580,y
	lda $7680,y : sta $7600,y
	lda $7700,y : sta $7680,y
	lda $7780,y : sta $7700,y
	lda $7800,y : sta $7780,y
	lda $7880,y : sta $7800,y
	lda $7900,y : sta $7880,y
	lda $7980,y : sta $7900,y
	lda $7a00,y : sta $7980,y
	lda $7a80,y : sta $7a00,y
	lda $7b00,y : sta $7a80,y
	lda $7b80,y : sta $7b00,y
	lda $7c00,y : sta $7b80,y
	lda $7c80,y : sta $7c00,y
	lda $7d00,y : sta $7c80,y
	lda $7d80,y : sta $7d00,y
	lda $7e00,y : sta $7d80,y
	lda $7e80,y : sta $7e00,y
	lda #0 : sta $7e80,y
	dey
	bmi noskip
	jmp scrollloop

notnl:
	cmp #13 : bne notcr
	ldy #0

notcr:
	cmp #$20 : bcc skip
	sta $7e80,y

noskip:
	iny
skip:

	inx
	cpx #bootmsg_initmessage_end-bootmsg : beq initmessagedone
	cpx #bootmsg_zeropagetest_end-bootmsg : beq zeropagetestmessagedone
	cpx #bootmsg_passed_end-bootmsg : beq passedmessagedone
	cpx #bootmsg_failed_end-bootmsg : beq failmessagedone
	jmp initprintloop
.)

initmessagedone:

	; Switch to process 0 and set up a read/write mapping for its page 0
	stz VIA_PORTANH : stz PT_BASE : stz PT_BASE+$100

	; Start the RAM test
	ldx #bootmsg_zeropagetest_start-bootmsg : jmp initprintmsg

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

	ldx #bootmsg_passed_start-bootmsg : jmp initprintmsg

zeropagetestfail:
	ldx #bootmsg_failed_start-bootmsg : jmp initprintmsg

failmessagedone:
	stp

passedmessagedone:
	bra loadsecondstage


bootmsg:
bootmsg_initmessage_start:
	.byte 13,10,10,"===",13,10,10,"gfoot's multitasking computer bootstrap ROM",13,10,10
bootmsg_initmessage_end:
bootmsg_zeropagetest_start:
	.byte "Stage 0",13,10,10,"Checking ZP/stack...",13,10
bootmsg_zeropagetest_end:
bootmsg_passed_start:
	.byte "    OK",13,10
bootmsg_passed_end:
bootmsg_failed_start:
	.byte "    FAILED"
bootmsg_failed_end:


loadsecondstage:

	jsr printinit

	jsr printimm
	.byte 10,"Loading Stage 1...",13,10,0

	dec videoprintdisable

bootloop:
	jsr printimm
	.byte 1,"boot001",2,0

cmdloop:
	jsr getchar
	jsr printchar

	dec : beq readaddr
	dec : beq loaddata
	dec : beq execute
	dec : beq notfound
	bra bootloop

readaddr:
	; Read an address and store it at 0,1
	jsr getchar : sta 0
	jsr getchar : sta 1
	lda 0 : jsr printchar
	lda 1 : jsr printchar
	bra cmdloop

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

notfound:
	; If the first stage wasn't found then use the embedded first stage

	inc videoprintdisable

	jsr printimm
	.byte "Loading Stage 1 from ROM",13,10,0

	lda #<firststage_start : sta 0
	lda #>firststage_start : sta 1
	lda #<firststage_target : sta 2
	lda #>firststage_target : sta 3

	ldx #>(firststage_end-firststage_start+255)

	ldy #0
relocloop:
	lda (0),y : sta (2),y
	iny
	bne relocloop
	inc 3
	inc 1
	dex
	bne relocloop	

	jsr printimm
	.byte "Starting Stage 1",13,10,0

	jmp firststage_target


#include "utils/bootprint.s"


nmi:
	ldx #$fe : stx VIA_PORTANH
	stp

irq:
	ldx #$fd : sta VIA_PORTANH
	stp


top:

#print $fffa-top
	.dsb $fffa-*, $00

vectors:
	.word nmi
	.word entry
	.word irq
.)

