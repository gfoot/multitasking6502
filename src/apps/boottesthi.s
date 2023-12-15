; High-loading test case as second stage for bootloader

#include "defs.s"


zp_ptr = $00
zp_physpage = $02
zp_numphyspages = $03

printptr = $80


* = $8800
entry:
.(
	; Make sure the mapping is good for our page zero / stack
	stz VIA_PORTANH : stz $8000 : stz $8100

	jsr pagingtest

	jsr pagedramtest

	jsr usermodetest

	jsr preempttest

loop:
	jsr printimm
	.byte "All work and no play  ",0
	bra loop
.)


; Pagetable address format:
;
;   15  14  13  12  11  10   9   8   7 ... 0
;    1  P2  P1  P0   0   0  P3   R   \-PID-/


pagingtest:
.(
	jsr printimm
	.byte "Checking pagetable... ",0

	; We want to test that the paging mechanism is working properly, without corrupting our own zero page/stack.
	; Right now, PID=0 and its logical page 0 is mapped for read and write to physical page 0.
	;
	; We will use logical pages 1 and 2 for the test.  Initially we will map each physical page in turn to page 1
	; and write the physical page numbers into byte $FFF, checking they stick.
	;
	; Then we'll set page 1 to read physical page 1 but write physical page 2, and check that works as expected.
	;
	; Then we'll repeat the first test, but only reading back the values, using page 2 instead of page 1.

	ldx #0
initloop:
	stx $9000    ; Select for writing, page 1
	stx $9100    ; Select for reading, page 1
	stx $1fff    ; Write physical page number into its top byte
	cpx $1fff    ; Check physical page number is stored in its top byte
	bne initloopfail
	inx
	cpx #8       ; Stop at 8 because the hardware only has 8 physical pages
	bne initloop

	bra test

initloopfail:
	jsr printimm
	.byte "initloop fail ",0
	jsr printregs
	stp

test:
	ldx #0
testloop:
	stx $9000    ; Select for writing, page 1
	stx $9100    ; Select for reading, page 1
	cpx $1fff    ; Check physical page number is stored in its top byte
	bne testloopfail
	inx
	cpx #8       ; Stop at 8 because the hardware only has 8 physical pages
	bne testloop

	bra rwtest

testloopfail:
	jsr printimm
	.byte "testloop fail ",0
	jsr printregs
	stp

rwtest:
	lda #0 ; used as error stage flag

	ldx #1 : stx $9100  ; page 1 read from physical page 1
	ldy #2 : sty $9000  ; page 1 write to physical page 2

	lda #1
	cpx $1fff           ; X (1) should still be stored at $FFF in physical page 1, mapped for read to logical page 1
	bne rwtestfail

	lda #2
	stz $1fff           ; overwrite $FFF in physical page 2
	cpx $1fff           ; check physical page 1 was unaffected
	bne rwtestfail

	lda #3
	sty $9100           ; page 1 read from physical page 2
	ldx $1fff           ; check the overwrite happened to this page instead
	bne rwtestfail

	; put it back
	ldx #2 : stx $1fff

	bra page2test

rwtestfail:
	jsr printimm
	.byte "rwtest fail ",0
	jsr printregs
	stp

page2test:
	ldx #0
page2testloop:
	stx $a100    ; Select for reading, page 2
	cpx $2fff    ; Check physical page number is stored in its top byte
	bne page2testloopfail
	inx
	cpx #8       ; Stop at 8 because the hardware only has 8 physical pages
	bne page2testloop

	jsr printimm
	.byte "OK",13,10,0

	rts

page2testloopfail:
	jsr printimm
	.byte "page2test fail ",0
	jsr printregs
	stp
.)


pagedramtest:
.(

	jsr printimm
	.byte "Measuring paged RAM... ",0
.(
	; Find the first physical page, above 0, which can be written and read without affecting page 0

	ldx #$ff : stx $fff    ; marker

	ldx #1
loop:
	stx $9000 : stx $9100   ; Select this physical page for reading and writing by logical page 1

	; Store something nonzero in the page, and read it back to check the page has memory backing it
	stx $1fff : cpx $1fff : bne nomemory

	; Store zero in the page, and read back the marker from physical page 0, to check whether the
	; physical memory has wrapped
	stz $1fff : ldy $fff : beq wrapped

	inx
	bne loop

nomemory:
wrapped:
	; Whatever X is, we have that many pages of 4K each in the system
	stx zp_numphyspages

	stz zp_ptr+1
	txa
	asl : rol zp_ptr+1
	asl : rol zp_ptr+1
	sta zp_ptr

	ldx #<zp_ptr : jsr printdecu16

	jsr printimm
	.byte "K",13,10,0
.)

.(
	jsr printimm
	.byte "Thoroughly testing paged RAM... ",0

	; Logical page 0 is used for zero page and stack, so we won't change that - it is also using physical
	; page 0 which we will also not touch.
	;
	; For the rest, we want to fill the entire memory space with a distinctive pattern, different in every 
	; physical page, and then go back and check that the pattern still exists in every physical page.
	;
	; We will use logical page 1 throughout.
	
	jsr printimm
	.byte "init",0

	ldx #1

initpageloop:
	stx zp_physpage
	stx $9000 : stx $9100   ; select this physical page for reading and writing by logical page 1

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
	inx
	cpx zp_numphyspages
	bne initpageloop
	
	bra test

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
	stx $9000 : stx $9100   ; select this physical page for reading and writing by logical page 1

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
	inx
	cpx zp_numphyspages
	bne testpageloop
	
	bra testcomplete

testfail:
	jsr printimm
	.byte " testfail ",13,10,0
	jsr printregs
	jsr printzp16
	stp

testcomplete:

	jsr printimm
	.byte ", OK",13,10,0
.)

	rts
.)


usermodetest:
.(
	; A bit tricky as there's no way to return from user mode - it has to issue a BRK and we pick up from there.
	; So we set the IRQ vector to our continuation, then chain to the user-mode code.
	;
	; The mechanism of returning to user mode is via RTI which then pops the flags and return address from the 
	; user-mode stack.  This will be in sync with the supervisor stack pointer in this case - though generally
	; in practice it will not remain in sync, these are controlled conditions.

	jsr printimm
	.byte "User mode switch test... ",0

	lda #<continuation : sta $fffe
	lda #>continuation : sta $ffff

	; Map physical page 1 as PID 1's page 0, and as our page 1
	lda #1 : sta $8001 : sta $8101
	sta $9000 : sta $9100

	; Write some code there, at $200
	ldy #usermodecodesize
copyloop:
	lda usermodecode-1,y
	sta $11ff,y
	dey
	bne copyloop

	; Write some flags and a return address to its stack, so we can RTI into it
	php : php : php
	tsx
	stx 7
	lda #2 : sta $1103,x
	lda #0 : sta $1102,x
	pla : pha : sta $1101,x

	; Activate the process and RTI
	lda #1 : sta VIA_PORTA
	rti

continuation:
	; We were called as an interrupt, so there are flags and the process's return address on the stack
	cmp #7 : bne fail
	pla : plx : ply : jsr printregs
	tsx : txa : jsr printhex : jsr printspace

	; It was a bit naughty to do so many JSRs with the user's PID, but this is just test code
	stz VIA_PORTANH

	jsr printimm
	.byte "OK",13,10,0
	
	rts

fail:
	jsr printimm
	.byte "FAIL",0
	stp

usermodecode:
* = $200
	lda #7
	brk : .byte 0
	stp
usermodecodesize = *-$200
* = usermodecode + usermodecodesize

.)


preempttest:
.(
	; As above, we need a trick here to catch the IRQ and resume

	jsr printimm
	.byte "Preempting test... ",0

	lda #<continuation1irq : sta $fffe
	lda #>continuation1irq : sta $ffff
	lda #<continuation1reset : sta $fffc
	lda #>continuation1reset : sta $fffd

	; Map physical page 1 as PID 1's page 0
	lda #1 : sta $8001 : sta $8101

	; Switch to PID 1
	sta VIA_PORTANH

	; Write some code there, at $200
	ldy #usermodecodesize
copyloop:
	lda usermodecode-1,y
	sta $1ff,y
	dey
	bne copyloop

	; Write some flags and a return address to its stack, so we can RTI into it
	lda #2 : pha
	lda #0 : pha
	php

	; Set up the VIA's T1 to interrupt us after a while
	cycles = 512
	lda #<cycles : sta VIA_T1CL
	lda #>cycles : sta VIA_T1CH

	lda VIA_ACR : and #$3f : ora #$40 : sta VIA_ACR   ; enable continuous T1 interrupts

	lda #$40 : sta VIA_IFR       ; clear T1's flag
	lda #$7F : sta VIA_IER       ; clear other interrupt enable bits
	lda #$c0 : sta VIA_IER       ; set T1's interrupt enable bit

	; Switch to user mode and return
	lda #1 : ldx #2 : ldy #3
	bit VIA_PORTA
	rti

fail:
	jsr printimm
	.byte "FAIL",0
	stp

continuation1reset:
	stz VIA_PORTANH

	jsr printimm
	.byte "FAIL (reset not IRQ)", 0
	stp

continuation1irq:
	stz VIA_PORTANH

	; We were called as an interrupt, so there are flags and the process's return address on the stack
	cmp #7 : bne fail

	jsr printregs

	jsr printimm
	.byte "OK",13,10,0

	jsr printimm
	.byte "Wait a bit to ensure super mode isn't killed... ",0
;	ldx #0 : ldy #0 : lda #0
;waitabit:
;	dex : bne waitabit
;	dey : bne waitabit
;	dec : bne waitabit

	jsr printimm
	.byte "OK",13,10,0

	jsr printimm
	.byte "Preempt blocking test... ",0

	; For this test, we don't expect to get the IRQ continuation, but we do expect to get a reset when the hardware
	; decides IRQs have been disabled for too long
	
	lda #<continuation2irq : sta $fffe
	lda #>continuation2irq : sta $ffff
	lda #<continuation2reset : sta $fffc
	lda #>continuation2reset : sta $fffd

	; Stash the stack pointer somewhere as it might get lost in the reset
	tsx : stx 7
	txa : jsr printhex : jsr printspace

	; Switch the MMU back to the user process
	lda #1 : sta VIA_PORTANH

	; Pull the flags then repush our flags with the I bit set
	; edit: this has been modified to test STP instead of having the I bit set, but both work
	;pla : ;php
	lda #$db : sta $204    ; Overwrite the BRA instruction with a STP

	; Restart T1, check we clear pending interrupts
	lda #>cycles : sta VIA_T1CH
	lda VIA_IFR : ora ACIA_STAT : bpl nopendingirq
	
	lda VIA_IFR : ldx ACIA_STAT
	jsr printimm
	.byte "Pending IRQs! ",0
	jsr printhex : jsr printdot : txa : jsr printhex : jsr printspace

nopendingirq:
	; Return to the user process that's still running - with chosen values in registers
	lda #1 : ldx #2 : ldy #3
	bit VIA_PORTA
	rti

continuation2irq:
	jsr printimm
	.byte "IRQ ",0

	pha
	lda #$40
	sta VIA_IFR

	lda VIA_IFR : jsr printhex
	lda ACIA_STAT : jsr printhex
	pla

	rti
	stz VIA_PORTANH

	jsr printimm
	.byte "FAIL (IRQ not reset)",0
	stp

continuation2reset:
	stz VIA_PORTANH

	; After a reset, context may have been lost - let's check
	stx 8
	tsx : stx 9
	ldx 7 : txs
	ldx 8
	jsr printregs
	lda 9 : jsr printhex : jsr printspace

	jsr printimm
	.byte "OK",13,10,0

	; Discard the interrupt frame
	pla : pla : pla

	rts

usermodecode:
* = $200
.(
	cli
	lda #7
loop:
	inx
	bra loop
.)
usermodecodesize = *-$200
* = usermodecode + usermodecodesize

.)


printregs:
	pha
	jsr printhex : jsr printspace
	txa : jsr printhex : jsr printspace
	tya : jsr printhex : jsr printspace
	pla
	rts


printzp16:
.(
	pha : phx

	ldx #0
loop:
	lda 0,x : jsr printhex : jsr printspace
	inx
	cpx #$10 : bne loop

	plx : pla
	rts
.)


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


#include "utils/print.s"

#print *-entry

