; Stage 1 bootloader
;
; Lives in paged RAM
; Disables ROM and checks private RAM is working
; Loads Stage 2 into private RAM, from serial or from ROM

#include "utils/bootdefs.s"

zp_ptr = $00

* = $200

entry:
	; Disable the ROM now
	lda #1
	sta VIA_PORTB

	; Map video memory to page 7 read and write
	ldy VIA_PORTANH
	lda #$f0 : sta $f000,y : sta $f100,y

	jsr printinit

	jsr printimm
	.byte 13,10,"Stage 1",13,10,0

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
	.byte 13,10,"Loading second stage...",13,10,0

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
	jmp (0)

notfound:
	; If stage 2 wasn't supplied by serial, use the embedded one

	inc videoprintdisable

	jsr printimm
	.byte "Loading Stage 2 from ROM",13,10,0

	; Re-enable the ROM
	stz VIA_PORTB

	; Loop over the kernel's footprint in ROM, rewriting it to populate the private RAM
	; This will also overwrite some pagetable entries but nothing critical so long as we stop at $f000
	kernel_start = $8800
	kernel_end = $f000

	lda #<kernel_start : sta 0
	lda #>kernel_start : sta 1
	ldx #>kernel_end

	ldy #0
copykernelloop:
	lda (0),y : sta (0),y
	iny
	bne copykernelloop
	inc 1
	cpx 1
	bne copykernelloop

	; Disable the ROM again
	lda #1 : sta VIA_PORTB

	jsr printimm
	.byte "Starting Stage 2",13,10,0

	jmp kernel_start


privateramtest:
.(
	jsr printimm
	.byte "Testing private RAM... ",0

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


#include "utils/bootprint.s"

headroom = $1000-*
#print headroom

