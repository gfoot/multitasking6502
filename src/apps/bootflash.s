; Stage 1 program to reflash the boot ROM
; 
; Stage 0 has already checked that paged RAM appears to work, and should have
; set PID to 0 and mapped PP0 into LP0
; 
; We will receive the ROM data via serial in the usual fashion, and write it into the EEPROM 64 bytes at a time
; 
; The datasheet wants 100ns active cycle time, which is technically violated by having a system clock faster than 5MHz
; but it probably doesn't matter
;
; We need to write the bytes within a page within 150us of each other, which shouldn't be a problem
;
; After writing 64 bytes we may need to wait for up to 10ms but we can poll the EEPROM to find out when enough time has elapsed
;
; Interrupts or resets during the write would cause all sorts of problems, so don't do that.


#include "utils/bootdefs.s"

PP_ROMIMAGEBASE = $10

* = $200
entry:
	; Disable the ROM to check pagetable entries
	lda #1
	sta VIA_PORTB

	; Map video memory to page 7 read and write
	ldy VIA_PORTANH
	lda #$f0 : sta $f000,y : sta $f100,y

	jsr printinit

	jsr printimm
	.byte 13,10,"Stage 1 - Boot ROM writer",13,10,0

	ldy VIA_PORTANH
	tya
	ora $8000,y
	ora $8100,y
	beq pidpagingok

	jsr printimm
	.byte "ERROR - boot ROM didn't set up PID and paging correctly!"

	stp


pidpagingok:

#if 0
	jmp writetest
#endif


#if 1

	; We load the ROM image into RAM as if it was a file
	;
	; Load it into PPs $10-$17
	; using LP1

	jsr printimm
	.byte 13,10,"Loading ROM image...",13,10,0

	; Disable video display output during loading
	dec videoprintdisable

bootloop:
	jsr printimm
	.byte 1,"romimage",2,0

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
	lda #'.' : jsr video_printchar_raw

	; Look at the specified load address and work out which PP it should be in
	lda 1

	pha

	lsr : lsr : lsr : lsr
	ora #PP_ROMIMAGEBASE
	sta $9000 : sta $9100

	; Update the load address to be in LP1
	lda 1 : and #$0f : ora #$10 : sta 1

	; Standard load code follows

	ldy #0
	stz 2 : stz 3 : stz 4
	clc
loaddataloop:
	jsr getchar
	sta (0),y
	tax
	adc 2 : sta 2
	adc 3 : sta 3
#if 0
	php
	txa : jsr video_printchar_raw
	lda (0),y : jsr video_printchar_raw
	plp
#endif
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

	pla
	sta 1

	inc 1
	
	jmp cmdloop

notfound:
	jmp notfoundimpl


execute:
	inc videoprintdisable

	jsr printimm
	.byte 10,10,"Writing image to ROM",13,10,0

	; The whole ROM image is loaded and verified now, we can start writing it
	;
	; Certain addresses are problematic:
	;
	;    $8000, $8100 - affect our LP0 mapping (zero page, stack, program code)
	;    $9000, $9100 - affect our LP1 mapping (used to read the ROM image)
	;    $f000, $f100 - affect our LP7 mapping (video)
	;
	; We can write everything else first using fast page writes, and then come back to these with 
	; carefully written code to deal with the issues.  It is convenient that they all occur at the 
	; start of a ROM page.
	;
	; $00/$01 currently stores the total number of bytes in the image (16-bit)

	zp_size = $00
	zp_bytesremaining = $02
	zp_readptr = $04         ; $1000-$1FFF
	zp_readpp = $06          ; $10, $11, $12, ..., $17
	zp_writeptr = $07        ; $8000-$FFFF
	zp_verifyresult = $09
	zp_verifyresult_overall = $0a

.(
	lda zp_size : sta zp_bytesremaining
	lda zp_size+1 : sta zp_bytesremaining+1

	stz zp_writeptr : lda #$80 : sta zp_writeptr+1
	stz zp_readptr : lda #$10 : sta zp_readptr+1

	lda #PP_ROMIMAGEBASE : sta zp_readpp : sta $9000 : sta $9100
	
writeloop_newline:
	jsr printnewline

	lda zp_readpp : jsr printhex : jsr printspace
	lda zp_writeptr+1 : jsr printhex
	lda zp_writeptr : jsr printhex
	jsr printspace
	lda zp_bytesremaining+1 : jsr printhex
	lda zp_bytesremaining : jsr printhex
	jsr printspace

writeloop:

	ldy #0 ; byte being written, within ROM page

	ldx #64 ; number of bytes to write in this page
	lda zp_bytesremaining+1 : bne notpartialpage  ; over 255 bytes remaining
	cpx zp_bytesremaining : bcc notpartialpage    ; at least 64 bytes remaining
	
	; Less than 64 bytes remain, so reload with just however many remain
	ldx zp_bytesremaining

notpartialpage:

.(
	; If the destination is an awkward address, skip the first byte
	lda zp_writeptr : bne notawkward
	lda zp_writeptr+1 : and #$fe
	cmp #$80 : beq awkward
	cmp #$90 : beq awkward
	cmp #$f0 : bne notawkward
awkward:
	iny
	dex

notawkward:
.)

	jsr waitromidle

.(
	; Skip I/O region
	lda zp_writeptr+1
	cmp #$84 : bcc notio
	cmp #$88 : bcs notio

	lda #'-' : jsr printchar

	jmp advance

notio:
.)

.(	; Skip bootloader for now
	cmp #$f4 : bcc notbootloader

	lda #'-' : jsr printchar

	jmp advance

notbootloader:
.)

	; Enable writing to ROM
	lda #2 : sta VIA_PORTB

	; Execute the temporary unprotect sequence
	lda #$aa : sta $d555   ; cycle 0
	lda #$55 : sta $aaaa   ; cycle 6
	lda #$a0 : sta $d555   ; cycle 12

	; Write all the data
byteloop:
	lda (zp_readptr),y : sta (zp_writeptr),y
	iny
	dex
	bne byteloop

	; Disable writing to ROM
	stz VIA_PORTB

	lda #'.' : jsr printchar

advance:
	; Decrease the bytes remaining
	sec
	lda zp_bytesremaining : sbc #64 : sta zp_bytesremaining
	lda zp_bytesremaining+1 : sbc #0 : sta zp_bytesremaining+1

	bcc finished
	ora zp_bytesremaining
	beq finished

	; Advance addresses
	clc
	lda zp_writeptr : adc #64 : sta zp_writeptr
	lda zp_writeptr+1 : adc #0 : sta zp_writeptr+1
	lda zp_readptr : adc #64 : sta zp_readptr
	lda zp_readptr+1 : adc #0 : sta zp_readptr+1

	cmp #$20 : beq nextpp
	jmp writeloop

nextpp:
	lda zp_readpp : inc : sta zp_readpp : sta $9000 : sta $9100
	lda #$10 : sta zp_readptr+1

	jmp writeloop_newline


finished:

	jsr printimm
	.byte 13,10,10,"Writing awkward bytes",13,10,0

	; All pages done, now we need to deal with $8000, $8100, $9000, $9100, $f000 and $f100.
	; We execute some code in LP2, accepting that it might corrupt mappings for LP0, LP1, and LP7,
	; and have it put the mappings back after the operation before returning.

	; Map PP0 to LP2, so all our code is also mirrored $2000 bytes higher in the address space
	stz $a000 : stz $a100

	; Handle each of the problem cases
	ldx #$80 : jsr writeawkwardbyte
	ldx #$81 : jsr writeawkwardbyte
	ldx #$90 : jsr writeawkwardbyte
	ldx #$91 : jsr writeawkwardbyte
	ldx #$f0 : jsr writeawkwardbyte
	ldx #$f1 : jsr writeawkwardbyte

	jsr printimm
	.byte 10,"Write complete",13,10,10,0
.)

	jsr printimm
	.byte 10, "Verifying...",13,10,0

.(
	; Verifying is easier because we only need to read from it, and won't corrupt the pagetable

	stz zp_verifyresult_overall

	lda zp_size : sta zp_bytesremaining
	lda zp_size+1 : sta zp_bytesremaining+1

	stz zp_writeptr : lda #$80 : sta zp_writeptr+1
	stz zp_readptr : lda #$10 : sta zp_readptr+1

	lda #PP_ROMIMAGEBASE : sta zp_readpp : sta $9000 : sta $9100

verifyloop_newline:
	jsr printnewline

	lda zp_readpp : jsr printhex : jsr printspace
	lda zp_writeptr+1 : jsr printhex
	lda zp_writeptr : jsr printhex
	jsr printspace
	lda zp_bytesremaining+1 : jsr printhex
	lda zp_bytesremaining : jsr printhex
	jsr printspace

verifyloop:

	stz zp_verifyresult

	ldy #0 ; byte being written, within ROM page

	ldx #64 ; number of bytes to write in this page
	lda zp_bytesremaining+1 : bne notpartialpage  ; over 255 bytes remaining
	cpx zp_bytesremaining : bcc notpartialpage    ; at least 64 bytes remaining
	
	; Less than 64 bytes remain, so reload with just however many remain
	ldx zp_bytesremaining

notpartialpage:

.(
	; Skip I/O region
	lda zp_writeptr+1
	cmp #$84 : bcc notio
	cmp #$88 : bcs notio

	lda #'-' : jsr printchar

	jmp advance

notio:
.)

.(	; Skip bootloader for now
	cmp #$f4 : bcc notbootloader

	lda #'-' : jsr printchar

	jmp advance

notbootloader:
.)

	; Verify the data
byteloop:
	lda (zp_readptr),y : eor (zp_writeptr),y
	ora zp_verifyresult : sta zp_verifyresult
	iny
	dex
	bne byteloop

	lda #'.'
	ldx zp_verifyresult
	beq verifyok
	lda #'X'
	stx zp_verifyresult_overall
verifyok:
	jsr printchar

advance:
	; Decrease the bytes remaining
	sec
	lda zp_bytesremaining : sbc #64 : sta zp_bytesremaining
	lda zp_bytesremaining+1 : sbc #0 : sta zp_bytesremaining+1

	bcc finished
	ora zp_bytesremaining
	beq finished
	
	; Advance addresses
	clc
	lda zp_writeptr : adc #64 : sta zp_writeptr
	lda zp_writeptr+1 : adc #0 : sta zp_writeptr+1
	lda zp_readptr : adc #64 : sta zp_readptr
	lda zp_readptr+1 : adc #0 : sta zp_readptr+1

	cmp #$20 : beq nextpp
	jmp verifyloop

nextpp:

	lda zp_readpp : inc : sta zp_readpp : sta $9000 : sta $9100
	lda #$10 : sta zp_readptr+1

	jmp verifyloop_newline


finished:
	jsr printnewline

	lda zp_verifyresult_overall
	beq verify_overall_ok

	jsr printimm
	.byte 10,10,"Verification failed",13,10,0
	stp

verify_overall_ok:
	jsr printimm
	.byte 10,10,"Done",13,10,0
.)

	stp


waitromidle:
.(
	pha
poll:
	lda $8000 : cmp $8000 : bne poll
	pla
	rts
.)


notfoundimpl:
	inc videoprintdisable
	jsr printimm
	.byte "Server didn't supply ROM image", 13, 10, 0
	stp


* = * + $2000
writeawkwardbyte:
.(
	; Calculate PP where the ROM image data is stored for this byte
	txa : and #$70
	lsr : lsr : lsr : lsr
	ora #PP_ROMIMAGEBASE

	; Set the PP mapping for LP1
	sta $9000 : sta $9100

	; Calculate the logical address where we can read the data from
	txa : and #$0f : ora #$10 : sta zp_readptr+1
	stz zp_readptr

	; Read the data
	lda (zp_readptr)

	; Form the write pointer
	stx zp_writeptr+1
	stz zp_writeptr

	jsr waitromidle

	; Enable writing to ROM
	ldx #2 : stx VIA_PORTB

	; Execute the temporary unprotect sequence
	ldx #$aa : stx $d555   ; cycle 0
	ldx #$55 : stx $aaaa   ; cycle 6
	ldx #$a0 : stx $d555   ; cycle 12
	
	; Write the value - this may cause ZP/stack to be unavailable!  NMI would be bad here.
	sta (zp_writeptr)

	; Disable writing to ROM
	stz VIA_PORTB

	; Restore LP0 and LP7 mappings in case we corrupted them
	stz $8000 : stz $8100
	lda #$f0 : sta $f000 : sta $f100

	rts
.)
* = * - $2000

#endif


#if 0

writetest:	

	; Reenable the ROM
	stz VIA_PORTB

	lda #3 : sta VIA_DDRB


	; As a test, increment a byte of the ROM
	;
	; $e000 is unused in the ROM

	jsr printimm
	.byte 13,10,"Old values:",13,10,0

	jsr printpage
	jsr printnewline

	ldy $e000
	iny

	; Enable writing to ROM
	ldx #2 : stx VIA_PORTB

	; Execute the temporary unprotect sequence
	lda #$aa : sta $d555   ; cycle 0
	lda #$55 : sta $aaaa   ; cycle 6
	lda #$a0 : sta $d555   ; cycle 12

	; Write new values
	tya
	ldy #0
.(
writeloop:
	sta $e000,y
	inc
	iny
	cpy #64
	bne writeloop
.)

	; Disable writing to ROM
	stz VIA_PORTB

	; Poll for completion
	stz 0
	stz 1
	stz 2
	ldx #0

.(
poll:
	lda $e000 : cmp $e000 : beq done

	inx : bne poll
	inc 0 : bne poll
	inc 1 : bne poll
	inc 2 : bra poll

done:
.)
	lda 2 : jsr printhex
	lda 1 : jsr printhex
	lda 0 : jsr printhex
	txa : jsr printhex

	jsr printimm
	.byte 13,10,"New values:",13,10,0

	jsr printpage
	jsr printnewline

	lda $e000 : jsr printhex : jsr printnewline
	lda $d555 : jsr printhex : jsr printnewline
	lda $aaaa : jsr printhex : jsr printnewline

	stp


printpage:
.(
	ldy #0
lineloop:
	ldx #16
loop:
	lda $e000,y : jsr printhex : jsr printspace
	iny : cpy #64 : beq done
	dex : bne loop
	jsr printnewline
	bra lineloop
done:
	jsr printnewline
	rts
.)

#endif

#include "utils/bootprint.s"

#print *
