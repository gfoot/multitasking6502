; Test app for mtos kernel - does "life" in a quarter of the screen

#include "mtos_syscalls.s"

zp_pid = 0
zp_screen_base = 1

YSIZE = 39  ; across
XSIZE = 15  ; down

zp_ptr = $20
zp_temp = $22
zp_hsumptr = $23
zp_hsumptrup = $25
zp_hsumptrdown = $27
zp_xcount = $29
zp_ptr2 = $2a


* = $200
entry:
.(
	; A = PID, we use this to decide which bit of the screen to use
	sta zp_pid

	; Set up the video memory mapping for page F
	lda #$f
	SYSCALL_MAPMEMORY

	lda zp_pid
	; Make a pointer to the top left cell of the area we'll use
	ldy #$f0
	ldx #$00
	ror : bcc toprow
	ldy #$f7
	ldx #$80
toprow:
	stx zp_screen_base
	sty zp_screen_base+1

	ror : bcc leftside

	clc
	lda zp_screen_base : adc #40 : sta zp_screen_base
	lda zp_screen_base+1 : adc #0 : sta zp_screen_base+1

leftside:

	; Clear the region
	lda zp_screen_base : sta zp_ptr
	lda zp_screen_base+1 : sta zp_ptr+1

	ldx #XSIZE-1
rowloop:
	lda #0
	ldy #YSIZE-1
colloop:
	sei : sta (zp_ptr),y : cli
	dey
	bpl colloop

	clc
	lda zp_ptr : adc #$80 : sta zp_ptr
	lda zp_ptr+1 : adc #0 : sta zp_ptr+1

	dex
	bne rowloop

	; Draw borders
	ldx #YSIZE
	lda #$c4
borderrowloop:
	dex
	sei : sta $f000+(XSIZE-1)*$80,x : cli
	sei : sta $f000+(XSIZE-1)*$80+YSIZE+1,x : cli
	cpx #0
	bne borderrowloop

	lda #YSIZE : sta zp_ptr
	lda #$f0 : sta zp_ptr+1
	ldx #2
	ldy #XSIZE-1

bordercolloop:
	lda #$b3 : sei : sta (zp_ptr) : cli
	
bordercolloopincr:
	clc
	lda zp_ptr : adc #$80 : sta zp_ptr
	lda zp_ptr+1 : adc #0 : sta zp_ptr+1

	dey : bne bordercolloop

	dex : beq borderdone

	ldy #XSIZE

	lda #$c5 : sei : sta (zp_ptr) : cli

	bra bordercolloopincr

borderdone:
.)

.(

	lda #<init_grid0 : sta zp_ptr
	lda #>init_grid0 : sta zp_ptr+1

	; Advance zp_ptr to skip init grids for other pids
	lda zp_pid
	and #3
	tay
	beq noskip
	
skiploop:

	ldx #XSIZE
skiploopinner:

	clc
	lda zp_ptr : adc #YSIZE : sta zp_ptr
	lda zp_ptr+1 : adc #0 : sta zp_ptr+1

	dex
	bne skiploopinner

	dey
	bne skiploop

noskip:

	lda zp_screen_base : sta zp_ptr2
	lda zp_screen_base+1 : sta zp_ptr2+1

	ldx #XSIZE-1
	stx zp_xcount
xloop:
	ldy #0 : sty zp_temp
	ldy #YSIZE-1 : sty zp_temp+1
yloop:
	ldy zp_temp : lda (zp_ptr),y : tax
	ldy zp_temp+1 : lda (zp_ptr),y
	ldy zp_temp : sei : sta (zp_ptr2),y : cli
	ldy zp_temp+1 : txa : sei : sta (zp_ptr2),y : cli
	inc zp_temp
	dec zp_temp+1
	ldy zp_temp+1
	cpy #YSIZE/2
	bcs yloop

	clc
	lda zp_ptr : adc #YSIZE : sta zp_ptr
	lda zp_ptr+1 : adc #0 : sta zp_ptr+1

	clc
	lda zp_ptr2 : adc #$80 : sta zp_ptr2
	lda zp_ptr2+1 : adc #0 : sta zp_ptr2+1

	dec zp_xcount
	bne xloop

loop:
	jsr sumhoriz
	jsr updatecells

	ldx #0 : ldy #$40
delay:
	dex : bne delay
	dey : bne delay

	bra loop
.)


sumhoriz:
.(
	; Loop over cells 0..YSIZE-2 x 0..XSIZE-2, calculating sum of cell content with left and right neighbours
	;
	; When we reach each cell, we already know its sum with its left neighbour.  We need to add its right neighbour,
	; store the result, then subtract its left neigbour again.
	;
	; For each row, we precalculate the border sums (cell 0 and cell YSIZE-1 have the same value) and then
	; loop left from YSIZE-2 down to 1 filling in the others.
	;
	; We only loop over XSIZE-1 rows; the last would just be a copy of the opposite edge row.

	lda zp_screen_base : sta zp_ptr
	lda zp_screen_base+1 : sta zp_ptr+1
	lda #<data_hsumgrid : sta zp_hsumptr
	lda #>data_hsumgrid : sta zp_hsumptr+1
	ldx #XSIZE-1
	clc
xloop:
	; Copy left cell to right cell
	ldy #YSIZE-1
	lda (zp_ptr) : sei : sta (zp_ptr),y : cli

	; Add cell YSIZE-2
	dey : adc (zp_ptr),y

	; Now we're ready for the row loop starting with cell YSIZE-2
yloop:
	; We need to add the cell to the left, write the result, subtract the cell to the right, and continue
	dey : adc (zp_ptr),y
	iny : sta (zp_hsumptr),y
	iny : sec : sbc (zp_ptr),y : clc
	dey : dey
	bne yloop

	; Calculate value for cell 0 (wrapping to YSIZE-2).  A already holds cell 0 plus cell 1.
	ldy #YSIZE-2 : adc (zp_ptr),y
	sta (zp_hsumptr)
	iny : sta (zp_hsumptr),y

	; Advance to next row
	dex : beq done

	; Add 128 to zp_ptr
	lda zp_ptr : adc #$80 : sta zp_ptr
	lda zp_ptr+1 : adc #0 : sta zp_ptr+1

	; Add YSIZE to zp_hsumptr
	lda zp_hsumptr : adc #YSIZE : sta zp_hsumptr
	bcc xloop
	clc
	inc zp_hsumptr+1
	bra xloop

done:

	; For the last row, just copy the first row
	ldy #YSIZE-1
lastrowloop:
	lda data_hsumgrid,y : sta data_hsumgrid+YSIZE*(XSIZE-1),y
	dey
	bpl lastrowloop

	rts
.)


updatecells:
.(
	; The hsum grid contains, for each cell, the sum of it and its horizontal neighbours.
	; We can loop over all the cells, for each cell adding its hsum to the hsums above and below
	; and then calculate the next state of the cell and updating it in place.

	lda zp_screen_base : sta zp_ptr
	lda zp_screen_base+1 : sta zp_ptr+1

	lda #<data_hsumgrid : sta zp_hsumptr
	lda #>data_hsumgrid : sta zp_hsumptr+1
	lda #<(data_hsumgrid+YSIZE) : sta zp_hsumptrdown
	lda #>(data_hsumgrid+YSIZE) : sta zp_hsumptrdown+1
	lda #<(data_hsumgrid+YSIZE*(XSIZE-2)) : sta zp_hsumptrup
	lda #>(data_hsumgrid+YSIZE*(XSIZE-2)) : sta zp_hsumptrup+1

	ldx #XSIZE-1
	stx zp_xcount
	clc
xloop:
	ldy #YSIZE-2
yloop:
	; Calculate the sum
	lda (zp_hsumptr),y : adc (zp_hsumptrdown),y : adc (zp_hsumptrup),y

	; Double it and add the current state
	asl : ora (zp_ptr),y

	; Use a lookup table to determine its next state
	tax : lda lookup,x : sei : sta (zp_ptr),y : cli

	; Advance to next cell
	dey
	bpl yloop

	; Advance to next row
	dec zp_xcount
	beq done

	lda zp_ptr : adc #128 : sta zp_ptr
	lda zp_ptr+1 : adc #0 : sta zp_ptr+1

	lda zp_hsumptr : sta zp_hsumptrup
	lda zp_hsumptrdown : sta zp_hsumptr
	adc #YSIZE : sta zp_hsumptrdown

	lda zp_hsumptr+1 : sta zp_hsumptrup+1
	lda zp_hsumptrdown+1 : sta zp_hsumptr+1
	adc #0 : sta zp_hsumptrdown+1

	bra xloop

done:
	rts

lookup:
	; A cell lives if 2*neighboursum+self is:
	;    0011 1  - live with two other neighbours
	;    0011 0  - dead with three neighbours
    ;    0100 1  - live with three other neighbours
	; Otherwise it dies.
	.byte 0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0
.)

init_grid0:
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0
.byte 0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,1,1,1,1,1,1,0,0,0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,0,0
.byte 0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

init_grid1:
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,1,1,1,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,1,1,1,0,0,0,0,1,1,1,1,1,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

init_grid2:
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,1,1,1,1,1,1,0,0,0,1,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,1,1,1,1,1,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

init_grid3:
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,1,1,1,1,1,1,0,0,0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0
.byte 0,0,0,0,0,1,1,1,1,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0


data_hsumgrid:

