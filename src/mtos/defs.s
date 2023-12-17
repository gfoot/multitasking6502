; Kernel zero page allocation

zp_ptr = 0
zp_ptr2 = 2
zp_printptr = 4
zp_prevprocess = 6
zp_lastphyspage = 7
zp_serial_out_tail = 8
zp_serial_out_head = 9

zp_temp = $80


; Hardware registers
PID = VIA_PORTANH
ENDSUPER = VIA_PORTA


; Pagetable base addresses (add on the PID)
PT_LP0W = $8000
PT_LP0R = $8100
PT_LP1W = $9000
PT_LP1R = $9100
PT_LP2W = $a000
PT_LP2R = $a100
PT_LP3W = $b000
PT_LP3R = $b100


; Start addresses of some useful logical pages
LP0 = $0000
LP1 = $1000
LP2 = $2000
LP3 = $3000
LP4 = $4000
LP5 = $5000
LP6 = $6000
LP7 = $7000


; Kernel general purpose storage

vars = $f400        ; base address

; Small variables
var_saveda = vars
var_savedx = vars+1
var_t1ch = vars+2

; Large arrays
var_process_status = vars+$100
var_process_regs_a = vars+$200
var_process_regs_x = vars+$300
var_process_regs_y = vars+$400
var_process_regs_sp = vars+$500
var_pagerefcounts_lo = vars+$600
var_pagerefcounts_hi = vars+$700
var_serial_out_buffer = vars+$800
/* max is vars+$a00 */
