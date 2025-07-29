; Kernel zero page allocation

zp_ptr = 0
zp_ptr2 = 2
zp_printptr = 4
zp_prevprocess = 6
zp_lastphyspage = 7
zp_serial_out_tail = 8
zp_serial_out_head = 9
zp_runqueue_tail = 10
zp_runqueue_head = 11
zp_serial_in_tail = 12
zp_serial_in_head = 13

zp_video_cursoraddr = $10
zp_video_ptr1 = $12
zp_video_ptr2 = $14
zp_video_ptr3 = $16

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
PT_LP4W = $c000
PT_LP4R = $c100
PT_LP5W = $d000
PT_LP5R = $d100
PT_LP6W = $e000
PT_LP6R = $e100
PT_LP7W = $f000
PT_LP7R = $f100


; Start addresses of some useful logical pages
LP0 = $0000
LP1 = $1000
LP2 = $2000
LP3 = $3000
LP4 = $4000
LP5 = $5000
LP6 = $6000
LP7 = $7000


; Video framebuffer is in LP7
LPVID = LP7
PT_LPVW = PT_LP7W
PT_LPVR = PT_LP7R


; Kernel general purpose storage

vars = $ff00        ; base address for small vars

; Small variables
var_saveda = vars
var_savedx = vars+1
var_t1ch = vars+2
var_interrupt_from_supervisor = vars+3
var_superirq_saveda = vars+4
var_superirq_saved_pid = vars+5

arrays = $f400      ; base address for arrays

; Large arrays
var_runqueue = arrays
var_process_status = arrays+$100
var_process_regs_a = arrays+$200
var_process_regs_x = arrays+$300
var_process_regs_y = arrays+$400
var_process_regs_sp = arrays+$500
var_pagerefcounts_lo = arrays+$600
var_pagerefcounts_hi = arrays+$700
var_serial_out_buffer = arrays+$800
var_serial_in_buffer = arrays+$900
/* max is arrays+$a00 */

