zp_ptr = 0
zp_ptr2 = 2
zp_printptr = 4
zp_prevprocess = 6
zp_lastphyspage = 7

zp_temp = $80



PID = VIA_PORTANH
ENDSUPER = VIA_PORTA


vars = $f400

var_saveda = vars
var_savedx = vars+1
var_t1ch = vars+2

var_process_status = vars+$100
var_process_regs_a = vars+$200
var_process_regs_x = vars+$300
var_process_regs_y = vars+$400
var_process_regs_sp = vars+$500
var_pagerefcounts_lo = vars+$600
var_pagerefcounts_hi = vars+$700
