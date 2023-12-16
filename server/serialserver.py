import select
import serial
import sys
import termios
import time
import tty



if False:
	cmds = {
		"boot001": (0x0200, 0x0200, "bootchain"),
		"boot002": (0x8800, 0x8800, "boottesthi"),
	}

if False:
	cmds = {
		"boot001": (0x0200, 0x0200, "boottest"),
	}

if True:
	cmds = {
		"boot001": (0x0200, 0x0200, "bootchain"),
		"boot002": (0x8800, 0x8800, "kernel"),
	}



if len(sys.argv) < 3:
	print("Usage: %s <device> <baud>" % sys.argv[0])
	sys.exit(1)

DEVICE = sys.argv[1]
BAUD = int(sys.argv[2])

LOGLEVEL = 0


def log(level, *args, end='\n'):
	if level <= LOGLEVEL:
		print(*args, end=end)
		if end != '\n':
			sys.stdout.flush()



def write(ser, data):
	for d in data:
		ser.write(bytes([d]))
		#t = time.time()
		#while time.time()-t < 0.01:
		#	pass

def adc(a, b, carry):
	r = a + b + carry
	return r % 256, r // 256



def cmd_start(cmd):
	log(3, "cmd_start(%d)" % cmd)
	write(ser, [cmd])
	echo = ser.read(1)[0]
	if echo != cmd:
		print("Echo mismatch - cmd=%s echo=%s" % (cmd, echo))
		sys.exit(1)


def cmd_end():
	log(2, "cmd_end()")
	cmd_start(0)


def cmd_address(addr):
	log(2, "cmd_address(%04x)" % addr)
	cmd_start(1)
	payload = bytes([addr % 256, addr // 256])
	write(ser, payload)
	echo = ser.read(2)
	if echo != payload:
		print("Echo mismatch - address command - payload=%s echo=%s" % (repr(payload), repr(echo)))
		sys.exit(1)


def cmd_loaddata(data):
	log(2, "cmd_loaddata")
	cmd_start(2)
	payload = bytes(data)
	log(4, "data: %s" % repr(payload))
	write(ser, payload)
	echo = ser.read(2)

	check2 = 0
	check3 = 0
	carry = 0
	for b in payload:
		check2, carry = adc(b, check2, carry)
		check3, carry = adc(check2, check3, carry)

	expected_echo = bytes([check2, check3])
	if echo != expected_echo:	
		print("Echo mismatch - loaddata command - expected=%s echo=%s" % (repr(expected_echo), repr(echo)))
		sys.exit(1)

def cmd_execute():
	log(2, "cmd_execute()")
	cmd_start(3)



def load_file_at_addr(address, filename):
	log(1, "Loading file %s to address %04x" % (filename, address))
	cmd_address(address)

	load_file(filename)


def load_file(filename):
	log(2, "Reading file '%s'" % filename)
	with open(filename, "rb") as fp:
		data = fp.read()
		fp.close()

	log(2, "Sending %04x bytes...     " % len(data), end="")
	for i in range(0, len(data), 256):
		log(3, chr(8)*4 + "%04x" % i, end="")
		block = data[i:i+256]
		if len(block) < 256:
			block = block + bytes(256-len(block))
		cmd_loaddata(block)
	log(3, chr(8)*4 + "%04x" % len(data), end="")
	log(2, "")


def go(address):
	log(1, "Go to %04x" % address)
	cmd_address(address)
	cmd_execute()


def serialconsole():
	log(1, "\n--- Serial console, ^C to quit ---\n")

	outfile = open("log.txt", "w")

	stdin = sys.stdin.fileno()
	tattr = termios.tcgetattr(stdin)
	ser.timeout = 0

	command_in_progress = False

	try:
		tty.setcbreak(stdin, termios.TCSANOW)

		while True:
			readyinputs = select.select([sys.stdin, ser], [], [], 1000)[0]
			if sys.stdin in readyinputs:
				b = sys.stdin.buffer.read(1)
				write(ser, b)
			if ser in readyinputs:
				buffer = ser.read()
				if buffer:
					for b in buffer:
						if b == 1 and not command_in_progress:
							command_in_progress = True
							command = []
						elif b == 2 and command_in_progress:
							command_in_progress = False
							termios.tcsetattr(stdin, termios.TCSANOW, tattr)
							ser.timeout = 1000
							runcommand(command)
							ser.timeout = 0
							tty.setcbreak(stdin, termios.TCSANOW)
						elif command_in_progress:
							command.append(chr(b))
						elif not command_in_progress:
							if b < 128:
								print(chr(b), end="", flush=True)
								outfile.write(chr(b))
							else:
								print("\\x%02x" % b, end="", flush=True)
								outfile.write("\\x%02x" % b)

	finally:
		termios.tcsetattr(stdin, termios.TCSANOW, tattr)

	outfile.close()


def runcommand(command):
	command = ''.join(command)
	log(1, "Command: %s" % command)

	if command in cmds:
		loadaddr, execaddr, name = cmds[command]
		load_file_at_addr(loadaddr, "../bin/apps/%s.bin" % name)
		go(execaddr)
	elif command.startswith("L"):
		name = command[1:]
		load_file("../bin/apps/%s.bin" % name)
		cmd_end()
	else:
		log(0, "Unknown command: %s" % command)


log(1, "Initialising link - %d baud" % BAUD)
ser = serial.Serial(DEVICE, BAUD, 8, "N", 1)

time.sleep(0.1)
write(ser, bytes([0xff]))

serialconsole()

