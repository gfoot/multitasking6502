#BURN = ../bin/roms/test.rom
BURN = ../bin/roms/boot.rom
#BURN = ../bin/roms/testram.rom

#ROMTYPE = AT28C64B
#ROMBASE = E000

ROMTYPE = AT28C256E
ROMBASE = 8000


ROMS = $(patsubst roms/%.s, ../bin/roms/%.rom, $(wildcard roms/*.s))

APPS = $(patsubst apps/%.s, ../bin/apps/%.bin, $(wildcard apps/*.s))

all: $(ROMS) $(APPS)

burn: $(BURN)
	minipro -p $(ROMTYPE) -w $<

../bin/roms/%.rom ../labels/%.labels: roms/%.s
	xa -o ../bin/roms/$*.rom $< -w -M -DROMBASE=$(ROMBASE) -l ../labels/$*.labels

../bin/apps/kernel.bin: apps/kernel.s $(wildcard mtos/*.s)
	xa -o $@ $< -w -M -I mtos

../bin/apps/%.bin: apps/%.s ../labels/boot.inc $(wildcard utils/*.s) $(wildcard include/*.s)
	xa -o $@ $< -w -M -I ../labels -I include

../labels/%.inc: ../labels/%.labels
	python3 ../utils/genimports.py $< $@


.PRECIOUS: ../labels/%.inc

