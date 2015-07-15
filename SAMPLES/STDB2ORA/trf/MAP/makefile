MAPS = \
	RSSBM00.mpdef \
	RSSBM01.mpdef \
	RSSBM02.mpdef \
	RSSBM03.mpdef

CPYDIR = ../COPY

all:    $(MAPS)

.SUFFIXES:	.bms .mpdef
.bms.mpdef:
	tcxmapgen $*.bms

clean:
	rm $(MAPS) *.cpy

inst:
	cp *.cpy  $(CPYDIR)
