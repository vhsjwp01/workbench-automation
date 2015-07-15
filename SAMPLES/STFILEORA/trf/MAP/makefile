MAPS = \
	RSSAM00.mpdef \
	RSSAM01.mpdef \
	RSSAM02.mpdef \
	RSSAM03.mpdef

CPYDIR = ../COPY

all:    $(MAPS)

.SUFFIXES:	.bms .mpdef
.bms.mpdef:
	tcxmapgen $*.bms

clean:
	rm $(MAPS) *.cpy

inst:
	cp *.cpy  $(CPYDIR)
