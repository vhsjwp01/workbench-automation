GNTS =   \
	RSSAT000.gnt \
	RSSAT001.gnt \
	RSSAT002.gnt \
	RSSAT003.gnt

all:	$(GNTS)

.SUFFIXES:	.cbl .cob .gnt

.cbl.cob:
	prepro-cics.pl -type_output=orig < $*.cbl > $*.cob

.cob.gnt:
ifneq "$(COB)" "COBOL-IT"
	cob -ug $*.cob -C "use($(APPHOME)/opt.dir)" -C "list($*.lst)" -C XREF -C SETTINGS
else
	cobc -fthread-safe -m -g -G -fmf-gnt $*.cob -Wall -fixed -lcitextfh -t $*.lst -conf=$(APPHOME)/cit.conf
endif

clean:
	-rm -rf c *.lst *.lis *.int *.idy $(GNTS)
