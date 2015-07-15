GNTS =   \
	RELFILE-MW_SYSOUT.gnt\
	RELFILE-ODCSF0Q.gnt\
	RELFILE-ODCSFU.gnt\
	RELTABLE-ODCSF0.gnt

all:	$(GNTS)

.SUFFIXES:	.pco .cbl .gnt

.pco.cbl:
	procob $(PCCINCLUDE) iname=$*.pco \
                release_cursor=no hold_cursor=yes mode=oracle sqlcheck=syntax common_parser=yes \
                lname=$*.lis oname=$*.cbl declare_section=no picx=charf prefetch=100

.cbl.gnt:
ifneq "$(COB)" "COBOL-IT"
	cob -ug $*.cbl -C "use($(APPHOME)/opt.dir)" -C "list($*.lst)" -C XREF -C SETTINGS
else
	cobc -fthread-safe -m -g -G -fmf-gnt $*.cbl -w -fixed -lcitextfh -t $*.lst -conf=$(APPHOME)/cit.conf
endif

clean:
	-rm -rf c *.lst *.lis *.int *.idy $(GNTS)
