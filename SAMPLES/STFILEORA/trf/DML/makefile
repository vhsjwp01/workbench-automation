ifneq "$(ORACLE_CLIENT)" ""
ORACLE_HOME=$(ORACLE_CLIENT)
endif

GNTS =   \
	DL_ODCSF0.gnt \
	UL_ODCSF0.gnt \
	close_all_files.gnt \
	close_all_files_STFILEORA.gnt \
	dml_locking.gnt \
	mwgetfileinfo.gnt \
	init_all_files.gnt \
	init_all_files_STFILEORA.gnt \
	RM_ODCSF0.gnt

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
	cobc -fthread-safe -m -g -G -fmf-gnt $*.cbl -w -fixed -ffcdreg -lcitextfh -t $*.lst -conf=$(APPHOME)/cit.conf
endif

clean:
	-rm -rf c *.lst *.lis *.int *.idy $(GNTS)
