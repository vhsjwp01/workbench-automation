GNTS =   \
	close_all_files.gnt \
	init_all_files_STDB2ORA.gnt \
	close_all_files_STDB2ORA.gnt \
	mwgetfileinfo.gnt \
	dml_locking.gnt \
	init_all_files.gnt

all:	$(GNTS)

.SUFFIXES:	.cbl .gnt

.cbl.gnt:
ifneq "$(COB)" "COBOL-IT"
	cob -ug $*.cbl -C "use($(APPHOME)/opt.dir)" -C "list($*.lst)" -C XREF -C SETTINGS
else
	cobc -fthread-safe -m -g -G -fmf-gnt $*.cbl -w -fixed -ffcdreg -lcitextfh -t $*.lst -conf=$(APPHOME)/cit.conf
endif

clean:
	-rm -rf c *.lst *.lis *.int *.idy $(GNTS)
