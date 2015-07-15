ifneq "$(ORACLE_CLIENT)" ""
ORACLE_HOME=$(ORACLE_CLIENT)
endif

GNTS =   \
	DL_ODCSF0.gnt \
	UL_ODCSF0.gnt \
	close_all_files.gnt \
	close_all_files_STFILEDB2.gnt \
	dml_locking.gnt \
	mwgetfileinfo.gnt \
	init_all_files.gnt \
	init_all_files_STFILEDB2.gnt \
	RM_ODCSF0.gnt

all:	$(GNTS)

.SUFFIXES:	.sqb .cbl .gnt

.sqb.cbl:
	db2 connect to $(DB2BASE) user $(DBUSER) using $(DBPASSWD)
	db2 prep $*.sqb BINDFILE
#In db2, maximum length of package name is 8 bytes, If two *.sqb files have same 8 leading charaters in its name, db2 would raise error(SQLCODE -818) when executing program.
#Please use following db2 prep options to avoid db2 error(SQLCODE -818).
#	db2 prep $*.sqb BINDFILE PACKAGE USING '$(notdir $<)' VERSION $(firstword $(shell echo `uname -n`:$(CURDIR)/'$<'|md5sum ))

.cbl.gnt:
ifneq "$(COB)" "COBOL-IT"
	cob -ug $*.cbl -C "use($(APPHOME)/opt.dir)" -C "list($*.lst)" -C XREF -C SETTINGS
else
	cobc -fthread-safe -m -g -G -fmf-gnt $*.cbl -w -fixed -ffcdreg -lcitextfh -t $*.lst -conf=$(APPHOME)/cit.conf
endif

clean:
	-rm -rf c *.lst *.lis *.int *.idy $(GNTS)
