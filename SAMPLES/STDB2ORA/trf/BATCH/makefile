ifneq "$(ORACLE_CLIENT)" ""
ORACLE_HOME=$(ORACLE_CLIENT)
endif

GNTS =   \
	RSSBBB00.gnt \
	RSSBBB01.gnt \
	RSSBBB02.gnt

all:	$(COBS) $(GNTS)

.SUFFIXES:	.pco .cbl .gnt

.cbl.gnt:
ifneq "$(COB)" "COBOL-IT"
	cob -ug $*.cbl -C "use($(APPHOME)/opt.dir)" -C "list($*.lst)" -C XREF -C SETTINGS
else
	cobc -fthread-safe -m -g -G -fmf-gnt $*.cbl -Wall -fixed -lcitextfh -t $*.lst -conf=$(APPHOME)/cit.conf
endif 

.pco.gnt:
ifneq "$(COB)" "COBOL-IT"
	cob -ug -k $*.pco -C"preprocess(cobsql) nomakesyn end-c release_cursor==no hold_cursor==yes sqlcheck==synta mode==oracle declare_section==no  picx==charf prefetch==100 common_parser==yes preprocess(cp) SQL sy endp list()" -C"use($(APPHOME)/opt.dir)" -C XREF -C SETTINGS
else
	cobc -fthread-safe -m -g -G -fmf-gnt -preprocess=${APPHOME}/runprocob.sh $*.pco -Wall -fixed -lcitextfh -t $*.lst -conf=$(APPHOME)/cit.conf
endif

clean:
	-rm -rf c *.lst *.lis *.int *.idy $(GNTS) 
