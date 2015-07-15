ifneq "$(ORACLE_CLIENT)" ""
ORACLE_HOME=$(ORACLE_CLIENT)
endif

GNTS =   \
	RSSBT000.gnt \
	RSSBT001.gnt \
	RSSBT002.gnt \
	RSSBT003.gnt

all:	$(GNTS)

.SUFFIXES:	.pco .cbl .cob .gnt

.cbl.gnt:
	prepro-cics.pl -type_output=orig < $*.cbl > $*.cob
ifneq "$(COB)" "COBOL-IT"
	cob -ug $*.cob -C "use($(APPHOME)/opt.dir)" -C "list($*.lst)" -C XREF -C SETTINGS
else
	cobc -fthread-safe -m -g -G -fmf-gnt $*.cob -Wall -fixed -lcitextfh -t $*.lst -conf=$(APPHOME)/cit.conf
endif
	@rm -f $*.cob 

.pco.gnt:
	prepro-cics.pl -type_output=orig < $*.pco > $*.cob
ifneq "$(COB)" "COBOL-IT"
	cob -ug -k $*.cob -C"preprocess(cobsql) nomakesyn end-c release_cursor==no hold_cursor==yes sqlcheck==synta mode==oracle declare_section==no  picx==charf prefetch==100 common_parser==yes preprocess(cp) SQL sy endp list()" -C"use($(APPHOME)/opt.dir)" -C XREF -C SETTINGS
else
	cobc -fthread-safe -m -g -G -fmf-gnt -preprocess=${APPHOME}/runprocob.sh $*.cob -Wall -fixed -lcitextfh -t $*.lst -conf=$(APPHOME)/cit.conf
endif
	@rm -f $*.cob

clean:
	-rm -rf c *.lst *.lis *.int *.idy *.cob $(GNTS) 
