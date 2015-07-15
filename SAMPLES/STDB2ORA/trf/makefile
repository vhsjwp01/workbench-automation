all: Batch Cics Dml Map Reload

Batch:
	@( \
	echo " Compil BATCH ..." ;\
	cd BATCH ;\
	make all ;\
	)

Cics:
	@( \
	echo " Compil CICS ..." ;\
	cd CICS ;\
	make all ;\
	)

Dml:
	@( \
	echo " Compil DML ..." ;\
	cd DML ;\
	make all ;\
	)

Map:
	@( \
	echo " Compil MAP ..." ;\
	cd MAP ;\
	make all ;\
	)

Reload: file rdbms

file:
	@( \
	echo " Compil file ..." ;\
	cd reload/file/STDB2ORA ;\
	make all ;\
	)

rdbms:
	@( \
	echo " Compil rdbms ..." ;\
	cd reload/rdbms/PJ01DB2/src ;\
	make all ;\
	)

