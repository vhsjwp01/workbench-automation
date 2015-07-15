#!/bin/ksh

JOBS=" \
	DEFVCUST \
	LODVCUST \
	UPDVCUST \
	PRTVCUST \
	CHKVCUST \
"
# Boot Tuxedo application
# tmboot -y

# Submit Jobs
for i in $JOBS
do
	echo JCL/$i.ksh
	artjesadmin -i JCL/$i.ksh
	sleep 2
done
