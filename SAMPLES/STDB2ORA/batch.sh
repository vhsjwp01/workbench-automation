#!/bin/ksh

export MT_KSH=${PDKSH}
export MT_ROOT=${ARTDIR}/Batch_RT/ejr
export PATH=${MT_ROOT}:${PATH}

export MYHOME=${PWD}
export MT_TMP=${MYHOME}/LOGS/tmp
export MT_LOG=${MYHOME}/LOGS/log
export MT_ACC_FILEPATH=${MYHOME}/acc
export SPOOL=${MYHOME}/LOGS/sysout
export DATA=${MYHOME}/data
export DDL=${MYHOME}/trf/SQL/file
export MT_DB_LOGIN=${DBCONNECT}
export TMP=$MT_TMP
export TMPPROJECT=$MT_TMP

mkdir -p ${MT_ACC_FILEPATH}
mkdir -p ${MT_LOG}
mkdir -p ${SPOOL}
mkdir -p ${TMP}
touch ${MT_ACC_FILEPATH}/AccWait ${MT_ACC_FILEPATH}/AccLock

SQLs=" \
	TABLE-ODCSF0 \
	INDEX-ODCSF0 \
	CONSTRAINT-ODCSF0 \
"

JCLs=" \
	LODCUST \
	UPDCUST \
	PRTCUST \
"
for i in ${SQLs}
do
	echo sqlplus trf/SQL/rdbms/PJ01DB2/$i.sql
	sqlplus ${MT_DB_LOGIN} < trf/SQL/rdbms/PJ01DB2/$i.sql
done

for i in ${JCLs}
do
	echo ${MT_ROOT}/EJR trf-jcl/JCL/$i.ksh
	${MT_ROOT}/EJR trf-jcl/JCL/$i.ksh
done
