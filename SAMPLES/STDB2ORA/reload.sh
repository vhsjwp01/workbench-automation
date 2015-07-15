#!/bin/ksh

export MYHOME=${PWD}
export DATA_SOURCE=${MYHOME}/data_source
export DATA=${MYHOME}/data
export MT_LOG=${MYHOME}/LOGS
export DDL=${MYHOME}/trf/SQL/file

export MT_ACC_FILEPATH=${MYHOME}/acc

mkdir -p ${DATA}
mkdir -p ${MT_LOG}
mkdir -p ${MT_ACC_FILEPATH}
touch ${MT_ACC_FILEPATH}/AccLock
touch ${MT_ACC_FILEPATH}/AccWait

export MT_KSH=${PDKSH}
export PATH=${ARTDIR}/Batch_RT/ejr:${PATH}

export DD_ENTREE=${DATA_SOURCE}/PJ01AAA.S2.QSAM.CUSTOMER
export DD_SORTIE=${DATA}/PJ01AAA.S2.QSAM.CUSTOMER

export DATA_TRANSCODE=${MYHOME}/trf/reload/file/STDB2ORA

${PDKSH} ${DATA_TRANSCODE}/loadfile-ODCSF0.ksh

export DD_ENTREE=${DATA_SOURCE}/PJ01AAA.S2.QSAM.CUSTOMER.UPDATE
export DD_SORTIE=${DATA}/PJ01AAA.S2.QSAM.CUSTOMER.UPDATE

${PDKSH} trf/reload/file/STDB2ORA/loadfile-ODCSFU.ksh

sqlplus ${DBCONNECT} <<EOF
start MWDB2ORA_CONST.sql
start MWDB2ORA.plb
quit
EOF
