#!/bin/ksh

export MYHOME=${PWD}
export DATA_SOURCE=${MYHOME}/data_source
export DATA=${MYHOME}/data
export RDB_DATA=${MYHOME}/trf/data
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

export MT_DB_LOGIN=${DBCONNECT}

export DD_ENTREE=${DATA_SOURCE}/PJ01AAA.SS.QSAM.CUSTOMER
export DD_SORTIE=${DATA}/PJ01AAA.SS.QSAM.CUSTOMER

export DATA_TRANSCODE=${MYHOME}/trf/reload/file/STFILEDB2
${PDKSH} ${DATA_TRANSCODE}/loadfile-ODCSF0Q.ksh

export DD_ENTREE=${DATA_SOURCE}/PJ01AAA.SS.QSAM.CUSTOMER.UPDATE 
export DD_SORTIE=${DATA}/PJ01AAA.SS.QSAM.CUSTOMER.UPDATE

${PDKSH} ${DATA_TRANSCODE}/loadfile-ODCSFU.ksh

${PDKSH} trf/reload/file/STFILEDB2/loadtable-ODCSF0.ksh -d

#cp trf/data/PJ01AAA.SS.VSAM.CUSTOMER.rdb ${DATA}
