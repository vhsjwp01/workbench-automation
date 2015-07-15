#!/bin/ksh

mkdir -p sysfile
mkdir -p ${APPHOME}/LOGS/log
mkdir -p ${APPHOME}/LOGS/traces
mkdir -p ${APPHOME}/LOGS/xa
mkdir -p ${APPHOME}/LOGS/sysout

chmod u+w ${APPHOME}/config/resources/*.desc
rm -f ${APPHOME}/config/tux/tuxconfig
tmloadcf -y ${APPHOME}/config/tux/ubbconfig

rm -f ${APPHOME}/sysfile/TLOG
tmadmin -c <<!end
crdl -b 2000 -z ${APPHOME}/sysfile/TLOG -m  ${MASTER_NAME}
!end
