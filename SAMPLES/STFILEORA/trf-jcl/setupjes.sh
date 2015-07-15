#!/bin/ksh

# Create required directories
mkdir -p sysfile
mkdir -p ${MT_ACC_FILEPATH}
mkdir -p ${MT_LOG}
mkdir -p ${SPOOL}
mkdir -p ${TMP}
touch ${MT_ACC_FILEPATH}/AccWait ${MT_ACC_FILEPATH}/AccLock

# Create TUXCONFIG 
rm -f ${MYHOME}/config/tuxconfig
tmloadcf -y ${MYHOME}/config/ubbconfig

# Create TLOG
rm -f ${MYHOME}/sysfile/TLOG
tmadmin -c <<!end
crdl -b 2000 -z ${MYHOME}/sysfile/TLOG -m ${MASTER_NAME}
!end

# Create /Q for JES
qmadmin ${QMCONFIG} <<!
echo
ipcrm -y JES2QSPACE
q
!
rm -f ${QMCONFIG}

qmadmin ${QMCONFIG} <<!
echo
crdl ${QMCONFIG} 0 10000
qspacecreate
JES2QSPACE
${JES_QSPACE_IPCKEY}
5000
50
1000
1000
10000
errque
y
16
qopen JES2QSPACE
qcreate CONV fifo none 2 30 80% 0% ""
qcreate EXEC fifo none 2 30 80% 0% ""
qcreate OUTPUT fifo none 2 30 80% 0% ""
qcreate PURGE fifo none 2 30 80% 0% ""
qcreate HOLD fifo none 2 30 80% 0% ""
qcreate RUNNING fifo none 2 30 80% 0% ""
qcreate PEND fifo none 2 30 80% 0% ""
qcreate DELAYEXEC fifo none 2 30 80% 0% ""
qcreate A priority none 2 30 80% 0% ""
qcreate B priority none 2 30 80% 0% ""
qcreate C priority none 2 30 80% 0% ""
qcreate D priority none 2 30 80% 0% ""
qcreate E priority none 2 30 80% 0% ""
qcreate F priority none 2 30 80% 0% ""
qcreate G priority none 2 30 80% 0% ""
qcreate H priority none 2 30 80% 0% ""
qcreate I priority none 2 30 80% 0% ""
qcreate J priority none 2 30 80% 0% ""
qcreate K priority none 2 30 80% 0% ""
qcreate L priority none 2 30 80% 0% ""
qcreate M priority none 2 30 80% 0% ""
qcreate N priority none 2 30 80% 0% ""
qcreate O priority none 2 30 80% 0% ""
qcreate P priority none 2 30 80% 0% ""
qcreate Q priority none 2 30 80% 0% ""
qcreate R priority none 2 30 80% 0% ""
qcreate S priority none 2 30 80% 0% ""
qcreate T priority none 2 30 80% 0% ""
qcreate U priority none 2 30 80% 0% ""
qcreate V priority none 2 30 80% 0% ""
qcreate W priority none 2 30 80% 0% ""
qcreate X priority none 2 30 80% 0% ""
qcreate Y priority none 2 30 80% 0% ""
qcreate Z priority none 2 30 80% 0% ""
qcreate 0 priority none 2 30 80% 0% ""
qcreate 1 priority none 2 30 80% 0% ""
qcreate 2 priority none 2 30 80% 0% ""
qcreate 3 priority none 2 30 80% 0% ""
qcreate 4 priority none 2 30 80% 0% ""
qcreate 5 priority none 2 30 80% 0% ""
qcreate 6 priority none 2 30 80% 0% ""
qcreate 7 priority none 2 30 80% 0% ""
qcreate 8 priority none 2 30 80% 0% ""
qcreate 9 priority none 2 30 80% 0% ""
qcreate errque fifo none 2 30 80% 0% ""
q
!
