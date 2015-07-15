#!/bin/ksh

tailor_env()
{
	sed -e "s%##COBDIR##%`echo $COBDIR`%g" \
		-e "s%##ORACLE_HOME##%`echo $ORACLE_HOME`%g" \
		-e "s%##ORACLE_SID##%`echo $ORACLE_SID`%g" \
		-e "s%##DBUSER##%`echo $DBUSER`%g" \
		-e "s%##DBPASSWORD##%`echo $DBPASSWORD`%g" \
		-e "s%##TUXDIR##%`echo $TUXDIR`%g" \
		-e "s%##ARTDIR##%`echo $ARTDIR`%g" \
		-e "s%##PDKSH##%`echo $PDKSH`%g" \
		-e "s%##KIX_CWA_IPCKEY##%`expr $IPCKEY + 1`%g" \
		-e "s%##KIX_QSPACE_IPCKEY##%`expr $IPCKEY + 2`%g" \
		-e "s%##KIX_TD_QSPACE_IPCKEY##%`expr $IPCKEY + 3`%g" \
		$ENVFILE.template > $ENVFILE
}

tailor_ubb()
{
	sed -e "s%##APPDIR##%`echo $PWD/trf`%g" \
		-e "s%##TUXDIR##%`echo $TUXDIR`%g" \
		-e "s%##MACHINE##%`uname -n`%g" \
		-e "s%##IPCKEY##%`echo $IPCKEY`%g" \
		-e "s%##ORACLE_SID##%`echo $ORACLE_SID`%g" \
		-e "s%##DBUSER##%`echo $DBUSER`%g" \
		-e "s%##DBPASSWORD##%`echo $DBPASSWORD`%g" \
		-e "s%##DBPASSWORD##%`echo $DBPASSWORD`%g" \
		-e "s%##NETWORKADDR##%`echo $IPCKEY`%g" \
		-e "s%##PRIVATEADDR##%`expr $IPCKEY + 1`%g" \
		$UBBCONFIG.template > $UBBCONFIG
}

tailor_s3270in()
{
	if [ ! -f $S3270IN.template ]
	then
		return
	fi

	sed -e "s%##MACHINE##%`uname -n`%g" \
		-e "s%##NETWORKADDR##%`echo $IPCKEY`%g" \
		$S3270IN.template > $S3270IN
}

## main ##

if [ `uname -s` != "Windows_NT" ]
then
	ID=`whoami`
else
	ID=`id -nu`
fi

EXISTING_IPCS=`ipcs | grep $ID | wc -l`
if [ $EXISTING_IPCS -gt 0 ];
then
	echo "\nWarning: TUXEDO applications may active under $ID.\n"
	ipcs | grep $ID
#	exit 1
fi

# read parameters COBDIR ORACLE_HOME ORACLE_SID DBUSER DBPASSWORD TUXDIR ARTDIR
read COBDIR?"Enter COBDIR: "
read ORACLE_HOME?"Enter ORACLE_HOME: "
read ORACLE_SID?"Enter ORACLE_SID: "
read DBUSER?"Enter DBUSER: "
read DBPASSWORD?"Enter DBPASSWORD: "
read TUXDIR?"Enter TUXDIR: "
read ARTDIR?"Enter ARTDIR: "
read PDKSH?"Enter PDKSH: "

IPCKEY=0
while [ $IPCKEY -lt 32767 ]
do
	IPCKEY=$RANDOM
	IPCKEY=`expr $IPCKEY + 32767`
	IPCKEY=`expr $IPCKEY % 49150`
done

ENVFILE=setenv
UBBCONFIG=trf/config/tux/ubbconfig
S3270IN=trf/s3270/s3270.in

tailor_env
tailor_ubb
tailor_s3270in
