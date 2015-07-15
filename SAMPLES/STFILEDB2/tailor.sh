#!/bin/ksh

tailor_env()
{
	sed -e "s%##COBDIR##%`echo $COBDIR`%g" \
		-e "s%##DB2DIR##%`echo $DB2DIR`%g" \
		-e "s%##DB2INSTANCE##%`echo $DB2INSTANCE`%g" \
		-e "s%##DB2BASE##%`echo $DB2BASE`%g" \
		-e "s%##DBUSER##%`echo $DBUSER`%g" \
		-e "s%##DBPASSWD##%`echo $DBPASSWD`%g" \
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
		-e "s%##DB2BASE##%`echo $DB2BASE`%g" \
		-e "s%##DBUSER##%`echo $DBUSER`%g" \
		-e "s%##DBPASSWD##%`echo $DBPASSWD`%g" \
		-e "s%##DBPASSWD##%`echo $DBPASSWD`%g" \
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

# read parameters COBDIR DB2DIR DB2INSTANCE DB2BASE DBUSER DBPASSWD TUXDIR ARTDIR
read COBDIR?"Enter COBDIR: "
read DB2DIR?"Enter DB2DIR: "
read DB2INSTANCE?"Enter DB2INSTANCE: "
read DB2BASE?"Enter DB2BASE: "
read DBUSER?"Enter DBUSER: "
read DBPASSWD?"Enter DBPASSWD: "
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
