
cobpaths=" \
	reload/file/STFILEORA/ \
	BATCH/ \
	CICS/ \
	DML/ \
	MAP/ \
"

for i in ${cobpaths}
do
	cd ${APPHOME}/$i
	gmake clean
	gmake
	if [ $? -ne 0 ]
	then
		echo "failed, please see $i.log"
		exit 1
	fi
done

cd ${PROJECT}
