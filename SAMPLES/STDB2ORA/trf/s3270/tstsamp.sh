#!/bin/ksh

if [ `uname -s` != "Linux" -o `uname -m` != "x86_64" ]
then
	echo "Only for Linux x86_64"
	exit 1
fi	

rm -f *.out
s3270 -model 3278-2 < s3270.in > test.out 2>&1
sed 's/[0-9]\{2\}-[0-9]\{2\}-[0-9]\{2\}/DD-MM-YY/g' s3270.out | diff s3270.bmk -

if [ $? -ne 0 ]
then
	echo "Result is different."
else
	echo "Result is consistent."
fi
