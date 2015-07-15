#!/bin/ksh

STRIPCITCOMMENT=NO
if [ "${STRIPCITCOMMENT}" == "YES" ]; then
    grep -v "^      \*#DEBUG" $1 >$1.tmp
else
    cp $1 $1.tmp
fi

procob ${PCCINCLUDE} iname=$1.tmp release_cursor=no hold_cursor=yes mode=oracle sqlcheck=syntax common_parser=yes oname=$2 declare_section=no picx=charf prefetch=100

rm -f $1.tmp

