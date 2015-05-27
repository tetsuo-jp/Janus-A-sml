#!/bin/sh

if [ `uname` = "CYGWIN_NT-5.1" ]; then
    EXT=.bat
elif [ `uname` = "Linux" ]; then
    EXT=
fi

#Target script
f=test_sch10000_uncall.script

i=0
while [ $i -lt 3 ];
do
   echo "executing..."$f
   time janus$EXT < $f
   i=`expr $i + 1`
done