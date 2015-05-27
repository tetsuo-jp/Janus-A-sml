#!/bin/sh

if [ `uname` = "CYGWIN_NT-5.1" ]; then
    EXT=.bat
else
    EXT=
fi

for f in *.script
do
   echo "executing..."$f
   time janus$EXT < $f
done
