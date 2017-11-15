#!/bin/sh
list=`echo $1|sed  -e 's/Musics\///'`
for var in $list
do
    mpc add $var
done
