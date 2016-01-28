#!/bin/bash
for filename in packages/*; do

 prefix=`echo $(basename $filename) | sed "s/pacakges\///"| sed "s/\(.*\)-.*/\1/"`
 gpled=`grep $prefix GPLed-projects.txt`
 echo "gpled:" $gpled "prefix:" $prefix 
 if [ ! -z "$gpled" ]; then
      echo "keeping" $filename >> package-status.txt
 else
      echo "deleting" $filename >> package-status.txt
      rm $filename
 fi

 done
