#!/bin/bash
for filename in recipes/*; do

   result=`grep -l "github" $filename | xargs grep ":repo" | sed "s/.*:repo\s*\"\(.*\)/\1/" | sed "s/\".*//" | sed "s/.*\///" `
   if [ ! -z "$result" ]; then # not empty
	
	gpled=`grep -Fx $result GPLed.txt`
	if [ -z "$gpled" ]; then
		echo $filename >> recipes-deleted.txt
		rm $filename 
	else
		echo $filename >> recipes-kept.txt

	fi	
    else
		echo $filename >> recipes-deleted.txt
		rm $filename
   fi 

done
