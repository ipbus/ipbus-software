#! /bin/bash

if [[ $# != 2 ]]; then 
	echo "Usage: $0 <Total number of lines expected> '<Quoted instruction to perform>'"
else
	echo -n "Evaluating output size"
	PROGRESS_BAR_TOTAL=`eval "$1"`
	echo ": $PROGRESS_BAR_TOTAL lines"
	
	PROGRESS_BAR_CURRENT=0
	PROGRESS_BAR_LAST=0

	echo -ne "|0%                                                                                              100%|\n|"
	$2 | while read x ; do
		PROGRESS_BAR_CURRENT=$(($PROGRESS_BAR_CURRENT+100))
		TEMP=$(($PROGRESS_BAR_CURRENT/$PROGRESS_BAR_TOTAL))
		if [[ $TEMP != $PROGRESS_BAR_LAST ]]; then
			echo -n "#"
			PROGRESS_BAR_LAST=$TEMP
		fi
	done
	echo "|"	
fi	
