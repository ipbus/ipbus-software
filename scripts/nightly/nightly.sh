#!/bin/bash

#Nightly bootstrap script to update the nightly script automatically

#Update nightly scripts
cd $HOME/nightly
svn up

#Create directory to store logs
PLATFORM=`python -c "import platform;print platform.platform()"`
LOG_DIR=$HOME/www/nightly/${!#}/$PLATFORM/logs
echo $LOG_DIR
mkdir -p $LOG_DIR

#execute nightly
python nightly.py $@ 2>&1 | tee $LOG_DIR/nightly.log
