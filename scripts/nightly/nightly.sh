#!/bin/bash

#Nightly bootstrap script to update the nightly script automatically

#Update nightly scripts
cd $HOME
svn up nightly

#Create directory to store logs
PLATFORM=`python -c "import platform;print platform.platform()"`
CONF_FILE_NAME=`basename $1`
LOG_DIR=$HOME/www/nightly/$CONF_FILE_NAME/$PLATFORM/logs
echo $LOG_DIR
mkdir -p $LOG_DIR

#execute nightly
python nightly/nightly.py $@ 2>&1 | tee $LOG_DIR/nightly.log
