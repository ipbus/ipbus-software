#!/bin/bash

#Update nightly scripts
cd $HOME/nightly
svn up

#Create directory to store logs
PLATFORM=`python -c "import platform;print platform.platform()"`
LOG_DIR=www/nightly/$(PLATFORM)/logs
mkdir -p $(LOG_DIR)

#execute nightly
cd nightly
python nightly.py -s cactus.py &> &> $(LOG_DIR)/nightly.log
