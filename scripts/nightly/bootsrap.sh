#!/bin/bash
cd $HOME

#Create directory to store logs
PLATFORM=`python -c "import platform;print platform.platform()"`
LOG_DIR=www/nightly/$(PLATFORM)/logs
mkdir -p www/nightly/$(PLATFORM)/logs

#checkout nightly scripts
rm -rf nightly
svn co svn+ssh://svn.cern.ch/reps/cactus/trunk/scripts/nightly

#execute nightly
cd nightly
python nightly.py --log=$(LOG_DIR)/nightly.log

cactus.py 