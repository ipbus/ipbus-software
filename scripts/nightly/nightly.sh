#!/bin/bash

#Nightly bootstrap script to update the nightly script automatically

#Update nightly scripts
cd $HOME/nightly
svn up

#execute nightly
python nightly.py $@ 2>&1 | tee /tmp/nightly.log
