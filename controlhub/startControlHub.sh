#!/bin/bash

if [ ! -n "$CONTROL_HUB" ]; then
  echo "Error: the environment variable CONTROL_HUB has not been set!"
  echo "Please set the CONTROL_HUB environment variable to your ControlHub install directory!"
  exit 1
fi

if [ ! -n "$1" ]; then
  echo "Error: you must specifiy a device address file location!"
  echo
  echo "Usage:"
  echo "./startControlHub.sh deviceAddrFile"
  echo
  echo "See here for an example address file:  test/config/DeviceAddressFile.erl"
  exit 2
fi

if [ ! -f "$1" ]; then
  echo "Error: the device address file: '$1' does not exist!" 
  exit 3
fi

# Assuming the CONTROL_HUB environment var has been set correctly, 
# there should be an ebin directory within the CONTROL_HUB directory.
code_path=$CONTROL_HUB/ebin

if [ ! -d "$code_path" ]; then
  echo "Error: there is no 'ebin' directory within the '$CONTROL_HUB' directory!"
  echo "Have you compiled the Control Hub code?"
  exit 4
fi

erl -noshell -pz $code_path -s startup cmd_line_start $1
