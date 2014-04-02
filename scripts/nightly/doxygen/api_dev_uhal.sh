#!/bin/bash

export DOXYGEN_HOME=${HOME}/doxygen/doxygen-1.8.6/
export CACTUS_SANDBOX=/build/cactus/cactus/trunk
#export CACTUS_SANDBOX=${HOME}/doxygen/test
export API_DIR=/afs/cern.ch/user/c/cactus/www/release/2.3/api
export DOXYGEN_OUTPUT=/tmp/

cd ${HOME}/doxygen
echo "Cleaning up target directory"
rm -r ${DOXYGEN_OUTPUT}/html

DOXYGEN_MAINPAGE="${CACTUS_SANDBOX}/cactuscore/uhal/README.md"
DOXYGEN_INPUTS="${CACTUS_SANDBOX}/cactuscore/uhal "
DOXYGEN_PROJECT_NAME='&mu;HAL (v2.3.0)'

echo DOXYGEN_INPUTS=${DOXYGEN_INPUTS}
export DOXYGEN_MAINPAGE DOXYGEN_INPUTS DOXYGEN_PROJECT_NAME
${DOXYGEN_HOME}/bin/doxygen cactus-v2.doxy


echo "Removing old APIs"
rm -r ${API_DIR}/html_dev_uhal

echo "Uploading..."
mkdir -p ${API_DIR}
cp -a ${DOXYGEN_OUTPUT}/html ${API_DIR}/html_dev_uhal
echo "Done"
