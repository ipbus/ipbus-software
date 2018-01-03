#!/bin/bash

# 0. Check number of arguments
if [ $# -ne 1 ]; then
    echo "Incorrect usage"
    echo "`basename $0` <project name suffix>"
    exit 1
fi


# 1. Look for doxygen command
if [ -n "${DOXYGEN_BIN_DIR}" ]; then
    DOXYGEN_BIN=${DOXYGEN_BIN_DIR}/doxygen
    if [ -x "${DOXYGEN_BIN}" ]; then
        echo "Cannot find doxygen exe under directory '${DOXYGEN_DIR}'. Aborting"
        exit 1
    fi 
else
	command -v doxygen >/dev/null 2>&1 || { echo "Cannot find doxygen command. Aborting." >&2; exit 1; }
    DOXYGEN_BIN="doxygen"
fi
echo "Using doxygen command: ${DOXYGEN_BIN}"
echo "   (version `doxygen --version`)"


# 2. Set environment variables for doxygen
export REPO_BASE_DIR=$( readlink -f $(dirname $BASH_SOURCE)/../../ )

export DOXYGEN_PROJECT_NAME="&mu;HAL $1" # '&mu;HAL (nightly)'
export DOXYGEN_PROJECT_LOGO="${REPO_BASE_DIR}/scripts/doxygen/cactus_logo.png"
export DOXYGEN_HTML_HEADER="${REPO_BASE_DIR}/scripts/doxygen/header.html"
export DOXYGEN_HTML_EXTRA_FILES="${REPO_BASE_DIR}/scripts/doxygen/favicon.ico"
export DOXYGEN_MAINPAGE="${REPO_BASE_DIR}/uhal/README.md"
export DOXYGEN_UHAL_BASE_DIR="${REPO_BASE_DIR}/uhal"
export DOXYGEN_EXCLUDE_PATTERNS=''
export DOXYGEN_OUTPUT=/tmp/api_uhal
DOXYGEN_STRIP_FROM_INC_PATH=
for PACKAGE_PATH in log grammars uhal pycohal tests; do
    DOXYGEN_STRIP_FROM_INC_PATH+="${REPO_BASE_DIR}/uhal/${PACKAGE_PATH}/include "
done
export DOXYGEN_STRIP_FROM_INC_PATH

echo "Input parameters ..."
echo "  DOXYGEN_PROJECT_NAME        = ${DOXYGEN_PROJECT_NAME}"
echo "  DOXYGEN_PROJECT_LOGO        = ${DOXYGEN_PROJECT_LOGO}"
echo "  DOXYGEN_HTML_HEADER         = ${DOXYGEN_HTML_HEADER}"
echo "  DOXYGEN_HTML_EXTRA_FILES    = ${DOXYGEN_HTML_EXTRA_FILES}"
echo "  DOXYGEN_MAINPAGE            = ${DOXYGEN_MAINPAGE}"
echo "  DOXYGEN_UHAL_BASE_DIR       = ${DOXYGEN_UHAL_BASE_DIR}"
echo "  DOXYGEN_EXCLUDE_PATTERNS    = ${DOXYGEN_EXCLUDE_PATTERNS}"
echo "  DOXYGEN_OUTPUT              = ${DOXYGEN_OUTPUT}"
echo "  DOXYGEN_STRIP_FROM_INC_PATH = ${DOXYGEN_STRIP_FROM_INC_PATH}"

echo "Cleaning up target directory, ${DOXYGEN_OUTPUT}"
rm -rf ${DOXYGEN_OUTPUT}/html


# 3. Main command
${DOXYGEN_BIN} ${REPO_BASE_DIR}/scripts/doxygen/ipbus-sw.doxy


# 4. Copy output to ${TARGET_DOXY_DIR} if specified
if [ -n "${TARGET_DOXY_DIR}" ]; then
    echo "Copying output to TARGET_DOXY_DIR=${TARGET_DOXY_DIR} ..."
    echo "(Removing old copy first)"
    rm -r ${TARGET_DOXY_DIR}
    mkdir -p ${TARGET_DOXY_DIR}
    cp -a ${DOXYGEN_OUTPUT}/html ${TARGET_DOXY_DIR}
    echo "Done"
else
    echo "doxygen output stored in ${DOXYGEN_OUTPUT}"
fi