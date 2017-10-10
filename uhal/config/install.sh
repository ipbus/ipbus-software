#!/bin/bash
#
# Written by XDAQ Team (xdaq.cern.ch)
#
# install.sh <source> <destination> <permission> RPM_BUILD_ROOT PACKAGEDIR PACKAGENAME VERSION INCLUDEPREFIX INCLUDEDIRS
#
SRC=$1
DST=$2
PERM=$3
RPM_BUILD_ROOT=$4
PKGDIR=$5
NAME=$6
VERSION=$7
INCLUDEPREFIX=$8
INCLUDEDIRS=$9

# Symbolic link
if [ -h "$SRC" ]; then
  DSTPARENT=`echo $DST | sed -r 's/^(.*\/).*$/\1/'`
  #mkdir -p ${DSTPARENT}
  cp -d $SRC $RPM_BUILD_ROOT/$DST
else
  # Regular file
  if [ -f "$SRC" ]; then
    install -D -m $PERM $SRC $RPM_BUILD_ROOT/$DST

    MIME=`file -ib $SRC`
    REGEX="application/(x-executable|x-sharedlib).*"
    if [[ "$MIME" =~ $REGEX ]]; then
#      set -x
      # If there is already a debug file copy it instead of creating a new one
      if [ -e "$SRC.debug" ]; then
        install -D -m $PERM $SRC.debug $RPM_BUILD_ROOT/usr/lib/debug$DST.debug
      else
        install -D -m $PERM $SRC $RPM_BUILD_ROOT/usr/lib/debug$DST.debug
        objcopy --only-keep-debug $RPM_BUILD_ROOT/usr/lib/debug${DST}.debug
      fi
      # strip normal binary
      objcopy -g $RPM_BUILD_ROOT/${DST}

      # rewrite source directories
      /usr/lib/rpm/debugedit -b ${PKGDIR} -d /usr/src/debug/${NAME}-${VERSION} -l ${PKGDIR}/rpm/debug.source $RPM_BUILD_ROOT/usr/lib/debug$DST.debug

      # relocate all include files to /opt/xdaq/include
      for prefix in $INCLUDEDIRS; do
        /usr/lib/rpm/debugedit -b $prefix -d ${INCLUDEPREFIX} $RPM_BUILD_ROOT/usr/lib/debug$DST.debug
      done

      objcopy --remove-section .gnu_debuglink --add-gnu-debuglink=$RPM_BUILD_ROOT/usr/lib/debug${DST}.debug $RPM_BUILD_ROOT/${DST}
#      set +x
    fi
  fi
fi
