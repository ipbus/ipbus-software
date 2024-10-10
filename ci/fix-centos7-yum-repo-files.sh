#!/bin/bash

# N.B. This script was copied from the HERD repo

# In case these repositories stop working, potential alternatives are:
#   * http://archive.kernel.org/centos-vault/
#   * http://linuxsoft.cern.ch/centos-vault/
#   * http://mirror.nsc.liu.se/centos-store/
#   * http://linuxsoft.cern.ch/centos-altarch
#
# Note: CERN is extending CentOS7 support until mid-2026: https://linux.web.cern.ch/centos7/els7/
# Also see brief update from ITUM-42 - slide 4 of https://indico.cern.ch/event/1397307/contributions/5873112/attachments/2887690/5061458/ITUM-42,%20Technical%20Service%20Updates.pdf


OS_NAME=$(cat /etc/redhat-release)
if [[ $OS_NAME =~ ^CentOS.Linux.release.7.* ]]; then
  echo "fix-yum-repo-files.sh: CentOS7 release detected"

  if [[ "$(uname -m)" == "x86_64" ]]; then
    echo "Patching YUM repo files (AMD64 detected)"
    sed -i 's|^#.*baseurl=http://mirror.centos.org|baseurl=http://vault.centos.org|g' /etc/yum.repos.d/*.repo
  elif [[ "$(uname -m)" == "aarch64" ]]; then
    echo "Patching YUM repo files (ARM64 detected)"
    sed -i 's|^#.*baseurl=http://mirror.centos.org/centos|baseurl=http://vault.centos.org/altarch|g' /etc/yum.repos.d/CentOS-SCLo-scl*.repo
    sed -i 's|^#.*baseurl=http://mirror.centos.org|baseurl=http://vault.centos.org|g' /etc/yum.repos.d/*.repo
  elif [[ "$(uname -m)" == "armv7l" ]]; then
    echo "Patching YUM repo files (ARMv7 detected)"
    sed -i 's|^#.*baseurl=http://mirror.centos.org|baseurl=http://vault.centos.org|g' /etc/yum.repos.d/*.repo
    yum-config-manager --disable epel
  fi

  sed -i s/^mirrorlist=http/#mirrorlist=http/g /etc/yum.repos.d/*.repo
fi
