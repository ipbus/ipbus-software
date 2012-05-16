import os
from os import environ
from os.path import join
import socket

####VARIABLES
BUILD_HOME          = '/build/cactus'
RELEASE_DIR         = "/afs/cern.ch/user/c/cactus/www/nightly/RPMS"
XDAQ_REPO_FILE      = "/afs/cern.ch/user/c/cactus/nightly/cactus_xdaq.repo"
YUMGROUP_FILE       = "/afs/cern.ch/project/l1ts/nightly/cactus_yumgroups.xml"
CACTUS_REPO_FILE  = "/afs/cern.ch/project/l1ts/nightly/cactus.repo"

####VARIABLES: analysis of logs
TITLE             = "CACTUS Nightlies"
FROM_EMAIL        = "cactus.service@cern.ch"
TO_EMAIL          = "cms-cactus@cern.ch"
WEB_URL           = "http://cern.ch/cactus/nightly/"
WEB_DIR           = "/afs/cern.ch/user/c/cactus/www/nightly"
LOG_DIR           = "/afs/cern.ch/user/c/cactus/www/nightly/logs"
#The log file name and path should be the same than in the one in the acrontab
LOG_FILE          = "/afs/cern.ch/user/c/cactus/www/nightly/logs/nightly.log"

ERROR_LIST        = ['TEST FAILED, ',
                     'error: ',
                     'RPM build errors',
                     'collect2: ld returned',
                     ' ERROR ',
                     ' Error ',
                     'FAILED']

IGNORE_ERROR_LIST = []

TEST_PASSED_LIST  = ['TEST OK, ']


####ENVIRONMENT
#environ['CVSROOT'] = ':pserver:anonymous:98passwd@isscvs.cern.ch:/local/reps/tridas'

####COMMANDS
UNINSTALL_CMDS = ["sudo yum -y remove scons bzip2-devel erlang",
                  "sudo yum -y groupremove extern_coretools coretools extern_powerpack powerpack database_worksuite general_worksuite hardware_worksuite",
                  "rm -rf %s" % BUILD_HOME,
                  "mkdir -p %s" % BUILD_HOME]

ENVIRONMENT_CMDS = ["env"]

DEPENDENCIES_CMDS = ["sudo yum -y install scons bzip2-devel erlang",
                     "sudo cp %s %s" % (XDAQ_REPO_FILE,"/etc/yum.repos.d/xdaq.repo"),
                     "sudo yum -y install boost boost-devel e2fsprogs-devel curl curl-devel"]

CHECKOUT = ["cd %s" % BUILD_HOME,
            "svn co svn+ssh://svn.cern.ch/reps/cactus/trunk"]

CHECKOUT_CMDS = ["rm -rf %s" % BUILD_HOME,
                 "mkdir -p %s" % BUILD_HOME,
                 ";".join(CHECKOUT)]


BUILD_CMDS = ["cd %s" % join(BUILD_HOME,"trunk")]

RELEASE_CMDS = ["rm -rf %s" % RELEASE_DIR,
                "mkdir -p %s" % RELEASE_DIR,
                "cp %s %s" % (YUMGROUP_FILE,join(RELEASE_DIR,"yumgroups.xml")),
                "find %s -name '*.rpm' -exec cp {} %s \;" % (BUILD_HOME,RELEASE_DIR),
                "cd %s;createrepo -vg yumgroups.xml ." % RELEASE_DIR]

INSTALL_CMDS = ["sudo cp %s %s" % (CACTUS_REPO_FILE,"/etc/yum.repos.d/."),
                "sudo yum clean all",
                "sudo yum -y groupinstall cactus"]

TEST_CMDS = []

REPORT_CMDS = ["python $HOME/nightly/nanalyzer.py cactus"]

             
