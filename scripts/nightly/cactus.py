from os import environ
from os.path import join

####VARIABLES
BUILD_HOME          = '/build/cactus'
RELEASE_DIR         = "/afs/cern.ch/user/c/cactus/www/nightly/RPMS"
XDAQ_REPO_FILE      = "/afs/cern.ch/user/c/cactus/nightly/cactus_xdaq.repo"
YUMGROUP_FILE       = "/afs/cern.ch/user/c/cactus/nightly/cactus_yumgroups.xml"
CACTUS_REPO_FILE    = "/afs/cern.ch/user/c/cactus/nightly/cactus.repo"
INSTALL_PREFIX      = "/opt/cactus"

####VARIABLES: analysis of logs
TITLE             = "CACTUS Nightlies"
FROM_EMAIL        = "cactus.service@cern.ch"
TO_EMAIL          = "cms-cactus@cern.ch"
WEB_URL           = "http://cern.ch/cactus/nightly/"
WEB_DIR           = "/afs/cern.ch/user/c/cactus/www/nightly"
LOG_DIR           = join(WEB_DIR,"logs")
#The log file name and path should be the same than in the one in the acrontab
LOG_FILE          = join(LOG_DIR,"nightly.log")

#nanalyzer.py variables
ERROR_LIST        = ['TEST FAILED, ',
                     'error: ',
                     'RPM build errors',
                     'collect2: ld returned',
                     ' ERROR ',
                     ' Error ',
                     'FAILED']

IGNORE_ERROR_LIST = []

TEST_PASSED_LIST  = []


####ENVIRONMENT
environ["LD_LIBRARY_PATH"] = join(INSTALL_PREFIX,"lib") + ":" + environ.get("LD_LIBARY_PATH","")

####COMMANDS
UNINSTALL_CMDS = ["rm -rf %s" % BUILD_HOME,
                  "mkdir -p %s" % BUILD_HOME,
                  "sudo yum -y groupremove cactus",
                  "rpm -qa | grep cactus- | xargs sudo rpm -ev &> /dev/null",
                  "pkill -f DummyHardwareTcp.exe &> /dev/null",
                  "pkill -f DummyHardwareUdp.exe &> /dev/null",
                  "pkill -f \"cactus.*erlang\" &> /dev/null",
                  "pkill -f \"cactus.*controlhub\" &> /dev/null"]

ENVIRONMENT_CMDS = ["env"]

DEPENDENCIES_CMDS = ["sudo yum -y install bzip2-devel ncurses-devel python-devel"]

CHECKOUT = ["cd %s" % BUILD_HOME,
            "svn co svn+ssh://svn.cern.ch/reps/cactus/trunk"]

CHECKOUT_CMDS = ["rm -rf %s" % BUILD_HOME,
                 "mkdir -p %s" % BUILD_HOME,
                 ";".join(CHECKOUT)]


BUILD_CMDS = ["cd %s;make -k" % join(BUILD_HOME,"trunk"),
              "cd %s;make -k rpm" % join(BUILD_HOME,"trunk")]

RELEASE_CMDS = ["rm -rf %s" % RELEASE_DIR,
                "mkdir -p %s" % RELEASE_DIR,
                "cp %s %s" % (YUMGROUP_FILE,join(RELEASE_DIR,"yumgroups.xml")),
                "find %s -name '*.rpm' -exec cp {} %s \;" % (BUILD_HOME,RELEASE_DIR),
                "cd %s;createrepo -vg yumgroups.xml ." % RELEASE_DIR]

INSTALL_CMDS = ["sudo cp %s %s" % (CACTUS_REPO_FILE,"/etc/yum.repos.d/."),
                "sudo yum clean all",
                "sudo yum -y groupinstall cactus",
                "cd /build/cactus; doxygen %s" % join(BUILD_HOME,"trunk/scripts/nightly/cactus_Doxyfile"),
                "mkdir -p %s" % join(WEB_DIR,"api"),
                "cd /build/cactus;mv html %s" % join(WEB_DIR,"api/.")]

TEST_CMDS = []
## "%s 50001 &> %s &" % (join(INSTALL_PREFIX,"DummyHardwareUdp.exe"),
##                                    join("var/log","DummyHardwareUdp.exe.log")),
##              "%s 50002 &> %s &" % (join(INSTALL_PREFIX,"DummyHardwareTcp.exe"),
##                                    join("var/log","DummyHardwareTcp.exe.log")),
##              join(INSTALL_PREFIX,"controlhub_start"),
##              join(INSTALL_PREFIX,"controlhub_status"),
##              #tests
             
##              #clean up
##              join(INSTALL_PREFIX,"controlhub_stats"),
##              "pkill -f DummyHardwareTcp.exe &> /dev/null",
##              "pkill -f DummyHardwareUdp.exe &> /dev/null",
##              join(INSTALL_PREFIX,"controlhub_stop"),
                  
             
##              "%s 50001 &> /var/log/DummyHardwareUdp.log &" % join(INSTALL_PREFIX,"DummyHardwareUdp.exe"),
##              ]

REPORT_CMDS = ["python $HOME/nightly/nanalyzer.py cactus.py"]

             
