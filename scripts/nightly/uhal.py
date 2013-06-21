from nutils import system
from os import environ
from os.path import join,basename
from sys import argv
from platform import platform
from socket import getfqdn

####VARIABLES
BUILD_HOME          = "/build/cactus"
PLATFORM            = platform()
RELATIVE_BASE       = join("nightly",__name__+".py",PLATFORM)
NIGHTLY_BASE        = join("/afs/cern.ch/user/c/cactus/www",RELATIVE_BASE)
RELEASE_RPM_DIR     = join(NIGHTLY_BASE,"RPMS")
RELEASE_LOG_DIR     = join(NIGHTLY_BASE,"logs")
RELEASE_API_DIR     = join(NIGHTLY_BASE,"api")
#The log file name and path should be the same than in the one in the acrontab
CACTUS_PREFIX       = "/opt/cactus"
CONTROLHUB_EBIN_DIR = join(CACTUS_PREFIX,"lib/controlhub/lib/controlhub-1.1.0/ebin")

#PSEUDO PLATFORM
pseudo_platform = "unknown"
if PLATFORM.find("i686-with-redhat-5") != -1:
    pseudo_platform="slc5_i686"
elif PLATFORM.find("x86_64-with-redhat-5") != -1:
    pseudo_platform="slc5_x86_64"
elif PLATFORM.find("x86_64-with-redhat-6") != -1:
    pseudo_platform="slc6_x86_64"

system("mkdir -p %s" % NIGHTLY_BASE,exception=False)
system("rm -f %s" % join(NIGHTLY_BASE,"..",pseudo_platform),exception=False)
system("ln -s %s %s" % (NIGHTLY_BASE,join(NIGHTLY_BASE,"..",pseudo_platform)),exception=False)

####VARIABLES: analysis of logs
TITLE             = "uHAL Nightlies: %s " % pseudo_platform
FROM_EMAIL        = "cactus.service@cern.ch"
TO_EMAIL          = "cms-cactus@cern.ch"
WEB_URL           = join("http://cern.ch/cactus",RELATIVE_BASE)
RELEASE_LOG_FILE    = join(RELEASE_LOG_DIR,"nightly.log")
ERROR_LIST        = ['error: ',
                     'RPM build errors',
                     'collect2: ld returned',
                     ' ERROR ',
                     ' Error ',
                     'FAILED',
                     'FAIL: test', 'ERROR: test', #pycohal
                     '*failed*', #controlhub
                     'terminate called']

IGNORE_ERROR_LIST = ["sudo pkill",
                     "sudo rpm -ev"]

TEST_PASSED_LIST  = ["TEST PASSED",
                     "CHECK PASSED",
                     "TEST_THROW PASSED",
                     "TEST_NOTHROW PASSED",
                     " ... ok", #pycohal
                     "...ok", #controlhub
                     "Average read bandwidth",
                     "Average write bandwidth"]


####ENVIRONMENT
environ["LD_LIBRARY_PATH"] = ":".join([join(CACTUS_PREFIX,"lib"),
                                       "/lib",
                                       environ.get("LD_LIBARY_PATH","")])

environ["PATH"]            = ":".join([join(CACTUS_PREFIX,"bin"),
                                       join(CACTUS_PREFIX,"bin/uhal/tests"),
                                       environ.get("PATH","")])

####COMMANDS
COMMANDS = []

COMMANDS += [["UNINSTALL",
              ["sudo yum clean all",
               "sudo yum -y groupremove uhal",
               "rpm -qa| grep cactuscore- | xargs sudo rpm -ev &> /dev/null ",
               "rpm -qa| grep cactusprojects- | xargs sudo rpm -ev &> /dev/null ",
               "sudo pkill -f \"DummyHardwareTcp.exe\" &> /dev/null ",
               "sudo pkill -f \"DummyHardwareUdp.exe\" &> /dev/null ",
               "sudo pkill -f \"cactus.*erlang\" &> /dev/null ",
               "sudo pkill -f \"cactus.*controlhub\" &> /dev/null ",
               "sudo rm -rf %s" % BUILD_HOME,
               "sudo mkdir -p %s" % BUILD_HOME,
               "sudo chmod -R 777 %s" % BUILD_HOME]]]

COMMANDS += [["ENVIRONMENT",
              ["env"]]]

COMMANDS += [["DEPENDENCIES",
              ["sudo yum -y install arc-server createrepo bzip2-devel zlib-devel ncurses-devel python-devel curl curl-devel graphviz graphviz-devel boost boost-devel wxPython e2fsprogs-devel qt qt-devel PyQt PyQt-devel qt-designer"
               ]]]

CHECKOUT_CMDS = ["cd %s" % BUILD_HOME,
                 "svn -q co svn+ssh://svn.cern.ch/reps/cactus/trunk",
#                 "svn -q co svn+ssh://svn.cern.ch/reps/cactus/branches/uhal_2_0_x ./trunk"
                 ]

COMMANDS += [["CHECKOUT",
              [";".join(CHECKOUT_CMDS)]]]


COMMANDS += [["BUILD",
              ["cd %s;make -sk Set=uhal" % join(BUILD_HOME,"trunk"),
               "cd %s;make -sk Set=uhal rpm" % join(BUILD_HOME,"trunk")]]]

COMMANDS += [["RELEASE",
              ["rm -rf %s" % RELEASE_RPM_DIR,
               "mkdir -p %s" % RELEASE_RPM_DIR,
               "mkdir -p %s" % RELEASE_LOG_DIR,
               "mkdir -p %s" % RELEASE_API_DIR,
               "cp %s %s" % ("yumgroups.xml",RELEASE_RPM_DIR),
               "find %s -name '*.rpm' -exec cp {} %s \;" % (BUILD_HOME,RELEASE_RPM_DIR),
               "cd %s;createrepo -vg yumgroups.xml ." % RELEASE_RPM_DIR]]]

COMMANDS += [["INSTALL",
              ["sed \"s/<platform>/%s/\" uhal.nightly.repo  | sudo tee /etc/yum.repos.d/uhal.repo > /dev/null" % pseudo_platform,
               "sudo yum clean all",
               "sudo yum -y groupinstall uhal"]]]

COMMANDS += [["TEST CONTROLHUB",
              ["sudo chmod +w /var/log",
               "%s -noshell -pa %s %s -eval 'eunit:test(\"%s\",[verbose])' -s init stop" % (join(CACTUS_PREFIX,"bin/erl"), CONTROLHUB_EBIN_DIR, join(CONTROLHUB_EBIN_DIR, "unittest"), CONTROLHUB_EBIN_DIR)]]]

COMMANDS += [["TEST IPBUS 1.3",
              ["uhal_test_suite.py -v -s 1.3"]
            ]]

COMMANDS += [["TEST IPBUS 2.0 - UDP",
              ['uhal_test_suite.py -v -s "2.0 udp"']
            ]]

COMMANDS += [["TEST IPBUS 2.0 - TCP",
              ['uhal_test_suite.py -v -s "2.0 tcp"']
            ]]

COMMANDS += [["TEST IPBUS 2.0 - ControlHub",
              ['uhal_test_suite.py -v -s "2.0 controlhub"']
            ]]

COMMANDS += [["TEST uHAL GUI",
              ["uhal_test_suite.py -v -s gui"]
            ]]

COMMANDS += [["TEST uHAL TOOLS",
              ["uhal_test_suite.py -v -s tools"]
            ]]


