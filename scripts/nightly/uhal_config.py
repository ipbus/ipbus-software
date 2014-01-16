from nutils import system
from os import environ,listdir
from os.path import join,basename,isdir
from sys import argv
from platform import platform
from socket import getfqdn

####VARIABLES
BUILD_HOME          = "/build/cactus"
PLATFORM            = platform()
RELATIVE_BASE       = join("nightly",__name__+".py",PLATFORM)
NIGHTLY_BASE        = join("/afs/cern.ch/user/c/cactus/www",RELATIVE_BASE)
NIGHTLY_RPM_DIR     = join(NIGHTLY_BASE,"RPMS")
NIGHTLY_LOG_DIR     = join(NIGHTLY_BASE,"logs")
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

# The following lines are meant to delete old platform directories containing RPMs and logs
target_platform = "unknown"
if pseudo_platform == "slc5_i686":
    target_platform = "i686-with-redhat-5"
elif pseudo_platform == "slc5_x86_64":
    target_platform = "x86_64-with-redhat-5"
elif pseudo_platform == "slc6_x86_64":
    target_platform = "x86_64-with-redhat-6"
    
del_dirs = [d for d in listdir(join(NIGHTLY_BASE, "..")) if isdir(join(NIGHTLY_BASE, "..", d)) and d.find(target_platform) != -1 and d != platform()]
for d in del_dirs:
    system("rm -rf %s" % join(NIGHTLY_BASE, "..", d), exception=False)

####VARIABLES: analysis of logs
TITLE             = "uHAL Nightlies: %s " % pseudo_platform
FROM_EMAIL        = "cactus.service@cern.ch"
TO_EMAIL          = "cms-cactus@cern.ch"
WEB_URL           = join("http://cern.ch/cactus",RELATIVE_BASE)
NIGHTLY_LOG_FILE    = join(NIGHTLY_LOG_DIR,"nightly.log")
ERROR_LIST        = ['error: ',
                     'RPM build errors',
                     'collect2: ld returned',
                     ' ERROR ', ' FATAL ',
                     ' Error ',
                     'FAILED',
                     'FAIL: test', 'ERROR: test', #pycohal
                     '*failed*', #controlhub
                     'terminate called',
                     'Segmentation fault']

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
              ["rm -rf %s" % NIGHTLY_RPM_DIR,
               "mkdir -p %s" % NIGHTLY_RPM_DIR,
               "mkdir -p %s" % NIGHTLY_LOG_DIR,
               "cp %s %s" % ("yumgroups.xml",NIGHTLY_RPM_DIR),
               "find %s -name '*.rpm' -exec cp {} %s \;" % (BUILD_HOME,NIGHTLY_RPM_DIR),
               "cd %s;createrepo -vg yumgroups.xml ." % NIGHTLY_RPM_DIR]]]

COMMANDS += [["INSTALL",
              ["sed \"s/<platform>/%s/\" uhal.nightly.repo  | sudo tee /etc/yum.repos.d/uhal.repo > /dev/null" % pseudo_platform,
               "sudo yum clean all",
               "sudo yum -y groupinstall uhal"]]]

COMMANDS += [["TEST CONTROLHUB",
              ["sudo chmod +w /var/log",
               "%s -noshell -pa %s %s -eval 'eunit:test(\"%s\",[verbose])' -s init stop" % (join(CACTUS_PREFIX,"bin/erl"), CONTROLHUB_EBIN_DIR, join(CONTROLHUB_EBIN_DIR, "unittest"), CONTROLHUB_EBIN_DIR),
               'for i in `seq 1 100`; do sudo /opt/cactus/bin/controlhub_start; if [ "$?" != "0" ]; then echo "ERROR IN STARTING CONTROLHUB"; fi; /opt/cactus/bin/controlhub_status; if [ "$?" != "0" ]; then echo "ERROR: CONTROLHUB SHOULD HAVE ALREADY STARTED"; fi; sudo /opt/cactus/bin/controlhub_stop; done',
               'uhal_test_suite.py -v -s "test controlhub start"']
            ]]

COMMANDS += [["TEST IPbus 1.3 - UDP",
              ['uhal_test_suite.py -v -s "1.3 udp"']
            ]]

COMMANDS += [["TEST IPbus 1.3 - TCP",
              ['uhal_test_suite.py -v -s "1.3 tcp"']
            ]]

COMMANDS += [["TEST IPbus 1.3 - ControlHub",
              ['uhal_test_suite.py -v -s "1.3 controlhub"']
            ]]


COMMANDS += [["TEST IPbus 2.0 - UDP",
              ['uhal_test_suite.py -v -s "2.0 udp"']
            ]]

COMMANDS += [["TEST IPbus 2.0 - TCP",
              ['uhal_test_suite.py -v -s "2.0 tcp"']
            ]]

COMMANDS += [["TEST IPbus 2.0 - ControlHub (no packet loss)",
              ['uhal_test_suite.py -v -s "2.0 controlhub - normal"']
            ]]

COMMANDS += [["TEST IPbus 2.0 - ControlHub (with packet loss)",
              ['uhal_test_suite.py -v -s "2.0 controlhub - light packet loss"']
            ]]


COMMANDS += [["TEST IPbus 2.0 big-endian - UDP",
              ['uhal_test_suite.py -v -s "2.0 bigendian udp"']
            ]]

COMMANDS += [["TEST IPbus 2.0 big-endian - TCP",
              ['uhal_test_suite.py -v -s "2.0 bigendian tcp"']
            ]]

COMMANDS += [["TEST IPbus 2.0 big-endian - ControlHub (no packet loss)",
              ['uhal_test_suite.py -v -s "2.0 bigendian controlhub - normal"']
            ]]


COMMANDS += [["TEST PYCOHAL",
              ["uhal_test_suite.py -v -s pycohal"]
            ]]

COMMANDS += [["TEST uHAL GUI",
              ["uhal_test_suite.py -v -s gui"]
            ]]

COMMANDS += [["TEST uHAL TOOLS",
              ["uhal_test_suite.py -v -s tools"]
            ]]


