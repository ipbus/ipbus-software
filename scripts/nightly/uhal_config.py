from nutils import system
from os import environ,listdir
from os.path import join,basename,isdir,expanduser
from sys import argv
from platform import platform
from socket import getfqdn
import getpass


class Nightly :
  def __init__(self, group,email,name,b_home):
    print "Selected group '"+group+"'"

    self.DEFAULT_COMMANDS    = ["CLEANUP_WWW_AREA","UNINSTALL","ENVIRONMENT","DEPENDENCIES",
                       "CHECKOUT","BUILD","RELEASE","INSTALL","TEST_CONTROLHUB",
                       "TEST_IPbus_1.3_UDP","TEST_IPbus_1.3_TCP","TEST_IPbus_1.3_ControlHub",
                       "TEST_IPbus_2.0_UDP","TEST_IPbus_2.0_TCP",
                       "TEST_IPbus_2.0_ControlHub_No_packet_loss",
                       "TEST_IPbus_2.0_ControlHub_With_packet_loss",
                       "TEST_PYCOHAL","TEST_uHAL_GUI","TEST_uHAL_TOOLS"]
    
    self.TO_EMAIL            = email
    self.CHECKOUT_NAME       = name
    self.BUILD_HOME          = b_home
    
    self.USER_NAME           = getpass.getuser()
    self.USER_HOME           = join(expanduser("~"))
    self.PLATFORM            = platform()
    self.RELATIVE_BASE       = join("nightly",__name__+".py",self.PLATFORM)
    self.NIGHTLY_BASE        = join(self.USER_HOME,"www",self.RELATIVE_BASE)
    self.NIGHTLY_RPM_DIR     = join(self.NIGHTLY_BASE,"RPMS")
    self.NIGHTLY_LOG_DIR     = join(self.NIGHTLY_BASE,"logs")
    #The log file name and path should be the same than in the one in the acrontab
    self.CACTUS_PREFIX       = "/opt/cactus"
    self.CONTROLHUB_EBIN_DIR = join(self.CACTUS_PREFIX,"lib/controlhub/lib/controlhub-1.1.0/ebin")

    #if not CHECKOUT_NAME :
      #CHECKOUT_NAME  = USER_NAME
      
    #if not TO_EMAIL :
      #TO_EMAIL  = USER_NAME + "@cern.ch"

    #PSEUDO PLATFORM
    self.pseudo_platform = "unknown"
    if self.PLATFORM.find("i686-with-redhat-5") != -1:
        self.pseudo_platform="slc5_i686"
    elif self.PLATFORM.find("x86_64-with-redhat-5") != -1:
        self.pseudo_platform="slc5_x86_64"
    elif self.PLATFORM.find("x86_64-with-redhat-6") != -1:
        self.pseudo_platform="slc6_x86_64"

    ####VARIABLES: analysis of logs
    self.TITLE             = "uHAL Nightlies: %s " % self.pseudo_platform
    self.FROM_EMAIL        = "cactus.service@cern.ch"
    self.WEB_URL           = join("http://cern.ch/"+self.USER_NAME,self.RELATIVE_BASE)
    self.NIGHTLY_LOG_FILE    = join(self.NIGHTLY_LOG_DIR,"nightly.log")
    self.ERROR_LIST        = ['error: ',
                        'RPM build errors',
                        'collect2: ld returned',
                        ' ERROR ', ' FATAL ',
                        ' Error ',
                        'FAILED',
                        'FAIL: test', 'ERROR: test', #pycohal
                        '*failed*', #controlhub
                        'CRITICAL', # uhal/tools
                        'terminate called',
                        'Segmentation fault']

    self.IGNORE_ERROR_LIST = ["sudo pkill",
                        "sudo rpm -ev"]

    self.TEST_PASSED_LIST  = ["TEST PASSED",
                        "CHECK PASSED",
                        "TEST_THROW PASSED",
                        "TEST_NOTHROW PASSED",
                        " ... ok", #pycohal
                        "...ok", #controlhub
                        "Average read bandwidth",
                        "Average write bandwidth"]


    ####ENVIRONMENT
    environ["LD_LIBRARY_PATH"] = ":".join([join(self.CACTUS_PREFIX,"lib"),
                                          "/lib",
                                          environ.get("LD_LIBARY_PATH","")])

    environ["PATH"]            = ":".join([join(self.CACTUS_PREFIX,"bin"),
                                          join(self.CACTUS_PREFIX,"bin/uhal/tests"),
                                          join(self.CACTUS_PREFIX,"bin/uhal/tools"),
                                          environ.get("PATH","")])
    
    self.importCommands()

  def importCommands(self) :
    self.COMMANDS = []
    
    self.COMMANDS += [["TESTECHO", 
                  ["echo This is a test",
                  "echo TO_EMAIL = %s" % self.TO_EMAIL,
                  "echo CHECKOUT_NAME = %s" % self.CHECKOUT_NAME,
                  "echo BUILD_HOME = %s" % self.BUILD_HOME,
                  "echo PLATFORM = %s" % self.PLATFORM,
                  "echo RELATIVE_BASE = %s" % self.RELATIVE_BASE,
                  "echo NIGHTLY_BASE = %s" % self.NIGHTLY_BASE]]]

    self.COMMANDS += [["CLEANUP_WWW_AREA", [""]]] # Dummy placeholder -- this corresponds to the call to cleanupLogs()

    self.COMMANDS += [["UNINSTALL",
                  ["sudo yum clean all",
                  "sudo yum -y groupremove uhal",
                  "rpm -qa| grep cactuscore- | xargs sudo rpm -ev &> /dev/null ",
                  "rpm -qa| grep cactusprojects- | xargs sudo rpm -ev &> /dev/null ",
                  "sudo pkill -f \"DummyHardwareTcp.exe\" &> /dev/null ",
                  "sudo pkill -f \"DummyHardwareUdp.exe\" &> /dev/null ",
                  "sudo pkill -f \"cactus.*erlang\" &> /dev/null ",
                  "sudo pkill -f \"cactus.*controlhub\" &> /dev/null ",
                  "sudo rm -rf %s" % self.BUILD_HOME]]]

    self.COMMANDS += [["ENVIRONMENT",
                  ["env"]]]

    self.COMMANDS += [["DEPENDENCIES",
                  ["sudo yum -y install arc-server createrepo bzip2-devel zlib-devel ncurses-devel python-devel curl curl-devel graphviz graphviz-devel boost boost-devel wxPython e2fsprogs-devel qt qt-devel PyQt PyQt-devel qt-designer"
                  ]]]

    CHECKOUT_CMDS = ["sudo mkdir -p %s" % self.BUILD_HOME,
                    "sudo chmod -R 777 %s" % self.BUILD_HOME,
                    "cd %s" % self.BUILD_HOME,
                    "svn -q co svn+ssh://svn.cern.ch/reps/cactus/trunk",
    #                 "svn -q co svn+ssh://svn.cern.ch/reps/cactus/branches/uhal_2_0_x ./trunk"
                    ]

    self.COMMANDS += [["CHECKOUT",
                  [";".join(CHECKOUT_CMDS)]]]


    self.COMMANDS += [["BUILD",
                  ["cd %s;make -sk Set=uhal" % join(self.BUILD_HOME,"trunk"),
                  "cd %s;make -sk Set=uhal rpm" % join(self.BUILD_HOME,"trunk")]]]

    self.COMMANDS += [["RELEASE",
                  ["rm -rf %s" % self.NIGHTLY_RPM_DIR,
                  "mkdir -p %s" % self.NIGHTLY_RPM_DIR,
                  "mkdir -p %s" % self.NIGHTLY_LOG_DIR,
                  "cp %s %s" % ("yumgroups.xml",self.NIGHTLY_RPM_DIR),
                  "find %s -name '*.rpm' -exec cp {} %s \;" % (self.BUILD_HOME,self.NIGHTLY_RPM_DIR),
                  "cd %s;createrepo -vg yumgroups.xml ." % self.NIGHTLY_RPM_DIR]]]

    self.COMMANDS += [["INSTALL",
                  ["sed \"s/<platform>/%s/\" uhal.nightly.repo  | sudo tee /etc/yum.repos.d/uhal.repo > /dev/null" % self.pseudo_platform,
                  "sudo yum clean all",
                  "sudo yum -y groupinstall uhal"]]]

    self.COMMANDS += [["TEST_CONTROLHUB",
                ["sudo chmod +w /var/log",
                'for i in `seq 1 100`; do /sbin/service controlhub start; if [ "$?" != "0" ]; then echo "ERROR IN STARTING CONTROLHUB"; fi; /sbin/service controlhub status; if [ "$?" != "0" ]; then echo "ERROR: CONTROLHUB SHOULD HAVE ALREADY STARTED"; fi; /sbin/service controlhub stop; done',
                'uhal_test_suite.py -v -s "test controlhub start"',
                'uhal_test_suite.py -v -s "test controlhub start"']
              ]]

    self.COMMANDS += [["TEST_IPbus_1.3_UDP",
                  ['uhal_test_suite.py -v -s "1.3 udp"']
                ]]

    self.COMMANDS += [["TEST_IPbus_1.3_TCP",
                  ['uhal_test_suite.py -v -s "1.3 tcp"']
                ]]

    self.COMMANDS += [["TEST_IPbus_1.3_ControlHub",
                  ['uhal_test_suite.py -v -s "1.3 controlhub"']
                ]]


    self.COMMANDS += [["TEST_IPbus_2.0_UDP",
                  ['uhal_test_suite.py -v -s "2.0 udp"']
                ]]

    self.COMMANDS += [["TEST_IPbus_2.0_TCP",
                  ['uhal_test_suite.py -v -s "2.0 tcp"']
                ]]

    self.COMMANDS += [["TEST_IPbus_2.0_ControlHub_No_packet_loss",
                  ['uhal_test_suite.py -v -s "2.0 controlhub - normal"']
                ]]

    self.COMMANDS += [["TEST_IPbus_2.0_ControlHub_With_packet_loss",
                  ['uhal_test_suite.py -v -s "2.0 controlhub - light packet loss"']
                ]]


    self.COMMANDS += [["TEST_PYCOHAL",
                  ["uhal_test_suite.py -v -s pycohal"]
                ]]

    self.COMMANDS += [["TEST_uHAL_GUI",
                  ["uhal_test_suite.py -v -s gui"]
                ]]

    self.COMMANDS += [["TEST_uHAL_TOOLS",
                  ["uhal_test_suite.py -v -s tools"]
                ]]



  def cleanupLogs(self) :
    # The following lines are meant to delete old platform directories containing RPMs and logs
    target_platform = "unknown"
    if self.pseudo_platform == "slc5_i686":
        target_platform = "i686-with-redhat-5"
    elif self.pseudo_platform == "slc5_x86_64":
        target_platform = "x86_64-with-redhat-5"
    elif self.pseudo_platform == "slc6_x86_64":
        target_platform = "x86_64-with-redhat-6"
      
      
    system("mkdir -p %s" % self.NIGHTLY_BASE,exception=False)
    system("rm -f %s" % join(self.NIGHTLY_BASE,"..",self.pseudo_platform),exception=False)
    system("ln -s %s %s" % (self.NIGHTLY_BASE,join(self.NIGHTLY_BASE,"..",self.pseudo_platform)),exception=False)

    del_dirs = [d for d in listdir(join(self.NIGHTLY_BASE, "..")) if isdir(join(self.NIGHTLY_BASE, "..", d)) and d.find(target_platform) != -1 and d != platform()]
    for d in del_dirs:
        system("rm -rf %s" % join(self.NIGHTLY_BASE, "..", d), exception=False)
        
