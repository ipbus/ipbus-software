from nutils import system
from os import environ
from os.path import join
from platform import platform
from socket import getfqdn

####VARIABLES
BUILD_HOME          = "/build/cactus"
PLATFORM            = platform()
NIGHTLY_BASE        = "/afs/cern.ch/user/c/cactus/www/nightly"
RELEASE_BASE        = join(NIGHTLY_BASE,PLATFORM)
RELEASE_RPM_DIR     = join(RELEASE_BASE,"RPMS")
RELEASE_LOG_DIR     = join(RELEASE_BASE,"logs")
RELEASE_API_DIR     = join(RELEASE_BASE,"api")
#The log file name and path should be the same than in the one in the acrontab
CACTUS_PREFIX       = "/opt/cactus"
XDAQ_ROOT           = "/opt/xdaq"
L1PAGE_ROOT	    = "/opt/l1page/tomcat/webapps/ROOT"
CONTROLHUB_EBIN_DIR = join(CACTUS_PREFIX,"lib/controlhub/lib/controlhub-1.1.0/ebin")
#xdaq.repo file name as a function of the platform, and alias dirs for the nightlies results
pseudo_platform= "unknown"
l1page_platform= "noarch"

if PLATFORM.find("i686-with-redhat-5") != -1:
    pseudo_platform="slc5_i686"
elif PLATFORM.find("x86_64-with-redhat-5") != -1:
    pseudo_platform="slc5_x86_64"
elif PLATFORM.find("i686-with-redhat-6") != -1:
    pseudo_platform="slc6_i686"
elif PLATFORM.find("x86_64-with-redhat-6") != -1:
    pseudo_platform="slc6_x86_64"

XDAQ_REPO_FILE_NAME = "xdaq.%s.repo" % pseudo_platform
system("cd %s;rm -f %s" % (NIGHTLY_BASE,pseudo_platform),exception=False)
system("cd %s;ln -s %s %s" % (NIGHTLY_BASE,PLATFORM,pseudo_platform),exception=False)
  

####VARIABLES: analysis of logs
TITLE             = "CACTUS Nightlies: %s " % pseudo_platform
FROM_EMAIL        = "cactus.service@cern.ch"
TO_EMAIL          = "cms-cactus@cern.ch"
WEB_URL           = join("http://cern.ch/cactus/nightly/",PLATFORM)
RELEASE_LOG_FILE    = join(RELEASE_LOG_DIR,"nightly.log")
ERROR_LIST        = ['TEST FAILED, ',
                     'error: ',
                     'RPM build errors',
                     'collect2: ld returned',
                     ' ERROR ',
                     ' Error ',
                     'FAILED',
                     'FAIL: test', 'ERROR: test', #pycohal
                     '*failed*', #controlhub
                     'terminate called',
		     'L1Page ERROR']

IGNORE_ERROR_LIST = ["sudo pkill",
                     "sudo rpm -ev"]

TEST_PASSED_LIST  = ["TEST PASSED",
                     "CHECK PASSED",
                     "TEST_THROW PASSED",
                     "TEST_NOTHROW PASSED",
                     " ... ok", #pycohal
                     "...ok", #controlhub
                     "Average read bandwidth",
                     "Average write bandwidth",
                     "TEST OK",
                     "L1Page OK"]


####ENVIRONMENT
environ["XDAQ_ROOT"]       = XDAQ_ROOT
environ["LD_LIBRARY_PATH"] = ":".join([join(CACTUS_PREFIX,"lib"),
                                       join(XDAQ_ROOT,"lib"),
                                       "/lib",
                                       environ.get("LD_LIBARY_PATH","")])

environ["PATH"]            = ":".join([join(CACTUS_PREFIX,"bin"),
                                       join(CACTUS_PREFIX,"bin/uhal/tests"),
                                       environ.get("PATH","")])

####COMMANDS
COMMANDS = []

COMMANDS += [["UNINSTALL",
              ["sudo /sbin/service xdaqd stop &> /dev/null ",
               "sudo yum -y groupremove cactus ",
               "rpm -qa | grep cactus- | xargs sudo rpm -ev &> /dev/null ",
               "sudo yum -y groupremove triggersupervisor uhal ",
               "sudo yum -y groupremove extern_coretools coretools extern_powerpack powerpack database_worksuite general_worksuite hardware_worksuite ",
               "rpm -qa | grep l1page | xargs sudo rpm -ev &> /dev/null ",
               "sudo pkill -f \"xdaq.exe\" ",
               "rpm -qa| grep cactuscore- | xargs sudo rpm -ev &> /dev/null ",
               "rpm -qa| grep cactusprojects- | xargs sudo rpm -ev &> /dev/null ",
               "sudo pkill -f \"jsvc\" &> /dev/null ",
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
              ["sudo yum -y install arc-server createrepo bzip2-devel zlib-devel ncurses-devel python-devel curl curl-devel e2fsprogs-devel graphviz graphviz-devel boost boost-devel wxPython",
               "sudo cp %s %s" % (XDAQ_REPO_FILE_NAME,"/etc/yum.repos.d/xdaq.repo"),
               "sudo yum -y groupinstall extern_coretools coretools extern_powerpack powerpack database_worksuite general_worksuite hardware_worksuite"]]]

CHECKOUT_CMDS = ["cd %s" % BUILD_HOME,
                 "svn co svn+ssh://svn.cern.ch/reps/cactus/trunk",
                 "svn co svn+ssh://svn.cern.ch/reps/cmsos/branches/l1_xaas daq/xaas"]
#            "svn co svn+ssh://svn.cern.ch/reps/cactus/branches/cactus_1_0_x ./trunk"]

L1PAGE_SED_CMDS = [""
                   ]
COMMANDS += [["CHECKOUT",
              [";".join(CHECKOUT_CMDS)]]]


COMMANDS += [["BUILD",
              ["cd %s;make -k" % join(BUILD_HOME,"trunk"),
               "cd %s;make -k rpm" % join(BUILD_HOME,"trunk")]]]

COMMANDS += [["RELEASE",
              ["rm -rf %s" % RELEASE_RPM_DIR,
               "mkdir -p %s" % RELEASE_RPM_DIR,
               "mkdir -p %s" % RELEASE_LOG_DIR,
               "mkdir -p %s" % RELEASE_API_DIR,
               "cp %s %s" % ("yumgroups.xml",RELEASE_RPM_DIR),
               "find %s -name '*.rpm' -exec cp {} %s \;" % (BUILD_HOME,RELEASE_RPM_DIR),
               "cd %s;createrepo -vg yumgroups.xml ." % RELEASE_RPM_DIR]]]

COMMANDS += [["INSTALL",
              ["sed \"s/<platform>/%s/\" cactus.repo  | sudo tee /etc/yum.repos.d/cactus.repo > /dev/null" % PLATFORM,
               "sudo yum clean all",
               "sudo yum -y groupinstall triggersupervisor uhal"]]]

COMMANDS += [["TEST CONTROLHUB",
              ["sudo chmod +w /var/log",
               "%s -noshell -pa %s %s -eval 'eunit:test(\"%s\",[verbose])' -s init stop" % (join(CACTUS_PREFIX,"bin/erl"), CONTROLHUB_EBIN_DIR, join(CONTROLHUB_EBIN_DIR, "unittest"), CONTROLHUB_EBIN_DIR)]]]

COMMANDS += [["TEST IPBUS 1.3",
              ["uhal_test_suite.py -v -s 1.3"]
            ]]

COMMANDS += [["TEST IPBUS 2.0",
              ["uhal_test_suite.py -v -s 2.0"]
            ]]

COMMANDS += [["TEST uHAL GUI",
              ["uhal_test_suite.py -v -s gui"]
            ]]

COMMANDS += [["TEST uHAL TOOLS",
              ["uhal_test_suite.py -v -s tools"]
            ]]

COMMANDS += [["TEST TRIGGER SUPERVISOR",             
              ["sudo cp %s /etc/tnsnames.ora" % join(BUILD_HOME,"daq/xaas/slim/l1test/settings/etc/tnsnames.cern.ora"),
               "cp -r %s %s" % ("/afs/cern.ch/user/c/cactus/secure",BUILD_HOME),
               "sed -i 's|\(PWD_PATH=\).*$|\\1%s|' %s" % (join(BUILD_HOME,"secure"),
                                                          join(BUILD_HOME,"daq/xaas/slim/l1test/service/mf.service.settings")),
               "sed -i 's|\(SLIM_SERVICE_HOST=\).*$|\\1%s|' %s" % (getfqdn(),
                                                                   join(BUILD_HOME,"daq/xaas/slim/l1test/service/mf.service.settings")),
               "cd %s;make;make rpm;make install" % join(BUILD_HOME,"daq/xaas/slim/l1test"),
               "sudo cp %s /etc/slp.conf" % join(BUILD_HOME,"daq/xaas/slim/l1test/settings/etc/slp.localhost.conf"),
               "sudo /sbin/service slp restart",
               "/bin/slptool findsrvs service:directory-agennt",
               "sudo /sbin/service xdaqd start",
               "sudo /sbin/service xdaqd status",
               "sleep 240",
               "cd %s;python multicell.py" % join(BUILD_HOME,"trunk/cactusprojects/subsystem/tests"),
               "cd %s;python multicell_fault.py;" % join(BUILD_HOME,"trunk/cactusprojects/subsystem/tests"),
               "cd %s;python multicell_stress.py" % join(BUILD_HOME,"trunk/cactusprojects/subsystem/tests")]]]

COMMANDS += [["TEST CENTRAL CELL",
              ["cd %s;python central.py" % join(BUILD_HOME,"trunk/cactusprojects/central/tests")]]]

COMMANDS += [["TEST RETRI CELL",
              ["cd %s;python retri.py" % join(BUILD_HOME,"trunk/cactusprojects/retri/tests")]]]

COMMANDS += [["TEST TTC",
              ["cd %s;python ttc.py" % join(BUILD_HOME,"trunk/cactusprojects/ttc/tests"),
               "sudo /sbin/service xdaqd stop",
               "rpm -qa | grep daq-xaas-l1tes | xargs sudo rpm -ev"]]]

COMMANDS += [["TEST L1PAGE",
              ["sudo yum -y install cactusprojects-l1page-*",
               "mkdir -p %s" % join(BUILD_HOME, "triggerpro/l1page/data"),
               "sudo sed -i 's|%s|%s|g' %s" % ("/nfshome0/centraltspro", BUILD_HOME, join(L1PAGE_ROOT, "main/l1page.properties")),
               "sudo sed -i 's|%s|%s|g' %s" % ("/nfshome0", BUILD_HOME, join(L1PAGE_ROOT, "main/l1page.properties")),
               "sudo sed -i 's|%s|%s|g' %s" % ("log4j.appender","#log4j.appender", join(L1PAGE_ROOT, "WEB-INF/classes/log4j.properties")),
               "python %s" % join(L1PAGE_ROOT, "test/l1pageTest.py")]
              ]]

COMMANDS += [["REPORTING",
              ["python -u %s %s" % ("nanalyzer.py","cactus.py"),
               "mkdir -p %s" % RELEASE_LOG_DIR,
               "sudo cp -r %s %s" % ("/var/log/*",RELEASE_LOG_DIR)]]]
