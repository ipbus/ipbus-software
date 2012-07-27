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

TEST_PASSED_LIST  = ["TEST PASSED",
                     "CHECK PASSED",
                     "TEST_THROW PASSED",
                     "TEST_NOTHROW PASSED"]


####ENVIRONMENT
environ["LD_LIBRARY_PATH"] = join(INSTALL_PREFIX,"lib") + ":" + environ.get("LD_LIBARY_PATH","")
environ["PATH"]            = join(INSTALL_PREFIX,"bin/uhal/tests") + ":" + environ.get("PATH","")

####COMMANDS
UNINSTALL_CMDS = ["pkill -f \"DummyHardwareTcp.exe\" &> /dev/null",
                  "pkill -f \"DummyHardwareUdp.exe\" &> /dev/null",
                  "pkill -f \"cactus.*erlang\" &> /dev/null",
                  "pkill -f \"cactus.*controlhub\" &> /dev/null",
                  "rm -rf %s" % BUILD_HOME,
                  "mkdir -p %s" % BUILD_HOME,
                  "sudo yum -y groupremove cactus",
                  "rpm -qa | grep cactus- | xargs sudo rpm -ev &> /dev/null",
                  ]

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
                "cd /build/cactus;rm -rf %s;mv html %s" % (join(WEB_DIR,"api/html"), join(WEB_DIR, "api/."))]

TEST_CMDS = ["sudo chmod +w /var/log",
             #SERVER NOT REACHABLE TESTS
             "test_dummy_timeout.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_timeout.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_timeout.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "sudo /opt/cactus/bin/controlhub_start",
             "sudo /opt/cactus/bin/controlhub_status",
             "test_dummy_timeout.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "sudo /opt/cactus/bin/controlhub_stop",
             #TIMEOUT TESTS
             "DummyHardwareUdp.exe 50001 20 &> /var/log/DummyHardwareUdp.exe.log &",
             "test_dummy_timeout.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "sudo /opt/cactus/bin/controlhub_start",
             "sudo /opt/cactus/bin/controlhub_status",
             "test_dummy_timeout.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "pkill -f \"DummyHardwareUdp.exe\"",
             "sudo /opt/cactus/bin/controlhub_stop",
             "DummyHardwareTcp.exe 50001 20 &> /var/log/DummyHardwareUdp.exe.log &",
             "test_dummy_timeout.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "pkill -f \"DummyHardwareTcp.exe\"",
             #UDP TESTS
             "DummyHardwareUdp.exe 50001 &> /var/log/DummyHardwareUdp.exe.log &",
             "test_dummy_single.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_block.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_check_permissions.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_hierarchy.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_multithreaded.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_metainfo.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_navigation.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_rawclient.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "pkill -f \"DummyHardwareUdp.exe\"",
             #CONTROL HUB TESTS
             "DummyHardwareUdp.exe 50001 &> /var/log/DummyHardwareUdp.exe.log &",
             "sudo /opt/cactus/bin/controlhub_start",
             "sudo /opt/cactus/bin/controlhub_status",
             "test_dummy_single.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_block.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_check_permissions.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_hierarchy.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_multithreaded.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_metainfo.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_navigation.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_rawclient.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "pkill -f \"DummyHardwareUdp.exe\"",
             "sudo /opt/cactus/bin/controlhub_stop",
             #TCP TESTS
             "DummyHardwareTcp.exe 50002 &> /var/log/DummyHardwareTcp.exe.log &",
             "test_dummy_single.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_block.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_check_permissions.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_hierarchy.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_multithreaded.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_metainfo.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_navigation.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_rawclient.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "pkill -f \"DummyHardwareTcp.exe\""
             ]

REPORT_CMDS = ["python $HOME/nightly/nanalyzer.py cactus.py"]

             
