from os import environ
from os.path import join

####VARIABLES
BUILD_HOME          = '/build/cactus'
RELEASE_DIR         = "/afs/cern.ch/user/c/cactus/www/nightly/RPMS"
XDAQ_REPO_FILE      = "/afs/cern.ch/user/c/cactus/nightly/cactus_xdaq.repo"
YUMGROUP_FILE       = "/afs/cern.ch/user/c/cactus/nightly/cactus_yumgroups.xml"
CACTUS_REPO_FILE    = "/afs/cern.ch/user/c/cactus/nightly/cactus.repo"
INSTALL_PREFIX      = "/opt/cactus"
CONTROLHUB_EBIN_DIR = join(INSTALL_PREFIX,"lib/controlhub/lib/controlhub-1.1.0/ebin")

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
                     'FAILED',
                     'FAIL: test', 'ERROR: test', #pycohal
                     '*failed*', #controlhub
                     'terminate called']

IGNORE_ERROR_LIST = []

TEST_PASSED_LIST  = ["TEST PASSED",
                     "CHECK PASSED",
                     "TEST_THROW PASSED",
                     "TEST_NOTHROW PASSED",
                     " ... ok", #pycohal
                     "...ok", #controlhub
                     "Average read bandwidth",
                     "Average write bandwidth"]


####ENVIRONMENT
environ["LD_LIBRARY_PATH"] = join(INSTALL_PREFIX,"lib") + ":" + environ.get("LD_LIBARY_PATH","")
environ["PATH"]            = join(INSTALL_PREFIX,"bin/uhal/tests") + ":" + join(INSTALL_PREFIX,"bin/pycohal/tests") + ":" + environ.get("PATH","")

####COMMANDS
UNINSTALL_CMDS = ["pkill -f \"DummyHardwareTcp.exe\" &> /dev/null",
                  "pkill -f \"DummyHardwareUdp.exe\" &> /dev/null",
                  "pkill -f \"cactus.*erlang\" &> /dev/null",
                  "pkill -f \"cactus.*controlhub\" &> /dev/null",
                  "sudo rm -rf %s" % BUILD_HOME,
                  "mkdir -p %s" % BUILD_HOME,
                  "sudo yum -y groupremove cactus",
                  "rpm -qa | grep cactus- | xargs sudo rpm -ev &> /dev/null",
                  ]

ENVIRONMENT_CMDS = ["env"]

DEPENDENCIES_CMDS = ["sudo yum -y install bzip2-devel zlib-devel ncurses-devel python-devel"]

CHECKOUT = ["cd %s" % BUILD_HOME,
            "svn co svn+ssh://svn.cern.ch/reps/cactus/trunk"]
#            "svn co svn+ssh://svn.cern.ch/reps/cactus/branches/cactus_1_0_x ./trunk"]

CHECKOUT_CMDS = ["sudo rm -rf %s" % BUILD_HOME,
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
             #CONTROLHUB STANDALONE TESTS
             "%s -noshell -pa %s %s -eval 'eunit:test(\"%s\",[verbose])' -s init stop" % (join(INSTALL_PREFIX,"bin/erl"), CONTROLHUB_EBIN_DIR, join(CONTROLHUB_EBIN_DIR, "unittest"), CONTROLHUB_EBIN_DIR),
             #SERVER NOT REACHABLE TESTS
             "test_dummy_nonreachable.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_nonreachable.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_nonreachable.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "sudo /opt/cactus/bin/controlhub_start",
             "sudo /opt/cactus/bin/controlhub_status",
             "test_dummy_nonreachable.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "sudo /opt/cactus/bin/controlhub_stop",
             #TIMEOUT TESTS
             "DummyHardwareUdp.exe --version 1 --port 50001 --delay 2 &> /dev/null &",
             "test_dummy_timeout.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "pkill -f \"DummyHardwareUdp.exe\"",
             "DummyHardwareUdp.exe --version 1 --port 50001 --delay 2 &> /dev/null &",
             "sudo /opt/cactus/bin/controlhub_start",
             "sudo /opt/cactus/bin/controlhub_status",
             "test_dummy_timeout.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "pkill -f \"DummyHardwareUdp.exe\"",
             "sudo /opt/cactus/bin/controlhub_stop",
             "DummyHardwareTcp.exe --version 1 --port 50002 --delay 2 &> /dev/null &",
             "test_dummy_timeout.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "pkill -f \"DummyHardwareTcp.exe\"",
             #UDP TESTS
             "DummyHardwareUdp.exe --version 1 --port 50001 &> /dev/null &",
             "PerfTester.exe -t BandwidthTx -b 0x01 -w 1 -i 1000 -p -d ipbusudp-1.3://localhost:50001",
             "PerfTester.exe -t BandwidthTx -b 0x01 -w 262144 -i 1000 -p -d ipbusudp-1.3://localhost:50001",
             "PerfTester.exe -t BandwidthRx -b 0x01 -w 1 -i 1000 -p -d ipbusudp-1.3://localhost:50001",
             "PerfTester.exe -t BandwidthRx -b 0x01 -w 262144 -i 1000 -p -d ipbusudp-1.3://localhost:50001",
             "test_dummy_single.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_block.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_docu_examples.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.docu.udp",
             "test_dummy_check_permissions.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_hierarchy.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_multithreaded.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_metainfo.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_navigation.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "test_dummy_rawclient.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.udp",
             "pkill -f \"DummyHardwareUdp.exe\"",
             #CONTROL HUB TESTS
             "DummyHardwareUdp.exe --version 1 --port 50001 &> /dev/null &",
             "sudo /opt/cactus/bin/controlhub_start",
             "sudo /opt/cactus/bin/controlhub_status",
             "PerfTester.exe -t BandwidthTx -b 0x01 -w 1 -i 1000 -p -d chtcp-1.3://localhost:10203?target=localhost:50001",
             "PerfTester.exe -t BandwidthTx -b 0x01 -w 262144 -i 1000 -p -d chtcp-1.3://localhost:10203?target=localhost:50001",
             "PerfTester.exe -t BandwidthRx -b 0x01 -w 1 -i 1000 -p -d chtcp-1.3://localhost:10203?target=localhost:50001",
             "PerfTester.exe -t BandwidthRx -b 0x01 -w 262144 -i 1000 -p -d chtcp-1.3://localhost:10203?target=localhost:50001",
             "test_dummy_single.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_block.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_docu_examples.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.docu.controlhub",
             "test_dummy_check_permissions.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_hierarchy.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_multithreaded.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_metainfo.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_navigation.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "test_dummy_rawclient.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.controlhub",
             "pkill -f \"DummyHardwareUdp.exe\"",
             "sudo /opt/cactus/bin/controlhub_stop",
             #TCP TESTS
             "DummyHardwareTcp.exe --version 1 --port 50002 &> /dev/null &",
             "PerfTester.exe -t BandwidthTx -b 0x01 -w 1 -i 1000 -p -d ipbustcp-1.3://localhost:50002",
             "PerfTester.exe -t BandwidthTx -b 0x01 -w 262144 -i 1000 -p -d ipbustcp-1.3://localhost:50002",
             "PerfTester.exe -t BandwidthRx -b 0x01 -w 1 -i 1000 -p -d ipbustcp-1.3://localhost:50002",
             "PerfTester.exe -t BandwidthRx -b 0x01 -w 262144 -i 1000 -p -d ipbustcp-1.3://localhost:50002",
             "test_dummy_single.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_block.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_docu_examples.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.docu.tcp",
             "test_dummy_check_permissions.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_hierarchy.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_multithreaded.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_metainfo.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_navigation.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "test_dummy_rawclient.exe -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -d dummy.tcp",
             "pkill -f \"DummyHardwareTcp.exe\"",
             #PYCOHAL TESTS
             "DummyHardwareUdp.exe --version 1 --port 50001 &> /dev/null &",
             "test_pycohal -c file:///opt/cactus/etc/uhal/tests/dummy_connections.xml -v",
             "pkill -f \"DummyHardwareUdp.exe\""
             ]

REPORT_CMDS = ["python $HOME/nightly/nanalyzer.py cactus.py"]

