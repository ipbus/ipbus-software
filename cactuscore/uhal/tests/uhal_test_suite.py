#!/bin/env python
"""
Usage: test_uhal.py [-v] [-l] [-c <path to xml connections file>] [tests to run]
This script runs all of the uHAL tests (either using installed system or using checked-out source code)

All options/arguments are optional:
   -v    : Print all output of tests (if not given, only the results summary for each test command is printed)
   -l    : Only list commands (don't run them)
   -c /path/to/dummy_connections.xml  : Full path to 'etc/uhal/tests' directory

E.g:
  # Limited output of all tests on installed system
  ./test_uhal.py
  # Full output of IPbus 1.3 UDP & ControlHub tests on developer source code
  ./test_uhal.py -v -c /path/to/dummy_connections.xml ipbus1.3:udp ipbus1.3:controlhub

N.B: LD_LIBRARY_PATH and PATH env variables both have to be correctly set before running the script.
"""

from os.path import join
import sys
import getopt
import subprocess
import string
from datetime import datetime
import time
from os import environ

def get_commands(conn_file):
    if not conn_file.startswith("file://"):
        conn_file = "file://" + conn_file

    cmds = []
    cmds += [["TEST IPBUS1.3 UDP"
              [#uhal.tools.ipbus_addr_map
               "python -c \"import uhal.tools.ipbus_addr_map;uhal.tools.ipbus_addr_map.main()\"",
               # SERVER NOT REACHABLE TEST
               "test_dummy_nonreachable.exe -c " + conn_file + " -d dummy.udp",
               # TIMEOUT TEST
               "DummyHardwareUdp.exe --version 1 --port 50001 --delay 2 &> /dev/null &",
               "test_dummy_timeout.exe -c " + conn_file + " -d dummy.udp",
               "pkill -f \"DummyHardwareUdp.exe\"",
               # NORMAL TESTS
               "DummyHardwareUdp.exe --version 1 --port 50001 &> /dev/null &",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1 -i 1000 -p -d ipbusudp-1.3://localhost:50001",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 262144 -i 1000 -p -d ipbusudp-1.3://localhost:50001",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 1 -i 1000 -p -d ipbusudp-1.3://localhost:50001",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 262144 -i 1000 -p -d ipbusudp-1.3://localhost:50001",
               "test_dummy_single.exe -c " + conn_file + " -d dummy.udp",
               "test_dummy_block.exe -c " + conn_file + " -d dummy.udp",
               "test_dummy_docu_examples.exe -c " + conn_file + " -d dummy.docu.udp",
               "test_dummy_check_permissions.exe -c " + conn_file + " -d dummy.udp",
               "test_dummy_hierarchy.exe -c " + conn_file + " -d dummy.udp",
               "test_dummy_multithreaded.exe -c " + conn_file + " -d dummy.udp",
               "test_dummy_metainfo.exe -c " + conn_file + " -d dummy.udp",
               "test_dummy_navigation.exe -c " + conn_file + " -d dummy.udp",
               "test_dummy_rawclient.exe -c " + conn_file + " -d dummy.udp",
               "pkill -f \"DummyHardwareUdp.exe\""]
            ]]

    cmds += [["TEST IPBUS1.3 TCP",
              [# SERVER NOT REACHABLE TESTS
               "test_dummy_nonreachable.exe -c %s -d dummy.tcp" % (conn_file),
               # TIMEOUT TESTS
               "DummyHardwareTcp.exe --version 1 --port 50002 --delay 2 &> /dev/null &",
               "test_dummy_timeout.exe -c %s -d dummy.tcp" % (conn_file),
               "pkill -f \"DummyHardwareTcp.exe\"",
               # NORMAL TESTS
               "DummyHardwareTcp.exe --version 1 --port 50002 &> /dev/null &",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1 -i 1000 -p -d ipbustcp-1.3://localhost:50002",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 262144 -i 1000 -p -d ipbustcp-1.3://localhost:50002",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 1 -i 1000 -p -d ipbustcp-1.3://localhost:50002",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 262144 -i 1000 -p -d ipbustcp-1.3://localhost:50002",
               "test_dummy_single.exe -c %s -d dummy.tcp" % (conn_file),
               "test_dummy_block.exe -c %s -d dummy.tcp" % (conn_file),
               "test_dummy_docu_examples.exe -c %s -d dummy.docu.tcp" % (conn_file),
               "test_dummy_check_permissions.exe -c %s -d dummy.tcp" % (conn_file),
               "test_dummy_hierarchy.exe -c %s -d dummy.tcp" % (conn_file),
               "test_dummy_multithreaded.exe -c %s -d dummy.tcp" % (conn_file),
               "test_dummy_metainfo.exe -c %s -d dummy.tcp" % (conn_file),
               "test_dummy_navigation.exe -c %s -d dummy.tcp" % (conn_file),
               "test_dummy_rawclient.exe -c %s -d dummy.tcp"  % (conn_file),
               "pkill -f \"DummyHardwareTcp.exe\""]
             ]]

    cmds += [["TEST IPBUS1.3 CONTROLHUB",
              [# SERVER NOT REACHABLE TESTS
               "test_dummy_nonreachable.exe -c %s -d dummy.controlhub" % (conn_file),
               "sudo controlhub_start",
               "sudo controlhub_status",
               "test_dummy_nonreachable.exe -c %s -d dummy.controlhub" % (conn_file),
               "sudo controlhub_stop",
               # TIMEOUT TESTS
               "DummyHardwareUdp.exe --version 1 --port 50001 --delay 2 &> /dev/null &",
               "sudo controlhub_start",
               "sudo controlhub_status",
               "test_dummy_timeout.exe -c %s -d dummy.controlhub" % (conn_file),
               "pkill -f \"DummyHardwareUdp.exe\"",
               "sudo controlhub_stop",
               #CONTROL HUB TESTS
               "DummyHardwareUdp.exe --version 1 --port 50001 &> /dev/null &",
               "sudo controlhub_start",
               "sudo controlhub_status",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1 -i 1000 -p -d chtcp-1.3://localhost:10203?target=localhost:50001",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 262144 -i 1000 -p -d chtcp-1.3://localhost:10203?target=localhost:50001",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 1 -i 1000 -p -d chtcp-1.3://localhost:10203?target=localhost:50001",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 262144 -i 1000 -p -d chtcp-1.3://localhost:10203?target=localhost:50001",
               "test_dummy_single.exe -c %s -d dummy.controlhub" % (conn_file),
               "test_dummy_block.exe -c %s -d dummy.controlhub" % (conn_file),
               "test_dummy_docu_examples.exe -c %s -d dummy.docu.controlhub" % (conn_file),
               "test_dummy_check_permissions.exe -c %s -d dummy.controlhub" % (conn_file),
               "test_dummy_hierarchy.exe -c %s -d dummy.controlhub" % (conn_file),
               "test_dummy_multithreaded.exe -c %s -d dummy.controlhub" % (conn_file),
               "test_dummy_metainfo.exe -c %s -d dummy.controlhub" % (conn_file),
               "test_dummy_navigation.exe -c %s -d dummy.controlhub" % (conn_file),
               "test_dummy_rawclient.exe -c %s -d dummy.controlhub" % (conn_file),
               "pkill -f \"DummyHardwareUdp.exe\"",
               "sudo controlhub_stop"]
                ]]

    cmds += [["TEST IPBUS2.0 UDP",
              [#uhal.tools.ipbus_addr_map
               "python -c \"import uhal.tools.ipbus_addr_map;uhal.tools.ipbus_addr_map.main()\"",
               # SERVER NOT REACHABLE TEST
               "test_dummy_nonreachable.exe -c " + conn_file + " -d dummy.udp2",
               # TIMEOUT TEST
               "DummyHardwareUdp.exe --version 2 --port 60001 --delay 2 &> /dev/null &",
               "test_dummy_timeout.exe -c " + conn_file + " -d dummy.udp2",
               "pkill -f \"DummyHardwareUdp.exe\"",
               # NORMAL TESTS
               "DummyHardwareUdp.exe --version 2 --port 60001 &> /dev/null &",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1 -i 1000 -p -d ipbusudp-2.0://localhost:60001",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 262144 -i 1000 -p -d ipbusudp-2.0://localhost:60001",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 1 -i 1000 -p -d ipbusudp-2.0://localhost:60001",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 262144 -i 1000 -p -d ipbusudp-2.0://localhost:60001",
               "test_dummy_single.exe -c " + conn_file + " -d dummy.udp2",
               "test_dummy_block.exe -c " + conn_file + " -d dummy.udp2",
               "test_dummy_docu_examples.exe -c " + conn_file + " -d dummy.docu.udp2",
               "test_dummy_check_permissions.exe -c " + conn_file + " -d dummy.udp2",
               "test_dummy_hierarchy.exe -c " + conn_file + " -d dummy.udp2",
               "test_dummy_multithreaded.exe -c " + conn_file + " -d dummy.udp2",
               "test_dummy_metainfo.exe -c " + conn_file + " -d dummy.udp2",
               "test_dummy_navigation.exe -c " + conn_file + " -d dummy.udp2",
               "test_dummy_rawclient.exe -c " + conn_file + " -d dummy.udp2",
               "pkill -f \"DummyHardwareUdp.exe\""]
            ]]

    cmds += [["TEST IPBUS2.0 TCP",
              [# SERVER NOT REACHABLE TESTS
               "test_dummy_nonreachable.exe -c %s -d dummy.tcp2" % (conn_file),
               # TIMEOUT TESTS
               "DummyHardwareTcp.exe --version 2 --port 60002 --delay 2 &> /dev/null &",
               "test_dummy_timeout.exe -c %s -d dummy.tcp2" % (conn_file),
               "pkill -f \"DummyHardwareTcp.exe\"",
               # NORMAL TESTS
               "DummyHardwareTcp.exe --version 2 --port 60002 &> /dev/null &",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1 -i 1000 -p -d ipbustcp-2.0://localhost:60002",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 262144 -i 1000 -p -d ipbustcp-2.0://localhost:60002",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 1 -i 1000 -p -d ipbustcp-2.0://localhost:60002",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 262144 -i 1000 -p -d ipbustcp-2.0://localhost:60002",
               "test_dummy_single.exe -c %s -d dummy.tcp2" % (conn_file),
               "test_dummy_block.exe -c %s -d dummy.tcp2" % (conn_file),
               "test_dummy_docu_examples.exe -c %s -d dummy.docu.tcp2" % (conn_file),
               "test_dummy_check_permissions.exe -c %s -d dummy.tcp2" % (conn_file),
               "test_dummy_hierarchy.exe -c %s -d dummy.tcp2" % (conn_file),
               "test_dummy_multithreaded.exe -c %s -d dummy.tcp2" % (conn_file),
               "test_dummy_metainfo.exe -c %s -d dummy.tcp2" % (conn_file),
               "test_dummy_navigation.exe -c %s -d dummy.tcp2" % (conn_file),
               "test_dummy_rawclient.exe -c %s -d dummy.tcp2"  % (conn_file),
               "pkill -f \"DummyHardwareTcp.exe\""]
             ]]

    cmds += [["TEST IPBUS2.0 CONTROLHUB",
              [# SERVER NOT REACHABLE TESTS
               "test_dummy_nonreachable.exe -c %s -d dummy.controlhub2" % (conn_file),
               "sudo controlhub_start",
               "sudo controlhub_status",
               "test_dummy_nonreachable.exe -c %s -d dummy.controlhub2" % (conn_file),
               "sudo controlhub_stop",
               # TIMEOUT TESTS
               "DummyHardwareUdp.exe --version 2 --port 60001 --delay 2 &> /dev/null &",
               "sudo controlhub_start",
               "sudo controlhub_status",
               "test_dummy_timeout.exe -c %s -d dummy.controlhub2" % (conn_file),
               "pkill -f \"DummyHardwareUdp.exe\"",
               "sudo controlhub_stop",
               #CONTROL HUB TESTS
               "DummyHardwareUdp.exe --version 2 --port 60001 &> /dev/null &",
               "sudo controlhub_start",
               "sudo controlhub_status",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1 -i 1000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 262144 -i 1000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 1 -i 1000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 262144 -i 1000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001",
               "test_dummy_single.exe -c %s -d dummy.controlhub2" % (conn_file),
               "test_dummy_block.exe -c %s -d dummy.controlhub2" % (conn_file),
               "test_dummy_docu_examples.exe -c %s -d dummy.docu.controlhub2" % (conn_file),
               "test_dummy_check_permissions.exe -c %s -d dummy.controlhub2" % (conn_file),
               "test_dummy_hierarchy.exe -c %s -d dummy.controlhub2" % (conn_file),
               "test_dummy_multithreaded.exe -c %s -d dummy.controlhub2" % (conn_file),
               "test_dummy_metainfo.exe -c %s -d dummy.controlhub2" % (conn_file),
               "test_dummy_navigation.exe -c %s -d dummy.controlhub2" % (conn_file),
               "test_dummy_rawclient.exe -c %s -d dummy.controlhub2" % (conn_file),
               "pkill -f \"DummyHardwareUdp.exe\"",
               "sudo controlhub_stop"]
                ]]

    return cmds


def run_command(cmd, verbose=True):
    if cmd.startswith("sudo"):
       cmd = "sudo PATH=$PATH " + cmd[4:]
    print "+ At", datetime.strftime(datetime.now(),"%H:%M:%S"), ": Running ", cmd
    t0 = time.time()
    p  = subprocess.Popen(cmd,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,shell=True)
    stdout = []
    while True:
        nextline = filter(lambda x: x in string.printable,p.stdout.readline())
        if nextline:
            stdout += [nextline]
            if verbose:
                sys.stdout.write(nextline)
                sys.stdout.flush()
        if p.poll() != None and not nextline:
            break

    return stdout, p.poll(), time.time()-t0


if __name__=="__main__":
    # Parse options 
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hvlc:", ["help"])
    except getopt.GetoptError, err:
        print __doc__
        sys.exit(2)

    verbose = False
    conn_file = "/opt/cactus/etc/uhal/tests.dummy_connections.xml"

    for opt, value in opts:
        if opt in ("-h", "--help"):
            print __doc__
            sys.exit(0)
        elif opt == "-v":
            verbose = True
        elif opt == "-c":
            conn_file = value
    
    # Generate list of ipbusX.Y, transport pairs to run tests for
    sections_to_run = []
    ipbus_args      = []
    transport_args  = []
    for a in args:
        a = a.lower()
        if len(a.split(":")) == 2:
            sections_to_run += [tuple(a.split(":",1))]
        elif a.startswith("ipbus"):
            ipbus_args += [a]
        else:
            transport_args += [a]

    if ipbus_args == [] and (transport_args!=[] or sections_to_run==[]):
        ipbus_args = ["ipbus1.3", "ipbus2.0"]
    if transport_args == []:
        transport_args = ["udp", "tcp", "controlhub"]

    for i in ipbus_args:
        for j in transport_args:
            sections_to_run += [(i, j)]

    print "Parsed options are:"
    print "   verbose   :",  verbose
    print "   conn_file :",  conn_file
    print "   sections  :",  sections_to_run 

    print
    print "Environment variables ..."
    run_command("echo $PATH")
    run_command("echo $LD_LIBRARY_PATH")

    # Run the commands
    for section_name, cmds in get_commands(conn_file):
        skip_section = True
        for xx in sections_to_run:
            if min([section_name.lower().count(x) for x in xx])!=0:
                skip_section = False

        print
        print "======================================================================================================================="
        print "-----------------------------------------------------------------------------------------------------------------------"
        if skip_section:
            print " SKIPPING section:", section_name
            continue
        else:
            print " Entering section:", section_name 
 
        for cmd in cmds:
            print
            if verbose:
                print "-----------------------------------------------------------------------------------------------------------------------"
            stdout, exit_code, cmd_duration = run_command(cmd, verbose)

            if len(stdout) and not verbose:
                print stdout[-1].rstrip("\n")
            if exit_code:
                tmp = "+ *** ERRORS OCCURED (exit code = %s, time elapsed = %s seconds) ***" % (exit_code, cmd_duration)
            else:
                tmp = "+ Command completed successfully, time elapsed: %s seconds" % (cmd_duration)
            print tmp

            if cmd.count("controlhub_stop"):
                 time.sleep(5)

