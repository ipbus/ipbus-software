#!/bin/env python
"""
Usage: uhal_test_suite.py [-v] [-l] [-c <path to xml connections file>] [-s <search string for sections to run>]

This script runs all of the IPbus/uHAL tests (either on an installed system or using checked-out source code).
The tests for different IPbus versions / different uHAL components are separated into different sections.

All options/arguments are optional:
   -l                          : Just list commands (i.e. don't run them)
   -v                          : Print all output of tests (if omitted, only the results summary for each test command is printed)
   -c /path/to/dummy_conns.xml : Full path to dummy_connections.xml (without file:// prefix)
   -s search_string            : If specified, only sections that contain this string will be run (case-insenstive search).
                                 Otherwise, all sections will be run (see section list at end).

E.g:
  # Limited output of all tests on installed system
  ./uhal_test_suite.py
  # Full output of IPbus 1.3 tests on developer source code
  ./uhal_test_suite.py -v -c /path/to/dummy_connections.xml -s 1.3

N.B: All env variables have to be correctly set before running the script.
"""

from os.path import join
import fcntl
import sys
import getopt
import subprocess
import string
from datetime import datetime
import time
import os

SOFT_TIMEOUT_S = 570

def get_commands(conn_file):
    """Return full list of all sections/commands in this test suite."""

    if not conn_file.startswith("file://"):
        conn_file = "file://" + conn_file

    cmds = []
    cmds += [["TEST IPBUS 1.3 UDP",
              [# SERVER NOT REACHABLE TEST
               "test_dummy_nonreachable.exe -c %s -d dummy.udp" % (conn_file),
               # TIMEOUT TEST
               "DummyHardwareUdp.exe --version 1 --port 50001 --delay 2 &> /dev/null &",
               "test_dummy_timeout.exe -c %s -d dummy.udp" % (conn_file),
               "pkill -f \"DummyHardwareUdp.exe\"",
               # NORMAL TESTS
               "DummyHardwareUdp.exe --version 1 --port 50001 &> /dev/null &",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1 -i 1000 -p -d ipbusudp-1.3://localhost:50001",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 262144 -i 1000 -p -d ipbusudp-1.3://localhost:50001",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 1 -i 1000 -p -d ipbusudp-1.3://localhost:50001",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 262144 -i 1000 -p -d ipbusudp-1.3://localhost:50001",
               "test_dummy_single.exe -c %s -d dummy.udp" % (conn_file),
               "test_dummy_block.exe -c %s -d dummy.udp" % (conn_file),
               "test_dummy_docu_examples.exe -c %s -d dummy.docu.udp" % (conn_file),
               "test_dummy_check_permissions.exe -c %s -d dummy.udp" % (conn_file),
               "test_dummy_hierarchy.exe -c %s -d dummy.udp" % (conn_file),
               "test_dummy_multithreaded.exe -c %s -d dummy.udp" % (conn_file),
               "test_dummy_metainfo.exe -c %s -d dummy.udp" % (conn_file),
               "test_dummy_navigation.exe -c %s -d dummy.udp" % (conn_file),
               "test_dummy_rawclient.exe -c %s -d dummy.udp" % (conn_file),
               "test_pycohal -c %s -v" % (conn_file),
               "pkill -f \"DummyHardwareUdp.exe\"",
               "DummyHardwareUdp.exe --version 1 --port 50001 &> /dev/null &",
               "test_random.exe -c %s -d dummy.udp -t 300" % (conn_file),
               "pkill -f \"DummyHardwareUdp.exe\""]
            ]]

    cmds += [["TEST IPBUS 1.3 TCP",
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
               "pkill -f \"DummyHardwareTcp.exe\"",
               "DummyHardwareTcp.exe --version 1 --port 50002 &> /dev/null &",
               "test_random.exe -c %s -d dummy.tcp -t 300" % (conn_file),
               "pkill -f \"DummyHardwareTcp.exe\""]
             ]]

    cmds += [["TEST IPBUS 1.3 CONTROLHUB",
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
               "DummyHardwareUdp.exe --version 1 --port 50001 &> /dev/null &",
               "test_dummy_random.exe -c %s -d dummy.controlhub -t 300" % (conn_file),
               "pkill -f \"DummyHardwareUdp.exe\"",
               "sudo controlhub_stats",
               "sudo controlhub_stop"]
                ]]

    cmds += [["TEST IPBUS 2.0 UDP",
              [# SERVER NOT REACHABLE TEST
               "test_dummy_nonreachable.exe -c %s -d dummy.udp2" % (conn_file),
               # TIMEOUT TEST
               "DummyHardwareUdp.exe --version 2 --port 60001 --delay 2 &> /dev/null &",
               "test_dummy_timeout.exe -c %s -d dummy.udp2" % (conn_file),
               "pkill -f \"DummyHardwareUdp.exe\"",
               # NORMAL TESTS
               "DummyHardwareUdp.exe --version 2 --port 60001 &> /dev/null &",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1 -i 1000 -p -d ipbusudp-2.0://localhost:60001",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 262144 -i 1000 -p -d ipbusudp-2.0://localhost:60001",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 1 -i 1000 -p -d ipbusudp-2.0://localhost:60001",
               "PerfTester.exe -t BandwidthRx -b 0x01 -w 262144 -i 1000 -p -d ipbusudp-2.0://localhost:60001",
               "test_dummy_single.exe -c %s -d dummy.udp2" % (conn_file),
               "test_dummy_block.exe -c %s -d dummy.udp2" % (conn_file),
               "test_dummy_docu_examples.exe -c %s -d dummy.docu.udp2" % (conn_file),
               "test_dummy_check_permissions.exe -c %s -d dummy.udp2" % (conn_file),
               "test_dummy_hierarchy.exe -c %s -d dummy.udp2" % (conn_file),
               "test_dummy_multithreaded.exe -c %s -d dummy.udp2" % (conn_file),
               "test_dummy_metainfo.exe -c %s -d dummy.udp2" % (conn_file),
               "test_dummy_navigation.exe -c %s -d dummy.udp2" % (conn_file),
               "test_dummy_rawclient.exe -c %s -d dummy.udp2" % (conn_file),
               "pkill -f \"DummyHardwareUdp.exe\"",
               "DummyHardwareUdp.exe --version 2 --port 60001 &> /dev/null &",
               "test_random.exe -c %s -d dummy.udp2 -t 300" % (conn_file),
               "pkill -f \"DummyHardwareUdp.exe\""]
            ]]

    cmds += [["TEST IPBUS 2.0 TCP",
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
               "pkill -f \"DummyHardwareTcp.exe\"",
               "DummyHardwareTcp.exe --version 2 --port 60002 &> /dev/null &",
               "test_random.exe -c %s -d dummy.tcp2 -t 300" % (conn_file),
               "pkill -f \"DummyHardwareTcp.exe\""]
             ]]

    cmds += [["TEST IPBUS 2.0 CONTROLHUB",
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
               "sleep 30; DummyHardwareUdp.exe --version 2 --port 60001 &> /dev/null &",
               "test_random.exe -c %s -d dummy.controlhub2 -t 300" % (conn_file),
               "pkill -f \"DummyHardwareUdp.exe\"",
               "controlhub_stats",
               "sudo controlhub_stop"]
                ]]

    cmds += [["TEST IPBUS 2.0 CONTROLHUB with packet loss",
              [# Setup
               "sudo /sbin/tc -s qdisc ls dev lo",
               "sudo /sbin/tc qdisc del dev lo root",
               "DummyHardwareUdp.exe --version 2 --port 60001 &> /dev/null &",
               "sudo controlhub_start",
               "sudo controlhub_status",
               "controlhub_stats",
               # Main tests - no packet loss reference
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1   -i 150000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001 -v",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1   -i 150000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001 -v",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1   -i 150000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001 -v",
               "controlhub_stats",
               "sudo /sbin/tc qdisc add dev lo root netem loss 0.0001%",
               "sudo /sbin/tc -s qdisc ls dev lo",
               # Main tests - no packet loss reference
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1   -i 150000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001 -v",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1   -i 150000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001 -v",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1   -i 150000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001 -v",
               "controlhub_stats",
               # Main tests
               "sudo /sbin/tc qdisc change dev lo root netem loss 0.1%",
               "sudo /sbin/tc -s qdisc ls dev lo",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1   -i 150000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001 -v",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1   -i 150000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001 -v",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1   -i 150000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001 -v",
               "PerfTester.exe -t BandwidthTx -b 0x01 -w 1   -i 150000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001 -v",
               "controlhub_stats", 
               # Clean up
               "sudo /sbin/tc qdisc del dev lo root",
               "sudo /sbin/tc -s qdisc ls dev lo",
               "pkill -f \"DummyHardwareUdp.exe\"",
               "cat /var/log/controlhub.log",
               "sudo controlhub_stop"]
                ]]

    cmds += [["TEST uHAL GUI",
              ["python -c \"import uhal.gui.test.test_uhal_gui;uhal.gui.test.test_uhal_gui.main()\""]
            ]]

    cmds += [["TEST uHAL TOOLS",
              [#uhal.tools.ipbus_addr_map
               "python -c \"import uhal.tools.ipbus_addr_map;uhal.tools.ipbus_addr_map.main()\""]
            ]]

    return cmds


def cleanup_cmds():
    """Return list of cleanup commands to be used in case test script interupted - e.g. by ctrl-c"""
    
    cmds = ["pkill -f \"DummyHardwareUdp.exe\"",
            "pkill -f \"DummyHardwareTcp.exe\"",
            "sudo controlhub_stop",
            "sudo /sbin/tc qdisc del dev lo root"]
    return cmds


def get_sections():
    """Return list of all sections of commands defined in this test suite"""

    return [section for section, cmds in get_commands("")]


def run_command(cmd, verbose=True):
    """
    Run command, printing stdout and stderr to stdout if argument verbose = True
    Additionally, command will be killed if it produces no output over a period of
    SOFT_TIMEOUT_S seconds
    """

    if cmd.startswith("sudo"):
       cmd = "sudo PATH=$PATH " + cmd[4:]
    print "+ At", datetime.strftime(datetime.now(),"%H:%M:%S"), ": Running ", cmd
    t0 = time.time()

    p  = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.STDOUT, stdin=None, shell=True)
    f1 = fcntl.fcntl(p.stdout, fcntl.F_GETFL)
    fcntl.fcntl(p.stdout, fcntl.F_SETFL, f1 | os.O_NONBLOCK)

    stdout = []
    last = time.time()

    while True:
        current = time.time()

        try:
            nextline = p.stdout.readline()
            if not nextline:
                break

            last = time.time()
            stdout += [nextline]
            if verbose:
                sys.stdout.write(nextline)
                sys.stdout.flush()

            if p.poll() != None:
                break

        except IOError:
            time.sleep(0.1)
            
            if (current-last) > SOFT_TIMEOUT_S:
                print "+ ERROR : unresponsive command, missing output for %d sec" % (SOFT_TIMEOUT_S)
                return stdout, -1, time.time()-t0

    return stdout, p.poll(), time.time()-t0


if __name__=="__main__":
    # Parse options 
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hvls:c:", ["help"])
    except getopt.GetoptError, err:
        print __doc__
        sys.exit(2)

    run_cmds = True
    verbose  = False
    conn_file = "/opt/cactus/etc/uhal/tests/dummy_connections.xml"
    section_search_str = None

    for opt, value in opts:
        if opt in ("-h", "--help"):
            print __doc__
            print "The sections in this suite are:"
            for s in get_sections():
                print "  ", s
            sys.exit(0)
        elif opt == "-l":
            run_cmds = False
        elif opt == "-v":
            verbose = True
        elif opt == "-c":
            conn_file = value
        elif opt == "-s":
            section_search_str = value

    if len(args) != 0:
        print "Incorrect usage!"
        print __doc__
        sys.exit(1)

    print "Parsed options are:"
    print "   verbose   :",  verbose
    print "   conn_file :",  conn_file
    if section_search_str is None:
        print "All sections will be run"
    else:
        print 'Only sections whose names contain "'+section_search_str+'" will be run'

    print
    if run_cmds:
        print "Environment variables ..."
        run_command("echo $PATH")
        run_command("echo $LD_LIBRARY_PATH")
    else:
        print "N.B: Commands will only be listed, not run"

    # Run the commands
    try:
        for section_name, cmds in get_commands(conn_file):
            if section_search_str is None:
               skip_section = False
            else:
               skip_section = not (section_search_str.lower() in section_name.lower())

            print
            print "======================================================================================================================="
            print "-----------------------------------------------------------------------------------------------------------------------"
            if skip_section:
                print " SKIPPING section:", section_name
                continue
            else:
                print " Entering section:", section_name 
            print

            for cmd in cmds:
                if run_cmds == False:
                    print cmd
                    continue

                print
                if verbose:
                    print "-----------------------------------------------------------------------------------------------------------------------"
                stdout, exit_code, cmd_duration = run_command(cmd, verbose)

                if len(stdout) and not verbose:
                    print stdout[-1].rstrip("\n")
                if exit_code:
                    print "+ *** ERROR OCCURED (exit code = %s, time elapsed = %s seconds) ***" % (exit_code, cmd_duration)
                else:
                    print "+ Command completed successfully, time elapsed: %s seconds" % (cmd_duration)

                if "DummyHardware" in cmd:
                    print "     (Brief sleep after dummy H/W command)"
                    time.sleep(0.5)

    except KeyboardInterrupt:
        print 
        print "+ Ctrl-C detected. Running cleanup commands ..."
        for cmd in cleanup_cmds():
            run_command(cmd, False)
