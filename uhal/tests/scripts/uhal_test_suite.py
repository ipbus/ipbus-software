#!/usr/bin/env python

"""
Usage: uhal_test_suite.py [-v] [-l] [-c <path to xml connections file>] [-s <search string for sections to run>]

This script runs the IPbus/uHAL unit tests. The tests for different IPbus versions / uHAL components are separated into different sections.
N.B: All env variables have to be correctly set before running the script.

All options/arguments are optional:
   -l                          : Just list commands (i.e. don't run them)
   -v                          : Print full output of tests (if omitted, only the last stderr/out line from each test command is printed)
   -c /path/to/dummy_conns.xml : Full path to dummy_connections.xml (without file:// prefix)
   -s search_string            : If specified, only sections that contain this string will be run (case-insenstive search).
                                 Otherwise, all sections will be run (see section list at end).
   -p                          : Path containing ControlHub scripts that will be used
   -x                          : Quit on first error

E.g:
  # Limited output of all tests on installed system
  ./uhal_test_suite.py
  # Full output of IPbus 1.3 tests on developer source code
  ./uhal_test_suite.py -v -c /path/to/dummy_connections.xml -s 1.3

"""

from __future__ import print_function

from os.path import join
import fcntl
import sys
import getopt
import subprocess
import string
from datetime import datetime
import time
import os
import signal
import threading
import re
import platform

SOFT_TIMEOUT_S = 570


def get_commands(conn_file, controlhub_scripts_dir, uhal_tools_etc_path):
    """Return full list of all sections/commands in this test suite."""

    if not conn_file.startswith("file://"):
        conn_file = "file://" + conn_file

    if controlhub_scripts_dir is None:
        if "centos-7" in platform.platform():
            controlhub_start  = "sudo systemctl start controlhub"
            controlhub_status = "sudo systemctl status controlhub"
            controlhub_stats  = "/opt/cactus/bin/controlhub_stats"
            controlhub_stop   = "sudo systemctl stop controlhub"
        else:
            controlhub_start  = "/sbin/service controlhub start"
            controlhub_status = "/sbin/service controlhub status"
            controlhub_stats  = "/sbin/service controlhub stats"
            controlhub_stop   = "/sbin/service controlhub stop"
    else:
        controlhub_start = join(controlhub_scripts_dir, "controlhub_start")
        controlhub_status = join(controlhub_scripts_dir, "controlhub_status")
        controlhub_stats = join(controlhub_scripts_dir, "controlhub_stats")
        controlhub_stop = join(controlhub_scripts_dir, "controlhub_stop")

    cmds = []

    cmds += [["TEST CONTROLHUB START",
              [
               'for i in `seq 1 100`; do '+controlhub_start+'; if [ "$?" != "0" ]; then echo "ERROR IN STARTING CONTROLHUB"; fi; '+controlhub_status+'; if [ "$?" != "0" ]; then echo "ERROR: CONTROLHUB SHOULD HAVE ALREADY STARTED"; fi; '+controlhub_stop+'; done',
#               'for i in `seq 1 500`; do '+controlhub_start+'; if [ "$?" != "0" ]; then echo "ERROR IN STARTING CONTROLHUB"; fi; '+controlhub_status+'; if [ "$?" != "0" ]; then echo "ERROR: CONTROLHUB SHOULD HAVE ALREADY STARTED"; fi; '+controlhub_stop+'; done'
              ]
            ]]

    cmds += [["TEST IPBUS 1.3 UDP",
              ["run_uhal_tests.exe -c %s --run_test=ipbusudp_1_3 --log_level=test_suite" % (conn_file)]
            ]]

    cmds += [["TEST IPBUS 1.3 TCP",
              ["run_uhal_tests.exe -c %s --run_test=ipbustcp_1_3 --log_level=test_suite" % (conn_file)]
             ]]

    cmds += [["TEST IPBUS 1.3 CONTROLHUB",
              [controlhub_start,
               controlhub_status,
               "run_uhal_tests.exe -c %s --run_test=chtcp_1_3 --log_level=test_suite" % (conn_file),
               controlhub_stats,
               controlhub_stop]
                ]]

    cmds += [["TEST IPBUS 2.0 UDP",
              ["run_uhal_tests.exe -c %s --run_test=ipbusudp_2_0 --log_level=test_suite" % (conn_file)]
            ]]

    cmds += [["TEST IPBUS 2.0 TCP",
              ["run_uhal_tests.exe -c %s --run_test=ipbustcp_2_0 --log_level=test_suite" % (conn_file)]
             ]]

    cmds += [["TEST IPBUS 2.0 CONTROLHUB - normal",
              [controlhub_start,
               controlhub_status,
               "run_uhal_tests.exe -c %s --run_test=chtcp_2_0 --log_level=test_suite" % (conn_file),
               controlhub_stats,
               controlhub_stop]
                ]]

    cmds += [["TEST IPBUS 2.0 CONTROLHUB - light packet loss",
              [# Setup
               "sudo /sbin/tc -s qdisc ls dev lo",
               "sudo /sbin/tc qdisc del dev lo root ; sudo /sbin/tc -s qdisc ls dev lo",
               "DummyHardwareUdp.exe --version 2 --port 60001",
               controlhub_start,
               controlhub_status,
               controlhub_stats,
               # Main tests - no packet loss reference
               "PerfTester.exe t BandwidthTx -b 0x01 -w 1   -i 50000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001",
               "PerfTester.exe t BandwidthTx -b 0x01 -w 1   -i 50000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001",
               "PerfTester.exe t BandwidthTx -b 0x01 -w 1   -i 50000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001",
               controlhub_stats,
               "sudo /sbin/tc qdisc add dev lo root netem loss 0.0001%",
               "sudo /sbin/tc -s qdisc ls dev lo",
               # Main tests - no packet loss reference
               "PerfTester.exe t BandwidthTx -b 0x01 -w 1   -i 50000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001",
               "PerfTester.exe t BandwidthTx -b 0x01 -w 1   -i 50000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001",
               "PerfTester.exe t BandwidthTx -b 0x01 -w 1   -i 50000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001",
               controlhub_stats,
               # Main tests
               "sudo /sbin/tc qdisc change dev lo root netem loss 0.5%",
               "sudo /sbin/tc -s qdisc ls dev lo",
               "PerfTester.exe t BandwidthTx -b 0x01 -w 1   -i 50000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001",
               "PerfTester.exe t BandwidthTx -b 0x01 -w 1   -i 50000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001",
               "PerfTester.exe t BandwidthTx -b 0x01 -w 1   -i 50000 -p -d chtcp-2.0://localhost:10203?target=localhost:60001",
               controlhub_stats,
               # Clean up
               "sudo /sbin/tc qdisc del dev lo root",
               "sudo /sbin/tc -s qdisc ls dev lo",
               "KILL_BACKGROUND_PROCS",
               controlhub_stop]
                ]]

    cmds += [["TEST IPBUS 2.0 PCIe",
              ["run_uhal_tests.exe -c %s --run_test=ipbuspcie_2_0 --log_level=test_suite" % (conn_file)]
            ]]

    cmds += [["TEST PYCOHAL",
              ["DummyHardwareUdp.exe --version 1 --port 50001",
               sys.executable + " $(which test_pycohal) -c %s -v" % (conn_file),
               "KILL_BACKGROUND_PROCS"]
            ]]

    cmds += [["TEST uHAL GUI",
              [sys.executable + " -c \"import uhal.gui.test.test_uhal_gui;uhal.gui.test.test_uhal_gui.main()\""]
            ]]

    uhal_tools_template_vhdl = join( uhal_tools_etc_path, 'ipbus_addr_decode.vhd' )
    uhal_tools_test_inputs_dir = join( uhal_tools_etc_path, 'tests' )
    uhal_tools_test_refs_dir = join( uhal_tools_etc_path, 'tests', 'refs' )
    cmds += [["TEST uHAL TOOLS",
              [#uhal.tools.ipbus_addr_map
               sys.executable + " $(which gen_ipbus_addr_decode) -t %s %s" % (uhal_tools_template_vhdl, join( uhal_tools_test_inputs_dir, 'addr_table_a.xml')),
               "diff -I '^-- START' %s %s" % (join( uhal_tools_test_refs_dir, 'ipbus_decode_addr_table_a.vhd' ) , 'ipbus_decode_addr_table_a.vhd' ),
               sys.executable + " $(which gen_ipbus_addr_decode) -t %s %s" % (uhal_tools_template_vhdl, join( uhal_tools_test_inputs_dir, 'addr_table_b.xml')),
               "diff -I '^-- START' %s %s" % (join( uhal_tools_test_refs_dir, 'ipbus_decode_addr_table_b.vhd' ) , 'ipbus_decode_addr_table_b.vhd' ),
               sys.executable + " $(which gen_ipbus_addr_decode) -t %s %s" % (uhal_tools_template_vhdl, join( uhal_tools_test_inputs_dir, 'addr_table_c.xml')),
               "diff -I '^-- START' %s %s" % (join( uhal_tools_test_refs_dir, 'ipbus_decode_addr_table_c.vhd' ) , 'ipbus_decode_addr_table_c.vhd' ),
              ]
            ]]

    return cmds


def cleanup_cmds():
    """Return list of cleanup commands to be used in case test script interupted - e.g. by ctrl-c"""

    cmds = ["controlhub_stop",
            "sudo /sbin/tc qdisc del dev lo root"]
    return cmds


def run_cleanup_commands(background_processes):
    """Runs the cleanup commands"""
    print()
    for p in background_procs:
        print("  Terminating PID", p.pid)
        os.killpg(p.pid, signal.SIGTERM)

    for cmd in cleanup_cmds():
       print("+ Running cleanup command: ", cmd)
       run_command(cmd, False)


def get_sections():
    """Return list of all sections of commands defined in this test suite"""
    return [section for section, cmds in get_commands("", "", "")]


def get_controlhub_status( cmd ):
  if ("dummy.controlhub" in cmd) or ("chtcp" in cmd ):
    StdOut, ExitCode , Time = run_command("controlhub_status", False)
    if ExitCode:
      return "controlhub_status failed, "
    else:
      return StdOut[0].strip() + ", "
  else:
    return ""

def get_dummyhardware_status( cmd ):
  if ("dummy.controlhub" in cmd) or ("chtcp" in cmd) or ("dummy.udp" in cmd) or ("ipbusudp" in cmd) or ("test_pycohal" in cmd):
    StdOut, ExitCode , Time = run_command( 'ps aux | grep -ce "[D]ummyHardwareUdp"' , False)
  else:
    StdOut, ExitCode , Time = run_command( 'ps aux | grep -ce "[D]ummyHardwareTcp"' , False)

  StdOut = StdOut[0].strip()
  if StdOut == '1':
    return "1 DummyHardware running"
  else:
    return StdOut+" DummyHardwares running"


def convert_to_utf(byte_string):
    if sys.version_info[0] > 2:
        return str(byte_string, 'utf-8')
    else:
        return byte_string


def background_run_command(cmd , proc_list):
    """
    Run command in separate thread, ignoring stdout and stderr
    """
    def runInThread(cmd , proc_list):
      t0 = time.time()
      p = subprocess.Popen( cmd,stdout=subprocess.PIPE, stderr=subprocess.STDOUT, stdin=None, shell=True, preexec_fn=os.setsid )

      proc_list.append( p )
      p.wait()

      exit_code = p.poll()
      cmd_duration = time.time()-t0
      if exit_code == -15:
        print("+ Background command was terminated '%s' (time elapsed = %s seconds)" % (cmd , cmd_duration))
      elif exit_code:
        msg = "+ *** ERROR OCCURED (exit code = %s, time elapsed = %s seconds) IN BACKGROUND COMMAND '%s' ***\n" % (exit_code, cmd_duration, cmd)
        msg += "  ----- START OF STDOUT -----\n"
        msg += convert_to_utf(p.stdout.read())
        msg += "\n  ----- END OF STDOUT -----"
        print(msg)
      else:
        print("+ Background command '%s' completed successfully, time elapsed: %s seconds" % (cmd, cmd_duration))

    print("+ At", datetime.strftime(datetime.now(),"%H:%M:%S"), ": Background running ", cmd)
    thread = threading.Thread( target=runInThread , args=( cmd , proc_list ) )
    thread.start()


def run_command(cmd, verbose=True):
    """
    Run command, printing stdout and stderr to stdout if argument verbose = True
    Additionally, command will be killed if it produces no output over a period of
    SOFT_TIMEOUT_S seconds
    """

    if cmd.startswith("sudo"):
      cmd = "sudo PATH=$PATH " + cmd[4:]
#    print "+ At", datetime.strftime(datetime.now(),"%H:%M:%S"), ": Running ", cmd
    t0 = time.time()

    p  = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, stdin=None, shell=True, preexec_fn=os.setsid)
    try:
      f1 = fcntl.fcntl(p.stdout, fcntl.F_GETFL)
      fcntl.fcntl(p.stdout, fcntl.F_SETFL, f1 | os.O_NONBLOCK)

      stdout = []
      last = time.time()


      while True:
          current = time.time()

          try:
              nextline = p.stdout.readline()
              last = time.time()

              if nextline:
                  stdout += [convert_to_utf(nextline)]
                  if verbose:
                      sys.stdout.write(convert_to_utf(nextline))
                      sys.stdout.flush()
              elif (p.poll() is not None):
                  nextlines = [convert_to_utf(s) for s in p.stdout.readlines()]
                  if verbose:
                      for l in nextlines:
                          sys.stdout.write(l)
                          sys.stdout.flush()
                  stdout += nextlines
                  break

          except IOError:
              time.sleep(0.1)

              if (current-last) > SOFT_TIMEOUT_S:
                  print("+ ERROR : unresponsive command, missing output for %d sec" % (SOFT_TIMEOUT_S))
                  os.killpg(p.pid, signal.SIGTERM)
                  return stdout, -1, time.time()-t0

    except KeyboardInterrupt:
        print("+ Ctrl-C detected.")
        os.killpg(p.pid, signal.SIGTERM)
        print("  Press Ctrl-C again in next 5 seconds to stop script.")
        time.sleep(5)
        return stdout, -2, time.time()-t0

    return stdout, p.poll(), time.time()-t0


if __name__=="__main__":
    # Parse options
    try:
        opts, args = getopt.getopt(sys.argv[1:], "xhvlp:s:c:", ["help"])
    except getopt.GetoptError as err:
        print(__doc__)
        sys.exit(2)

    run_cmds = True
    verbose  = False
    conn_file = "/opt/cactus/etc/uhal/tests/dummy_connections.xml"
    section_search_str = None
    quit_on_error = False
    controlhub_scripts_dir = None

    stdout = []
    exit_code = 0
    cmd_duration = 0


    for opt, value in opts:
        if opt in ("-h", "--help"):
            print(__doc__)
            print("The sections in this suite are:")
            for s in get_sections():
                print("  ", s)
            sys.exit(0)
        elif opt == "-l":
            run_cmds = False
        elif opt == "-v":
            verbose = True
        elif opt == "-c":
            conn_file = value
        elif opt == "-s":
            section_search_str = value
        elif opt == "-x":
            quit_on_error = True
        elif opt == "-p":
            controlhub_scripts_dir = value

    if len(args) != 0:
        print("Incorrect usage!")
        print(__doc__)
        sys.exit(1)

    print("Configuration ... ")
    print("  connections file:       ",  conn_file)

    # Find directory for controlhub commands
    if controlhub_scripts_dir is None:
        which_controlhub_status = run_command("which controlhub_status", False)
        if which_controlhub_status[1]:
            controlhub_scripts_dir = "/opt/cactus/bin"
        else:
            controlhub_scripts_dir = os.path.dirname( which_controlhub_status[0][0].rstrip("\n") )
        if controlhub_scripts_dir.startswith("/opt/cactus/bin"):
            controlhub_scripts_dir = None
    print('  ControlHub scripts dir: ', controlhub_scripts_dir)

    # Get uhal tools template file name
    which_gen_ipbus_addr_decode = run_command("which gen_ipbus_addr_decode", False)
    uhal_tools_dir = "/opt/cactus/bin/uhal/tools" if which_gen_ipbus_addr_decode[1] else os.path.dirname ( which_gen_ipbus_addr_decode[0][0].rstrip('\n') )
    if uhal_tools_dir.endswith("bin/uhal/tools"): # RPM-installed code
        uhal_tools_dir = uhal_tools_dir[:-14]
    else: # Checked-out code
        uhal_tools_dir = os.path.split( uhal_tools_dir )[0]
    uhal_tools_etc_path = join( uhal_tools_dir, 'etc', 'uhal', 'tools')

    def skip_section(name):
        if (section_search_str is None):
            return False
        elif section_search_str.lower() in name.lower():
            return False
        else:
            return True
    sections_cmds_to_run = [(name, cmds) for (name, cmds) in get_commands(conn_file, controlhub_scripts_dir, uhal_tools_etc_path) if not skip_section(name)]
    sections_skipped     = [name for (name, cmds) in get_commands(conn_file, controlhub_scripts_dir, uhal_tools_etc_path) if skip_section(name)]

    if run_cmds:
        for env_var in ["PATH", "LD_LIBRARY_PATH"]:
            value = run_command("echo $" + env_var, verbose=False)[0][0]
            print(" $", env_var, "is: ", value.strip("\n"))

        if section_search_str is None:
            print("\nAll sections will be run.")
        elif len(sections_cmds_to_run) != 0:
            print("\nThe following sections will be skipped:")
            for name in sections_skipped:
                print("   ", name)
        else:
            print("No sections matched the search string \"" + section_search_str + "\"")

    else:
        print("\nN.B: Commands will only be listed, not run")

    background_procs = []

    # Run the commands
    nr_cmds_run = 0
    nr_cmds_err = 0
    try:
        for section_name, cmds in sections_cmds_to_run:
            print ("\n\n" + ("=" * 120))
            print ("-" * 120)
            print ("  --> Section:", section_name)

            for cmd in cmds:
                if run_cmds == False:
                    print(cmd)
                    continue


                print()
                if verbose:
                    print("-" * 120)

                nr_cmds_run += 1

                if cmd == "KILL_BACKGROUND_PROCS":
                    print("+ At", datetime.strftime(datetime.now(),"%H:%M:%S"), ": Killing background processes")
                    while len(background_procs) > 0:
                        p = background_procs.pop()
                        print("  Terminating PID", p.pid)
                        os.killpg(p.pid, signal.SIGTERM)
                        p.wait()
                    time.sleep(2)
                    continue

                if cmd.startswith( "DummyHardware" ):
                    background_run_command( cmd , background_procs )
                    print("     (Brief sleep after dummy H/W command)")
                    time.sleep(0.5)
                    continue

                print("+ At", datetime.strftime(datetime.now(),"%H:%M:%S"), ": Running ", cmd)
                stdout, exit_code, cmd_duration = run_command(cmd, verbose)

                if len(stdout) and not verbose:
                    print(stdout[-1].rstrip("\n"))
                if exit_code:
                    nr_cmds_err += 1
                    split_name_list = re.split('(\.exe)',cmd)
                    split_name = split_name_list[0]
                    if len( split_name_list ) > 1:
                        split_name = split_name + split_name_list[1]

                    print("+ *** ERROR OCCURED (section = '%s', test = '%s', exit code = %s, time elapsed = %s seconds, %s%s ) ***" % (section_name, split_name , exit_code, cmd_duration , get_controlhub_status( cmd ) , get_dummyhardware_status( cmd ) ))

                    if quit_on_error:
                        print("+ Quitting as an error was observed and the '-x' flag was specified by the user")
                        run_cleanup_commands(background_procs)
                        sys.exit(exit_code)
                else:
                    print("+ Command completed successfully, time elapsed: %s seconds" % (cmd_duration))

    except KeyboardInterrupt:
        print("\n+ Ctrl-C detected.")
        run_cleanup_commands(background_procs)
        sys.exit()

    if run_cmds:
        run_cleanup_commands(background_procs)

        print("\n   TEST SUITE COMPLETED! ", nr_cmds_run, "commands run," , nr_cmds_err, "errors (non-zero exit codes)")
        if nr_cmds_err > 0:
            sys.exit(1)

