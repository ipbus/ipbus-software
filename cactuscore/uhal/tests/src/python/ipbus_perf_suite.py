#!/bin/env python
"""
Usage: ipbus_perf_suite.py <config file>

This script runs the standard set of IPbus performance & robostness measurements.

N.B: You must set LD_LIBRARY_PATH and PATH appropriately before this 
     [So that this script can be run for checked-out sources (during dev) or installed RPMs.]
"""

from datetime import datetime
import fcntl
import getpass
import os
import paramiko
import re
import signal
import subprocess
import sys
import time


####################################################################################################
#  GLOBAL OPTIONS

TEST_CMD_TIMEOUT_S = 240

CH_PC_NAME = 'pc-e1x06-36-01'
CH_PC_USER = 'tsw'
CH_PC_ENV  = {'PATH':os.environ['PATH'],
              'LD_LIBRARY_PATH':os.environ['LD_LIBRARY_PATH'] 
              }

TARGETS = ['amc-e1a12-19-09:50001',
           'amc-e1a12-19-10:50001',
           'amc-e1a12-19-04:50001']


###################################################################################################
#  SETUP LOGGING

import logging

SCRIPT_LOGGER = logging.getLogger("ipbus_perf_suite_log")

SCRIPT_LOG_HANDLER = logging.StreamHandler(sys.stdout)
SCRIPT_LOG_FORMATTER = logging.Formatter("%(message)s")
SCRIPT_LOG_HANDLER.setFormatter(SCRIPT_LOG_FORMATTER)

SCRIPT_LOGGER.addHandler(SCRIPT_LOG_HANDLER)
SCRIPT_LOGGER.setLevel(logging.WARNING)

####################################################################################################
#  EXCEPTION CLASSES

class CommandHardTimeout(Exception):
    '''Exception thrown when command spends too long running and has to be killed by the script'''
    def __init__(self, cmd, timeout):
        self.cmd = cmd
        self.timeout = timeout
    def __str__(self):
        return "Command '" + self.cmd + "', timeout was " + str(self.timeout) + " seconds"

class CommandBadExitCode(Exception):
    '''Exception thrown when command returns a non-zero exit code'''
    def __init__(self, cmd, exit_code, output):
        self.cmd = cmd
        self.value = exit_code
        self.output = output
    def __str__(self):
        return "Exit code " + str(self.value) + " from command '" + self.cmd + "'\nOutput was:\n" + self.output


####################################################################################################
#  COMMAND-RUNNING/PARSING FUNCTIONS

def run_command(cmd, ssh_client=None):
  """
  Run command, returning tuple of exit code and stdout/err.
  The command will be killed if it takes longer than TEST_CMD_TIMEOUT_S
  """
  if cmd.startswith("sudo"):
      cmd = "sudo PATH=$PATH " + cmd[4:]

  if ssh_client is None:
    SCRIPT_LOGGER.info("Running (locally): "+cmd)
    t0 = time.time()

    p  = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.STDOUT, stdin=None, shell=True, preexec_fn=os.setsid)
    try:
      f1 = fcntl.fcntl(p.stdout, fcntl.F_GETFL)
      fcntl.fcntl(p.stdout, fcntl.F_SETFL, f1 | os.O_NONBLOCK)

      stdout = ""
      last = time.time()

      while True:
          current = time.time()

          try:
              nextline = p.stdout.readline()
              if not nextline:
                  break

              last = time.time()
              stdout += nextline

          except IOError:
              time.sleep(0.1)
             
              if (current-t0) > TEST_CMD_TIMEOUT_S:
                  os.killpg(p.pid, signal.SIGTERM)
                  raise CommandHardTimeout(cmd, TEST_CMD_TIMEOUT_S)

    except KeyboardInterrupt:
        print "+ Ctrl-C detected."
        os.killpg(p.pid, signal.SIGTERM)
        raise KeyboardInterrupt

    exit_code = p.poll()
    if exit_code:
        raise CommandBadExitCode(cmd, exit_code, stdout)

    return exit_code, stdout

  else:
    for env_var, value in CH_PC_ENV.iteritems():
        cmd = "export " + env_var + "=" + value + " ; " + cmd
    SCRIPT_LOGGER.info("Running (remotely): "+cmd)
    stdin, stdout, stderr = ssh_client.exec_command(cmd)
    exit_code = stdout.channel.recv_exit_status()
    output = "".join( stdout.readlines() ) + "".join( stderr.readlines() )
   
    SCRIPT_LOGGER.info("Output is ...\n"+output)
 
    if exit_code: 
        raise CommandBadExitCode(cmd, exit_code, output)

    return exit_code, output


def run_perftester(uri, test="BandwidthTx", width=1, iterations=50000, perItDispatch=True):
    """Run PerfTester.exe, and return tuple of latency per iteration (us), and bandwidth (Mb/s)"""
    
    cmd = "PerfTester.exe -t " + test + " -i " + str(iterations) + " -b 0x1000" + " -w " + str(width)
    if perItDispatch:
        cmd += " -p" 
    cmd += (" -d " + uri)

    exit_code, output = run_command(cmd)

    m1 = re.search(r"^Test iteration frequency\s+=\s*([\d\.]+)\s*Hz", output, flags=re.MULTILINE)
    freq = float(m1.group(1))
    m2 = re.search(r"^Average \S+ bandwidth\s+=\s*([\d\.]+)\s*KB/s", output, flags=re.MULTILINE)
    bandwidth = float(m2.group(1)) / 125.0

    SCRIPT_LOGGER.info("Parsed: Latency =" + str(1000000.0/freq) + "us/iteration , bandwidth =" + str(bandwidth) + "Mb/s")
    return (1000000.0/freq, bandwidth)


def run_ping(targetname):
    '''Runs unix ping command and returns average latency'''
    #TODO
    return -99.9


def ssh_into(hostname, username):
    try: 
        passwd = getpass.getpass('Password for ' + username + '@' + hostname + ': ')
        client = paramiko.SSHClient()
        client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        client.connect(hostname, username=username, password=passwd)
        return client
    except paramiko.AuthenticationException, e:
        print "Authenication exception caught. Details:", str(e)
        print "Let's try again ...\n"
        return ssh_into(hostname, username)


def start_controlhub(ssh_client=None):
    run_command("sudo controlhub_start", ssh_client=ssh_client)


def stop_controlhub(ssh_client=None):
    run_command("sudo controlhub_stop", ssh_client=ssh_client)


####################################################################################################
#  FUNCTIONS RUNNING SEQUENCES OF TESTS

def measure_latency(target, controhub_ssh_client=None):
    '''Measures latency for single word write to given endpoint'''
    print "\n ---> MEASURING LATENCY TO '" + target + "' <---"
    
    ping_latency = run_ping(target)
    
    udp_latency, udp_bw = run_perftester("ipbusudp-2.0://"+target, width=1, perItDispatch=True)
    
    start_controlhub(controlhub_ssh_client)
    ch_latency, ch_bw = run_perftester("chtcp-2.0://"+CH_PC_NAME+":10203?target="+target, width=1, perItDispatch=True)
    stop_controlhub(controlhub_ssh_client)    

    print "Latencies for "+target
    print "    + Ping           : " + ("%.2f" % ping_latency) + "us"
    print "    + Direct UDP     : " + ("%.2f" % udp_latency) + "us"
    print "    + Via ControlHub : " + ("%.2f" % ch_latency) + "us"


####################################################################################################
#  MAIN

if __name__ == "__main__":
    print " Entered main!"
    
    controlhub_ssh_client = ssh_into( CH_PC_NAME, CH_PC_USER )

    for target in TARGETS:
        measure_latency(target, controlhub_ssh_client)

    sys.exit(0)

    direct_uri = "ipbusudp-2.0://" + TARGETS[0]
    chtcp_uri  = "chtcp-2.0://" + CH_PC_NAME + ":10203?target=" + TARGETS[0]

    run_perftester(direct_uri, width=1, perItDispatch=True)
#    run_perftester(direct_uri, width=335, perItDispatch=True)
#    run_perftester(direct_uri, width=335, iterations=500000, perItDispatch=False)

    start_controlhub(controlhub_ssh_client)
    run_perftester(chtcp_uri, width=1, perItDispatch=True)
    run_perftester(chtcp_uri, width=335, perItDispatch=True)
    run_perftester(chtcp_uri, width=335, iterations=500000, perItDispatch=False)
    stop_controlhub(controlhub_ssh_client)


