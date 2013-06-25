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
import numpy
import os
import paramiko
import re
import signal
from socket import gethostname
import subprocess
import sys
import tempfile
import time


####################################################################################################
#  GLOBAL OPTIONS

TEST_CMD_TIMEOUT_S = 240

CH_PC_NAME = 'pc-e1x06-36-01'
CH_PC_USER = 'tsw'
CH_PC_ENV  = {'PATH':os.environ['PATH'],
              'LD_LIBRARY_PATH':os.environ['LD_LIBRARY_PATH'] 
              }
CH_SYS_CONFIG_LOCATION = "/cactusbuild/trunk/cactuscore/controlhub/RPMBUILD/SOURCES/lib/controlhub/releases/2.0.0/sys.config"

TARGETS = [#'amc-e1a12-19-09:50001',
           'amc-e1a12-19-10:50001',
           'amc-e1a12-19-04:50001']


###################################################################################################
#  SETUP LOGGING

import logging

SCRIPT_LOGGER = logging.getLogger("ipbus_perf_suite_log")

SCRIPT_LOG_HANDLER = logging.StreamHandler(sys.stdout)
SCRIPT_LOG_FORMATTER = logging.Formatter("%(asctime)-15s [%(thread)x] %(levelname)s > %(message)s")
SCRIPT_LOG_HANDLER.setFormatter(SCRIPT_LOG_FORMATTER)

SCRIPT_LOGGER.addHandler(SCRIPT_LOG_HANDLER)
SCRIPT_LOGGER.setLevel(logging.INFO)

####################################################################################################
#  SETUP MATPLOTLIB

import matplotlib.pyplot as plt

####################################################################################################
#  EXCEPTION CLASSES

class CommandHardTimeout(Exception):
    '''Exception thrown when command spends too long running and has to be killed by the script'''
    def __init__(self, cmd, timeout, output):
        self.cmd = cmd
        self.timeout = timeout
        self.output = output
    def __str__(self):
        return "Command '" + self.cmd + "', timeout was " + str(self.timeout) + " seconds, and output so far was:\n" + output

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

def run_command(cmd, ssh_client=None, parser=None):
  """
  Run command, returning tuple of exit code and stdout/err.
  The command will be killed if it takes longer than TEST_CMD_TIMEOUT_S
  """
  if (parser is None) and (cmd.startswith("PerfTester.exe")):
      parser = parse_perftester
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
                  raise CommandHardTimeout(cmd, TEST_CMD_TIMEOUT_S, stdout)

    except KeyboardInterrupt:
        print "+ Ctrl-C detected."
        os.killpg(p.pid, signal.SIGTERM)
        raise KeyboardInterrupt

    exit_code = p.poll()
    if exit_code:
        raise CommandBadExitCode(cmd, exit_code, stdout)

    if parser in [None,False]:
        return exit_code, stdout
    else:
        return parser(stdout)

  else:
    for env_var, value in CH_PC_ENV.iteritems():
        cmd = "export " + env_var + "=" + value + " ; " + cmd
    SCRIPT_LOGGER.info("Running (remotely): "+cmd)
    stdin, stdout, stderr = ssh_client.exec_command(cmd)
    exit_code = stdout.channel.recv_exit_status()
    output = "".join( stdout.readlines() ) + "".join( stderr.readlines() )
   
    SCRIPT_LOGGER.debug("Output is ...\n"+output)
 
    if exit_code: 
        raise CommandBadExitCode(cmd, exit_code, output)

    if parser in [None, False]:
        return exit_code, output
    else:
        return parser(output)


def parse_perftester(cmd_output):
    """Parses output of PerfTester.exe, and return tuple of latency per iteration (us), and bandwidth (Mb/s)"""
    
    m1 = re.search(r"^Test iteration frequency\s+=\s*([\d\.]+)\s*Hz", cmd_output, flags=re.MULTILINE)
    freq = float(m1.group(1))
    m2 = re.search(r"^Average \S+ bandwidth\s+=\s*([\d\.]+)\s*KB/s", cmd_output, flags=re.MULTILINE)
    bandwidth = float(m2.group(1)) / 125.0

    SCRIPT_LOGGER.info("Parsed: Latency = " + str(1000000.0/freq) + "us/iteration , bandwidth = " + str(bandwidth) + "Mb/s")
    return (1000000.0/freq, bandwidth)


def run_ping(target, ssh_client=None):
    '''Runs unix ping command, parses output and returns average latency'''
    
    target_dns = target.split(":")[0]
    run_command("ping -c 2 " + target_dns, ssh_client)
    exit_code, output = run_command("ping -c 10 " + target_dns, ssh_client)

    m = re.search(r"^.+\s=\s[\d\.]+/([\d\.]+)/[\d\.]+/[\d\.]+\sms", output, flags=re.MULTILINE)
    avg_latency_us = 1000 * float(m.group(1))

    return avg_latency_us


def start_controlhub(ssh_client=None):
    run_command("sudo controlhub_start", ssh_client=ssh_client)


def stop_controlhub(ssh_client=None):
    run_command("sudo controlhub_stop", ssh_client=ssh_client)


####################################################################################################
# SSH / SFTP FUNCTIONS

def ssh_into(hostname, username):
    try:
        passwd = getpass.getpass('Password for ' + username + '@' + hostname + ': ')
        client = paramiko.SSHClient()
        client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        client.connect(hostname, username=username, password=passwd)
        print " SSH client now connected!"
        return client
    except paramiko.AuthenticationException, e:
        print "Authenication exception caught. Details:", str(e)
        print "Let's try again ...\n"
        return ssh_into(hostname, username)


def update_controlhub_sys_config(max_in_flight, ssh_client, sys_config_location):
    """Writes new ControlHub sys.config file in /tmp, and copies to remote PC via SFTP."""
    
    content = ('[\n'
               '%% write log files to a particular location\n'
               '  {sasl,\n'
               '    [\n'
               '      {sasl_error_logger, {file, "/var/log/controlhub.log"}},\n'
               '      {error_logger_mf_dir, "/var/log/controlhub"},\n'
               '      {error_logger_mf_maxbytes, 10485760},\n'
               '      {error_logger_mf_maxfiles, 4}\n'
               '    ]\n'
               '  },\n'
               '  {controlhub,\n'
               '    [{max_in_flight, ' + str(max_in_flight) + '}]\n'
               '  }\n'
               '].\n')
    
    SCRIPT_LOGGER.info('ControlHub (remote) sys.config file at "' + sys_config_location + '" is being updated to have max_in_flight=' + str(max_in_flight) + '. New contents is ...\n' + content)
 
    with tempfile.NamedTemporaryFile('w+b', suffix="__ch_sys.config") as tmp_file:
        tmp_file.write(content)
        tmp_file.flush()

        sftp_client = ssh_client.open_sftp()
        sftp_client.put(tmp_file.name, sys_config_location)
        sftp_client.close()     


####################################################################################################
#  FUNCTIONS RUNNING SEQUENCES OF TESTS

def calc_y_with_errors(data_dict):
    means = []
    low_error  = []
    high_error = []
    for x in sorted(data_dict.keys()):
        y_values = data_dict[x]
        mean = numpy.mean(y_values)
        means.append(mean)
        low_error.append(mean - numpy.min(y_values))
        high_error.append(numpy.max(y_values) - mean)

    return means, [low_error, high_error]
        

def measure_latency(target, controhub_ssh_client=None):
    '''Measures latency for single word write to given endpoint'''
    print "\n ---> MEASURING LATENCY TO '" + target + "' <---"
    
    ping_localhost_ch     = run_ping(CH_PC_NAME)
    ping_ch_target        = run_ping(target, ssh_client=controlhub_ssh_client)
    ping_localhost_target = run_ping(target) 
    udp_latency_localhost = run_command("PerfTester.exe -t BandwidthTx -b 0x1000 -w 1 -i 10000 -p -d ipbusudp-2.0://"+target)[0]
    udp_latency_chpc      = run_command("PerfTester.exe -t BandwidthTx -b 0x1000 -w 1 -i 10000 -p -d ipbusudp-2.0://"+target, ssh_client=controlhub_ssh_client)[0]
    
    start_controlhub(controlhub_ssh_client)
    chtcp_latency = run_command("PerfTester.exe -t BandwidthTx -b 0x1000 -w 1 -p -d chtcp-2.0://"+CH_PC_NAME+":10203?target="+target)[0]
    stop_controlhub(controlhub_ssh_client)    

    print "Latencies for "+target
    print "    + Ping, here -> CH    : " + ("%5.1f" % ping_localhost_ch) + "us"
    print "    + Ping, CH -> board   : " + ("%5.1f" % ping_ch_target) + "us"
    print "    + Ping, here -> board : " + ("%5.1f" % ping_localhost_target) + "us"
    print "    + Direct UDP (this PC): " + ("%5.1f" % udp_latency_localhost) + "us"
    print "    + Direct UDP (CH PC)  : " + ("%5.1f" % udp_latency_chpc) + "us"
    print "    + Via ControlHub      : " + ("%5.1f" % chtcp_latency) + "us"


def measure_bandwidth_vs_depth(target, controlhub_ssh_client):
    '''Measures bandwidth for block writes to given endpoint'''
    print "\n ---> MEASURING BANDWIDTH TO '" + target + "' <---"
    
    depths = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25,
              50, 75, 100, 150, 200, 250, 300, 310, 320, 330, 340,
              341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 500,
              1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 10000, 20000]
    udp_bandwidths = dict((x, []) for x in depths)
    ch_bandwidths = dict((x, []) for x in depths)

    udp_uri = "ipbusudp-2.0://" + target
    for i in range(2):
        for d in depths:
            udp_bandwidths[d].append( run_command("PerfTester.exe -t BandwidthTx -b 0x1000 -w "+str(d)+" -p -i 2000 -d "+udp_uri, 
                                                  ssh_client=controlhub_ssh_client)[1] )
    udp_bws_mean, udp_bws_yerrors = calc_y_with_errors(udp_bandwidths)
    plt.errorbar(depths, udp_bws_mean, yerr=udp_bws_yerrors, label="Direct UDP (CH PC)")

    udp_tx_bw_inf = run_command("PerfTester.exe -t BandwidthTx -b 0x1000 -w 1000 -i 50000 -d "+udp_uri, ssh_client=controlhub_ssh_client)[1]
    udp_rx_bw_inf = run_command("PerfTester.exe -t BandwidthRx -b 0x1000 -w 1000 -i 50000 -d "+udp_uri, ssh_client=controlhub_ssh_client)[1]

    ch_uri = "chtcp-2.0://" + CH_PC_NAME + ":10203?target=" + target
    for nr_in_flight in [1,4,16]:
        ch_bandwidths = dict((x, []) for x in depths)

        update_controlhub_sys_config(nr_in_flight, controlhub_ssh_client, CH_SYS_CONFIG_LOCATION)
        start_controlhub(controlhub_ssh_client)
        for i in range(2):
            for d in depths:
                ch_bandwidths[d].append( run_command("PerfTester.exe -t BandwidthTx -b 0x1000 -w "+str(d)+" -p -i 2000 -d "+ch_uri)[1] )
        ch_bws_mean, ch_bws_yerrors = calc_y_with_errors(ch_bandwidths)
        plt.errorbar(depths, ch_bws_mean, yerr=ch_bws_yerrors, label="Via CH, "+str(nr_in_flight)+" in-flight")

        ch_tx_bw_inf  = run_command("PerfTester.exe -t BandwidthTx -b 0x1000 -w 1000 -i 50000 -d "+ch_uri)[1]
        ch_rx_bw_inf  = run_command("PerfTester.exe -t BandwidthRx -b 0x1000 -w 1000 -i 50000 -d "+ch_uri)[1]
        stop_controlhub(controlhub_ssh_client)

        print "inf, Tx  ==> ", "%.1f" % udp_tx_bw_inf, " / ", "%.1f" % ch_tx_bw_inf, " -- ratio CH/udp", "%.2f" % (ch_tx_bw_inf/udp_tx_bw_inf)
        print "inf, Rx  ==> ", "%.1f" % udp_rx_bw_inf, " / ", "%.1f" % ch_rx_bw_inf, " -- ratio CH/udp", "%.2f" % (ch_rx_bw_inf/udp_rx_bw_inf)

    plt.xlabel("Number of words")
    plt.ylabel("Write bandwidth [Mb/s]")
#    plt.xscale("log")
    plt.legend(loc='lower right')
    plt.title(gethostname() + "  ->  " + CH_PC_NAME + "  ->  " + target)

####################################################################################################
#  MAIN

if __name__ == "__main__":
    controlhub_ssh_client = ssh_into( CH_PC_NAME, CH_PC_USER )

    #measure_latency(TARGETS[0], controlhub_ssh_client)

    measure_bandwidth_vs_depth(TARGETS[0], controlhub_ssh_client)

    plt.show()

