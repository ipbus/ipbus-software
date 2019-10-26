#!/usr/bin/env python
"""
Usage: ipbus_perf_suite.py <config file>

This script runs the standard set of IPbus performance & robostness measurements.

N.B: You must set LD_LIBRARY_PATH and PATH appropriately before this 
     [So that this script can be run for checked-out sources (during dev) or installed RPMs.]
"""

from datetime import datetime
import fcntl
import getopt
import getpass
from itertools import izip
from math import sqrt
from math import ceil
import numpy
import os
import paramiko
import pickle
from random import randint
import re
import signal
from socket import gethostname
import subprocess
import sys
import tempfile
import threading
import time

####################################################################################################
#  GLOBAL OPTIONS

TEST_CMD_TIMEOUT_S = 60

UHAL_PC_NAME = gethostname()

CH_PC_NAME = 'pc-e1x06-36-01'
CH_PC_USER = 'tsw'
CH_PC_ENV  = {'PATH':os.environ['PATH'],
              'LD_LIBRARY_PATH':os.environ['LD_LIBRARY_PATH'] 
              }

CH_MAX_IN_FLIGHT = 16
CH_SYS_CONFIG_LOCATION = "/cactusbuild/trunk/cactuscore/controlhub/RPMBUILD/SOURCES/lib/controlhub/releases/2.3.0/sys.config"

TARGETS = [# GLIBs
           'amc-e1a12-19-04:50001',
#           'amc-e1a12-19-09:50001',
           'amc-e1a12-19-10:50001',
           # Mini-T5s
           'amc-e1a12-19-01:50001',
           'amc-e1a12-19-02:50001',
           'amc-e1a12-19-03:50001',
           'amc-e1a12-19-05:50001',
           'amc-e1a12-19-06:50001',
           'amc-e1a12-19-07:50001',
           'amc-e1a12-19-08:50001',
           'amc-e1a12-19-11:50001',
           'amc-e1a12-19-12:50001'
          ]

DIRECT_UDP_NR_IN_FLIGHT = 16

FW_VERSION = "ipbus_2_0_v1"
SW_VERSION = "v2.2.0"

CHANGES_TAG = "No changes"


#####################################################################################################
# CONSTANTS derived from global options



###################################################################################################
#  SETUP LOGGING

import logging

SCRIPT_LOGGER = logging.getLogger("ipbus_perf_suite_log")

SCRIPT_LOG_HANDLER = logging.StreamHandler(sys.stdout)
SCRIPT_LOG_FORMATTER = logging.Formatter("%(asctime)-15s [0x%(thread)x] %(levelname)-7s > %(message)s")
SCRIPT_LOG_HANDLER.setFormatter(SCRIPT_LOG_FORMATTER)

SCRIPT_LOGGER.addHandler(SCRIPT_LOG_HANDLER)
SCRIPT_LOGGER.setLevel(logging.WARNING)

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
        return "Command '" + self.cmd + "', timeout was " + str(self.timeout) + " seconds, and output so far was:\n" + self.output

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

def run_command(cmd, ssh_client=None, parser=None, throw_on_bad_exit_code=True):
  """
  Run command, returning tuple of exit code and stdout/err.
  The command will be killed if it takes longer than TEST_CMD_TIMEOUT_S
  """
  if (parser is None) and (cmd.startswith("PerfTester.exe") or cmd.startswith("perf_tester.escript")):
      parser = parse_perftester
  if cmd.startswith("sudo"):
      cmd = "sudo PATH=$PATH " + cmd[4:]

  if ssh_client is None:
    SCRIPT_LOGGER.debug("Running (locally): "+cmd)
    t0 = time.time()

    if cmd.startswith('PerfTester.exe'):
        p  = subprocess.Popen(cmd.split(),stdout=subprocess.PIPE, stderr=subprocess.STDOUT, stdin=None, shell=False, preexec_fn=os.setsid)
    else:
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
    if exit_code and throw_on_bad_exit_code:
        raise CommandBadExitCode(cmd, exit_code, stdout)

    if parser in [None,False]:
        return exit_code, stdout
    else:
        return parser(stdout)

  else:
    for env_var, value in CH_PC_ENV.iteritems():
        cmd = "export " + env_var + "=" + value + " ; " + cmd
    SCRIPT_LOGGER.debug("Running (remotely): "+cmd)
    stdin, stdout, stderr = ssh_client.exec_command(cmd)
    exit_code = stdout.channel.recv_exit_status()
    output = "".join( stdout.readlines() ) + "".join( stderr.readlines() )
   
    SCRIPT_LOGGER.debug("Output is ...\n"+output)
 
    if exit_code and throw_on_bad_exit_code: 
        raise CommandBadExitCode(cmd, exit_code, output)

    if parser in [None, False]:
        return exit_code, output
    else:
        return parser(output)


def parse_perftester(cmd_output):
    """Parses output of PerfTester.exe, and return tuple of latency per iteration (us), and bandwidth (Gbit/s)"""
    
    m1 = re.search(r"^Test iteration frequency\s+=\s*([\d\.]+)\s*Hz", cmd_output, flags=re.MULTILINE)
    freq = float(m1.group(1))
    m2 = re.search(r"^Average \S+ bandwidth\s+=\s*([\d\.]+)\s*KB/s", cmd_output, flags=re.MULTILINE)
    bw = float(m2.group(1)) / 125e3

    SCRIPT_LOGGER.info("Parsed: Latency = " + str(1000000.0/freq) + "us/iteration , bandwidth = " + str(bw) + "Gb/s")
    return (1000000.0/freq, bw)


def parse_fixed_packet_client(cmd_output):
    """
    Parses output of fixed-packet Erlang/boost clients.
    Returns tuple of latency per iteration (us) , and max of tx/rx bandwidth (Gbit/s)
    """

    m1 = re.search(r'^\s*Frequency\s+=\s+([\d\.]+)\s*Hz', cmd_output, flags=re.MULTILINE)
    freq = float(m1.group(1))
    m2 = re.search(r'^\s*Send\s*\(recv\) throughput\s*=\s*([\d\.]+)\s*\(([\d\.]+)\)\s*Mbit/s', cmd_output, flags=re.MULTILINE)
    bw = max( float(m2.group(1)), float(m2.group(2)) ) / 1e3

    SCRIPT_LOGGER.info('Parsed (fixed-packet client): Latency = ' + str(1e6/freq) + ' us/iteration , bandwidth = ' + str(bw) + ' Gbit/s')
    return (1e6/freq, bw)


def run_ping(target, ssh_client=None):
    '''Runs unix ping command, parses output and returns average latency'''
    
    target_dns = target.split(":")[0]
    run_command("ping -c 2 " + target_dns, ssh_client)
    exit_code, output = run_command("ping -c 10 " + target_dns, ssh_client)

    m = re.search(r"^.+\s=\s[\d\.]+/([\d\.]+)/[\d\.]+/[\d\.]+\sms", output, flags=re.MULTILINE)
    evg_latency_us = 1000 * float(m.group(1))

    return avg_latency_us


def cpu_mem_usage(cmd_to_check, ssh_client=None):
    assert " " not in cmd_to_check

    output = run_command("top -b -n 1 | grep "+cmd_to_check, ssh_client=ssh_client)[1]
#    SCRIPT_LOGGER.warning("Parsing ...")
    cpu, mem = 0.0, 0.0    

    regex = re.compile("^\\s*\\d+\\s+\\w+\\s+\\S+\\s+\\S+\\s+"        # PID USER      PR  NI  
                       "\\w+\\s+\\w+\\s+\\w+\\s+\\S+\\s+"         # VIRT  RES  SHR S
                       "([\\d+\\.]+)\\s+([\\d+\\.]+)\\s+"         # %CPU %MEM
                       "[\\d:\\.]+\\s+" + re.escape(cmd_to_check) # TIME+  COMMAND
                       )

    for line in output.splitlines():
        m = regex.search(line)
        if m:
            cpu += float(m.group(1))
            mem += float(m.group(2))

    return cpu, mem


def start_controlhub(ssh_client=None):
    run_command("sudo controlhub_start", ssh_client=ssh_client)


def stop_controlhub(ssh_client=None):
    run_command("sudo controlhub_stop", ssh_client=ssh_client)
    

class CommandRunner:
    """
    Class for running a set of commands in parallel - each in their own thread - whilst 
    monitoring CPU & mem usage of some other commands in the main thread.
    """
    def __init__(self, monitoring_options):
        """
        Contructor. 
        The monitoring_options argument must be a list of (command_to_montor, ssh_client) tuples
        """
        self.monitor_opts = monitoring_options

    def _run_in_thread(self, cmd, ssh_client, index):
        SCRIPT_LOGGER.debug('CommandRunner thread starting for command "' + cmd + '"')
        try:
            retval = run_command(cmd, ssh_client)
            self._cmd_completed = True
            self.cmd_results[index] = retval
        except Exception as e:
            SCRIPT_LOGGER.exception('Exception of type "' + str(type(e)) + '" thrown when executing the command "' + cmd + '" in this thread.')
            self._cmd_completed = True
            self.cmd_results[index] = e 

    def run(self, cmds):
        """
        Runs the commands via ssh_client simultaneously in different threads, blocking until they are all finished.
        The argument cmds is a list of commands, or (cmd, ssh_client) tuples. If ssh_client is not specified, then command is run locally.
        Returns a 2-tuple - element 1 is list of run_command(cmd) return values; element 2 is a list of (cmd, av_cpu, av_mem) tuples.
        """
        assert len(cmds)>0        
        SCRIPT_LOGGER.info( "CommandRunner will now run the following commands simultaneously:\n     " + "\n     ".join(cmds) )

        self.cmd_results = [None for x in cmds]
        self.threads = []
        self._cmd_completed = False

        monitor_results = []
        for cmd, ssh_client in self.monitor_opts:
            monitor_results.append( (cmd, ssh_client, [], []) )

        # Set each command running
        for i in range(len(cmds)):
            if isinstance(cmds[i], basestring):
                cmd = cmds[i]
                ssh_client = None
            else:
                cmd, ssh_client = cmds[i]
            t = threading.Thread(target=self._run_in_thread, args=(cmd, ssh_client, i) )
            self.threads.append(t)

        for t in self.threads:
            t.start()

        # Monitor CPU/mem usage whilst *all* commands running (i.e. until any one of the commands exits)
        time.sleep(0.4)
        SCRIPT_LOGGER.debug('CommandRunner is now starting monitoring.')
        while not self._cmd_completed:
            try:
                for cmd, ssh_client, cpu_vals, mem_vals in monitor_results:
#                    SCRIPT_LOGGER.warning('Monitoring command: %s' % cmd)
                    meas_cpu, meas_mem = cpu_mem_usage(cmd, ssh_client)
#                    print meas_cpu, meas_mem
                    cpu_vals.append(meas_cpu)
                    mem_vals.append(meas_mem)
            except CommandBadExitCode as e:
                if not self._cmd_completed:
                    raise
            #time.sleep(0.02) 
        for cmd, ssh_client, cpu_vals, mem_vals in monitor_results:
            cpu_vals.pop()
#            assert(len(cpu_vals) is not 0)
            mem_vals.pop()
#            assert(len(mem_vals) is not 0)


        # Wait (without monitoring)
        SCRIPT_LOGGER.debug('One of the commands has now finished. No more monitoring - just wait for rest to finish.')
        for t in self.threads:
            t.join()
            SCRIPT_LOGGER.debug("Thread with ID 0x%s has now finished. It's being removed from the list of running threads." % hex(t.ident))
            del t

        # Check for async exceptions
        for result in self.cmd_results:
            if issubclass(type(result), Exception):
                SCRIPT_LOGGER.error("An exception was raised in one of CommandRunner's command-running threads. Re-raising now ...")
                raise result

        return [(cmd, numpy.mean(cpu_vals), numpy.mean(mem_vals)) for cmd, ssh_client, cpu_vals, mem_vals in monitor_results], self.cmd_results


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
#  FUNCTIONS: SETUP

def setup_for_measurements():
    pass


def setup_for_plotting():
    pass



####################################################################################################
#  FUNCTIONS RUNNING SEQUENCES OF MEASUREMENTS / MAKING SEQUENCES OF PLOTS

class DataseriesStats(object):
    def __init__(self):
        self.mean = []
        self.mean_err_lo = []
        self.mean_err_hi = []
        self.rms = []
        self.rms_err_lo = []
        self.rms_err_hi = []

    def mean_errors(self):
        return [self.mean_err_lo, self.mean_err_hi]

    def rms_errors(self):
        return [self.rms_err_lo, self.rms_err_hi]


def calc_y_stats(data_dict):
    stats = DataseriesStats()
    for x in sorted(data_dict.keys()):
        y_values = data_dict[x]
        mean, mean_err = calc_mean_with_error(y_values)
        rms, rms_err = calc_rms_with_error(y_values)
        stats.mean.append(mean)
        stats.mean_err_lo.append(mean_err)
        stats.mean_err_hi.append(mean_err)
        stats.rms.append(rms/mean)
        stats.rms_err_lo.append(rms_err/mean)
        stats.rms_err_hi.append(rms_err/mean)

    return stats



def calc_mean_with_error(data_list):
    mean = numpy.mean(data_list)
    n = len(data_list)
    bootstrap_sample_means = []
    for i in range(100):
        sum = 0.0
        for i in range(n):
            sum += data_list[randint(0,n-1)]
        bootstrap_sample_means.append( sum/n )
    err = calc_rms( bootstrap_sample_means )
    # rms = sqrt( sum([(y-mean)**2 for y in data_list]) / len(data_list) )
    return mean, err



def calc_rms(data_list):
    mean = numpy.mean(data_list)
    mean_sq_diff = sum([ (y-mean)**2 for y in data_list ]) / len(data_list)
    return sqrt( mean_sq_diff )



def calc_rms_with_error(data_list):
    rms = calc_rms(data_list)
    # Bootstrap for error
    n = len(data_list)
    bootstrap_sample_rms = []
    for i in range(100):
        min, max = 0.0, 0.0
        sum, sum2 = 0.0, 0.0
        for j in range(n):
            val = data_list[randint(0,n-1)]
            sum += val
            sum2 += val ** 2
            if j == 0:
                min, max = val, val
            elif val < min:
                min = val
            elif val > max:
                max = val
        mean = sum/n
        if min == max or (max-min) < (1e-8 * mean):
           bootstrap_sample_rms.append ( 0.0 )
        else:
           bootstrap_sample_rms.append ( sqrt(sum2/n - mean*mean) )
    err = calc_rms( bootstrap_sample_rms )
    return rms, err



def measure_latency(target, controhub_ssh_client, ax):
    '''Measures latency for single word write to given endpoint'''
    print "\n ---> MEASURING LATENCY TO '" + target + "' <---"
    print time.strftime('%l:%M%p %Z on %b %d, %Y')

#    # Initialise vars to store latency measurements:
#    lats_ping_uhal_ch, lats_ping_ch_target, lats_ping_uhal_target = [], [], []
#    lats_ipbusudp_uhalpc, lats_ipbusudp_chpc = [], []
#    lats_chtcp = []
#
#    for i in range(5):
#       lats_ping_uhal_ch.append( run_ping(CH_PC_NAME) )
#       lats_ping_ch_target.append( run_ping(target, ssh_client=controlhub_ssh_client) )
#       lats_ping_uhal_target.append( run_ping(target) )
#       lats_ipbusudp_uhalpc.append( run_command("PerfTester.exe -t BandwidthTx -b 0x1000 -w 1 -i 1000 -p -d ipbusudp-2.0://"+target)[0] ) 
#       lats_ipbusudp_chpc.append( run_command("PerfTester.exe -t BandwidthTx -b 0x1000 -w 1 -i 1000 -p -d ipbusudp-2.0://"+target, ssh_client=controlhub_ssh_client)[0] )
#       start_controlhub(controlhub_ssh_client)
#       lats_chtcp.append( run_command("PerfTester.exe -t BandwidthTx -b 0x1000 -w 1 -i 1000 -p -d chtcp-2.0://"+CH_PC_NAME+":10203?target="+target)[0] )
#       stop_controlhub(controlhub_ssh_client)    

#    positions, vals, errs, labels = [], [], [], []
    
#    def analyse( latencies_list, label):
#        if len(positions) == 0:
#            positions.append( 0.5 )
#        else:
#            positions.append( positions[-1] - 1.0 )
#        mean, rms = calc_mean_with_error(latencies_list)
#        vals.append( mean )
#        errs.append( rms )
#        labels.append( label )

#    analyse( lats_ping_uhal_ch , 'Ping\nuHAL to CH PC')
#    analyse( lats_ping_ch_target, 'Ping\nCH PC to board')
#    analyse( lats_ping_uhal_target, 'Ping\nuHAL PC to board')
#    analyse( lats_ipbusudp_uhalpc, 'Direct UDP\nFrom uHAL PC')
#    analyse( lats_ipbusudp_chpc, 'Direct UDP\nFrom CH PC')
#    analyse( lats_chtcp, 'Via ControlHub')

#    axis.barh(positions, vals, xerr=errs, ecolor='black', align='center')
#    axis.set_xlabel('Latency [us]')
#    axis.set_yticks(positions)
#    axis.set_yticklabels(labels)
#    axis.grid(True) 

    depths = [1, #2, 3, 4, 5, 6, 7, 8, 9, 10,
              50, 100, 150, 200, 250, 300, 342, 345, 400, 450, 500, 550, 600, 680, 690, 800, 900, 1000
#              75, 100, 150, 175, 200, 250, 300, 350, 400, 
#              500, 600, 700, 800, 900, 1000
             ]

    ch_uri = "chtcp-2.0://" + CH_PC_NAME + ":10203?target=" + target
    ch_tx_lats = dict((x, []) for x in depths)
    ch_rx_lats = dict((x, []) for x in depths)

    start_controlhub(controlhub_ssh_client)
    for i in range(10):
        for d in depths:
            itns = 1000
            ch_tx_lats[d].append( run_command("PerfTester.exe -t BandwidthTx -b 0x2001 -w "+str(d)+" -p -i "+str(itns)+" -d "+ch_uri)[0] )
            ch_rx_lats[d].append( run_command("PerfTester.exe -t BandwidthRx -b 0x2001 -w "+str(d)+" -p -i "+str(itns)+" -d "+ch_uri)[0] )
    stop_controlhub(controlhub_ssh_client)

    ch_tx_lats_mean, ch_tx_lats_yerrors = calc_y_with_errors(ch_tx_lats)
    ch_rx_lats_mean, ch_rx_lats_yerrors = calc_y_with_errors(ch_rx_lats)

    ax.errorbar(depths, ch_tx_lats_mean, yerr=ch_tx_lats_yerrors, label="Block write")
    ax.errorbar(depths, ch_rx_lats_mean, yerr=ch_rx_lats_yerrors, label="Block read")

    ax.set_xlabel("Number of words")
    ax.set_ylabel("Mean latency [us]")
#    plt.xscale("log")
    ax.legend(loc='lower right')



def measure_1_to_1_latency(target, controlhub_ssh_client, n_meas, max_depth, pkt_depths):
    '''
    Measures latencies for block reads & writes to given endpoint, and 
    returns structured array containing latencies in micro-sec

    Arguments:
      target                 --  target hostname (or IP) & port in format "hostname:port"
      controlhub_ssh_client  --  ssh client instance for connecting to controlhub host
      n_meas                 --  number of measurements to take at each depth
    '''

    print "\n ---> MEASURING 1 to 1 performance vs DEPTH to '" + target + "' <---"
    print time.strftime('%l:%M%p %Z on %b %d, %Y')

    depths = [1]
    depths += [50, 100, 150, 200, 300, 500, 600, 800, 1000]
    for n in range(1, 18):
        depths += [n*min(pkt_depths), n*max(pkt_depths)+1]
    for n in range(6, 31, 2) + range(40, 80, 20) + range(80, 200, 40):
        depths += [3*n*min(pkt_depths)]
    for n in [2e2, 3e2, 4e2, 7e2, 10e2]:
        depths += [3*n*min(pkt_depths)]
    for n in [2e3, 3e3, 4e3, 7e3, 10e3]:
        depths += [3*n*min(pkt_depths)]

    depths.sort()
    depths = filter(lambda x: x<=max_depth, depths)

    data = numpy.zeros(len(depths), 
                    dtype=[('w','uint32'),
#                           ('udp_tx', 'float32', (n_meas,)),
#                           ('udp_tx_1itn', 'float32', (n_meas,)),
#                           ('udp_rx', 'float32', (n_meas,)),
                           ('ch_tx',  'float32', (n_meas,)),
#                           ('ch_tx_1itn', 'float32', (n_meas,)),
                           ('ch_rx',  'float32', (n_meas,))
                          ]
                   )

    for i in range(len(depths)):
        data['w'][i] = int(depths[i])

    update_controlhub_sys_config(CH_MAX_IN_FLIGHT, controlhub_ssh_client, CH_SYS_CONFIG_LOCATION)
    start_controlhub(controlhub_ssh_client)

    udp_uri = "ipbusudp-2.0://" + target
    ch_uri  = "chtcp-2.0://" + CH_PC_NAME + ":10203?target=" + target

    for i in range(n_meas):
        SCRIPT_LOGGER.warning('1-to-1 measurements: iteration %d' % i)

        for entry in data:
            cmd_tx = "PerfTester.exe -t BandwidthTx -b 0x2001 -w " + str(entry['w']) + " -p -i 1 -d "
            cmd_rx = cmd_tx.replace("BandwitdthTx", "BandwidthRx")

#            entry['udp_tx'][i] = run_command( cmd_tx + udp_uri )[0]
#            entry['udp_tx_1itn'][i] = run_command( cmd_tx.replace('-i 10', '-i 1') + udp_uri )[0]
#            entry['udp_rx'][i] = run_command( cmd_rx + udp_uri )[0]
            entry['ch_tx'][i]  = run_command( cmd_tx + ch_uri  )[0]
#            entry['ch_tx_10itn'][i] = run_command( cmd_tx.replace('-i 1', '-i 10') + ch_uri )[0]
            entry['ch_rx'][i]  = run_command( cmd_rx + ch_uri  )[0]


    # Final cleanup
    stop_controlhub(controlhub_ssh_client)

    return data



def random_sample(src, N=None, transform=lambda x: x):
    '''
    Returns a random sample of length N, generated by randomly picking elements of src
    '''
    assert isinstance(src, numpy.ndarray)
    assert hasattr(transform, '__call__')

    if N is None:
        N = len(src)

    result = numpy.zeros(N, src.dtype)
    for i in range(len(result)):
        result[i] = transform( src[ numpy.random.randint(len(src)) ] )
    return result



def calc_percentiles(sample, fractions):
    '''
    Returns the positions of the percentile 'fraction' within this sample
    NOTA BENE ... CURRENT IMPLEMENTATION SORTS sample IN PLACE.
    '''
    assert isinstance(sample, numpy.ndarray)
    assert isinstance(fractions, list)
    for f in fractions:
        assert (f > 0.0) and (f < 1.0)
    sample.sort() # In place sort => changes the argument
    result = numpy.zeros(len(fractions), 'float32')
    for i, f in enumerate(fractions):
        frac_as_index = f * ( len(sample) - 1 )
        low_index  = int(frac_as_index)
        high_index = low_index + 1
        result[i] = sample[low_index] + (frac_as_index - low_index) * (sample[high_index] - sample[low_index])
    return result



def bootstrap_percentile(measurements, fractions, N=100, transform=lambda x: x):
    '''
    Returns the estimate of the percentile corresponding to fraction, and it's error, as a tuple (estimate, error)

    Arguments:
      measurements  --  A 1D numpy.ndarray containing the 
      f  --  percentile value, expressed as a fraction - i.e. 0 < f < 1
      N  --  number of bootstrap
    '''
    assert isinstance(measurements, numpy.ndarray)
    assert isinstance(fractions, list)
#    assert ( f > 0.0 ) and ( f < 1.0 )
    assert isinstance(N, int)

    bootstrap_pc_struct = numpy.zeros(N, str(len(fractions))+'float32')
    for i in range(len(bootstrap_pc_struct)):
        sample = random_sample( measurements, transform=transform )
        bootstrap_pc_struct[i] = calc_percentiles(sample, fractions)

    return [(numpy.mean(bootstrap_pc_values), calc_rms( bootstrap_pc_values))
             for bootstrap_pc_values 
             in bootstrap_pc_struct.swapaxes(0,1)
           ]



def bootstrap_stats_array( raw_data, transforms=None ):
    '''
    Returns stats values as numpy structured array

    Arguments:
      raw_data  --  A numpy array of numpy arrays.
    '''

    result = numpy.zeros(
                         (len(raw_data),),
                         dtype=[('16_est', 'float32'), ('16_est_rel2median', 'float32'),
                                ('16_err', 'float32'), ('16_err_rel2median', 'float32'),
                                ('50_est', 'float32'),
                                ('50_err', 'float32'),
                                ('84_est', 'float32'), ('84_est_rel2median', 'float32'),
                                ('84_err', 'float32'), ('84_err_rel2median', 'float32'),
                               ]
                        )

    if transforms is None:
        transforms = [(lambda x: x) for i in result]

    for values, stats_struct, transform in izip(raw_data, result, transforms):
        [(stats_struct['16_est'], stats_struct['16_err']),
         (stats_struct['50_est'], stats_struct['50_err']),
         (stats_struct['84_est'], stats_struct['84_err']),
        ] = bootstrap_percentile(values, [0.16, 0.50, 0.84], transform=transform)

    for i in range(len(result)):
        for pc in ['16', '84']:
            result[pc+'_est_rel2median'][i] = ( result[pc+'_est'][i] / result['50_est'][i] )
            result[pc+'_err_rel2median'][i] = ( result[pc+'_err'][i] / result['50_est'][i] )

    return result



def plot_1_to_1_performance( all_data , key_label_pairs , words_per_pkt ):
    '''
    Plots latency and bandwidth for block reads and writes from single client to single target.
 
    Arguments:
      data  --  
    '''
    print "\n ---> PLOTTING LATENCY/BANDWIDTH vs DEPTH <---"
    print time.strftime('%l:%M%p %Z on %b %d, %Y')

    ## Set up graphs ...
    ## sharex: Makes x axes of plots have same limits

    med_ax_loc = (.15,.4,.8,.55)
    err_ax_loc = (.15,.1,.8,.25)

    fig_lat = plt.figure(figsize=(6,6.5))
    ax_lat1 = fig_lat.add_axes(med_ax_loc, autoscalex_on=False, autoscaley_on=True, xticklabels=[])
    ax_lat2 = fig_lat.add_axes(err_ax_loc, autoscalex_on=False, autoscaley_on=True, sharex=ax_lat1)

    ax_lat1.set_xlim(0,1001)
    ax_lat1.set_ylabel('Median latency [us]') 


    fig_linbw = plt.figure(figsize=(6,6.5))
    ax_linbw1 = fig_linbw.add_axes(med_ax_loc, autoscalex_on=False, autoscaley_on=True)
    ax_linbw2 = fig_linbw.add_axes(err_ax_loc, autoscalex_on=False, autoscaley_on=True, sharex=ax_linbw1)

    ax_linbw1.set_xlim(0, 4.5e3)
    ax_linbw1.set_ylabel('Median throughput [Gbit/s]')

    fig_logbw = plt.figure(figsize=(6,6.5))
    ax_logbw1 = fig_logbw.add_axes(med_ax_loc, autoscalex_on=False, autoscaley_on=True, xscale='log')
    ax_logbw2 = fig_logbw.add_axes(err_ax_loc, autoscalex_on=False, autoscaley_on=True, sharex=ax_logbw1)

    ax_logbw1.set_xlim(max(100,max(all_data['w'])/1e4), max(all_data['w']))
    ax_logbw1.set_ylabel('Median throughput [Gbit/s]')


    for ax in [ax_lat2, ax_linbw2, ax_logbw2]:
        ax.set_xlabel("Number of words")

    # Fractional error subplots
    for ax in [ax_lat2, ax_linbw2, ax_logbw2]:
        ax.set_ylabel('Fractional variation')

    # Calc stats for each line, and then plot

    depths = all_data['w']

    lat_mask = depths < 1001

    for key, label in key_label_pairs:
        print key, "--", label

        stats = bootstrap_stats_array( all_data[key] )
        bw_stats = bootstrap_stats_array( all_data[key], transforms=[(lambda x, d=d: 1e-3 * 32 * d / x) for d in depths] )

        col = ax_lat1.errorbar(depths[lat_mask], stats['50_est'][lat_mask], yerr=(stats['50_err'][lat_mask]), label=label)[0].get_color()

        ax_lat2.errorbar(depths[lat_mask], stats['16_est_rel2median'][lat_mask], yerr=(stats['16_err_rel2median'][lat_mask]), label=label, color=col)
        ax_lat2.errorbar(depths[lat_mask], stats['84_est_rel2median'][lat_mask], yerr=(stats['84_err_rel2median'][lat_mask]), label=label, color=col)

        for (ax_bw1,ax_bw2) in [(ax_linbw1,ax_linbw2), (ax_logbw1,ax_logbw2)]:
            ax_bw1.errorbar(depths, bw_stats['50_est'], yerr=(bw_stats['50_err']), label=label)
            ax_bw2.errorbar(depths, bw_stats['16_est_rel2median'], yerr=(stats['16_err_rel2median']), label=label, color=col)
            ax_bw2.errorbar(depths, bw_stats['84_est_rel2median'], yerr=(stats['84_err_rel2median']), label=label, color=col)


    # Add packet boundary lines
    for ax in [ax_lat1, ax_lat2, ax_linbw1, ax_linbw2]:
        for d in range(0, 10000, words_per_pkt):
            ax.axvline(d, color='DarkGrey', linestyle='-.')

    # Fractional variation plots:  Add y=1 line & change y range
    for ax in [ax_lat2, ax_linbw2, ax_logbw2]:
        ax.axhline(1, color='Black', linestyle=':')
        ax.set_ylim(0.77, 1.23)


    # Add legends
    for ax in [ax_lat1, ax_linbw1, ax_logbw1]:
        leg = ax.legend(loc='best', fancybox=True)
        #leg.get_frame().set_alpha(0.5)


    return [(fig_lat, '1_to_1_latency'),
            (fig_linbw, '1_to_1_bw_lin'),
            (fig_logbw, '1_to_1_bw_log')
           ]

# #        tx_rtt = 3*343*32 / ( 1000.0 * numpy.mean( ch_tx_bws[3*343] ) )
# #        tx_t_max = 2 * 3*343*32 * ( 1 / numpy.mean(ch_tx_bws[2*3*343]) - 0.5 / numpy.mean(ch_tx_bws[3*343]) ) / 1000.0
# #        print " --> PREDICTION PARAMS ..."
# #        print "       tx_rtt   =", tx_rtt, "us"
# #        print "       tx_t_max =", tx_t_max, "us"
# #        xx = numpy.arange(343.0, 200000.0, 0.1)
# #        tx_prediction = [ (x/1029.0)*32.928 / (tx_rtt + ((x/1029.0)-1)*tx_t_max) for x in xx]

        
#         print "1-word latencies ... tx w/o connect ; rx w/o connect; tx w/ connect ; tx w/o connect ..."
#         for tx_excl in sorted(ch_tx_lats_excl_connect[1]): #, rx_excl, tx_incl, rx_incl in 
#                   #zip( sorted(ch_tx_lats_excl_connect[1]),
#                   #sorted(ch_rx_lats_excl_connect[1]),
#                   #sorted(ch_tx_lats_incl_connect[1]),
#                   #sorted(ch_rx_lats_incl_connect[1]) 
#                   #):
#             #print "  {0:7.1f}  {1:7.1f}  {2:7.1f}  {2:7.1f}".format(tx_excl, rx_excl, tx_incl, rx_incl)
#             print "  {0:7.2f}".format(tx_excl)



###############################################################################################################################
###############################################################################################################################
###############################################################################################################################


def measure_n_to_m(targets, controlhub_ssh_client, n_meas, f_words, bw=True, write=True, nrs_clients=[1]):
    '''
    Measures continuous block-write bandwidth from to all endpoints, varying the number of clients.
    '''
    print "\n ---> " + ("BANDWIDTH" if bw else "LATENCY") + (" (write)" if write else " (read)") + " vs NR_CLIENTS to", targets, "<---"

    nrs_targets = range(1, len(targets)+1)

    # Preparing data structure to store measurements in

    data = numpy.zeros((len(targets), len(nrs_clients),), 
                       dtype=[('n_targets', 'uint8' ),
                              ('n_clients', 'uint8' ),
                              ('y',        'float32', (n_meas,) ),
                              ('ch_cpu',    'float32', (n_meas,) ),
                              ('ch_mem',    'float32', (n_meas,) ),
                              ('uhal_cpu',  'float32', (n_meas,) ),
                              ('uhal_mem',  'float32', (n_meas,) ),
                             ]
                      )
    for i in range(len(targets)):
        data[i]['n_targets'] = nrs_targets[i]
        for j in range(len(nrs_clients)):
            data[i]['n_clients'][j] = nrs_clients[j]

    # Run commands for measurements

    cmd_base = "PerfTester.exe"
    cmd_base += " -t BandwidthTx" if write else " -t BandwidthRx"
    cmd_base += " -i 1" if bw else " -p -w 1 "
    cmd_base += " -d chtcp-2.0://" + CH_PC_NAME + ":10203?target="

    cmd_runner = CommandRunner( [('PerfTester.exe',None), ('beam.smp',controlhub_ssh_client)] )

    update_controlhub_sys_config(CH_MAX_IN_FLIGHT, controlhub_ssh_client, CH_SYS_CONFIG_LOCATION)
    start_controlhub(controlhub_ssh_client)

    for i in range(n_meas):
        SCRIPT_LOGGER.warning( '--> Iteration %d of %d' % (i+1, n_meas) )
        for subdata in data:
            for entry in subdata:
                n_clients, n_targets = entry['n_clients'], entry['n_targets']
                n_words = int(f_words(n_clients, n_targets)) if hasattr(f_words,'__call__') else f_words
                SCRIPT_LOGGER.warning( '     %d, %d   (%d words)' % (n_clients, n_targets, n_words) )
                
                if bw: 
                    cmd_suffix = ' -w ' + str( n_words / ( n_clients * n_targets ) )
                else:
                    cmd_suffix = ' -i ' + str( n_words if ( (n_clients * n_targets) < 3) else n_words/2 )

                cmds = [cmd_base + t + cmd_suffix for t in targets[0:n_targets] for x in range(n_clients)]

                nr_attempts = 0
                while True:
                    nr_attempts += 1
                    try: 
                        monitor_results, cmd_results = cmd_runner.run(cmds)
                        break
                    except CommandHardTimeout, e:
                        if nr_attempts < 2:
                            SCRIPT_LOGGER.warning('      Command reached hard timeout. Re-running just in case that was an error ...')
                        else:
                            SCRIPT_LOGGER.error('      Command reached hard timeout on all %s attempts. Bailing out now ...' % (nr_attempts))
                            raise e

                bws = [ x[1] for x in cmd_results ]

                entry['y'][i] = sum([ x[1] for x in cmd_results]) if bw else numpy.median([x[0] for x in cmd_results])
                entry['ch_cpu'][i]   = monitor_results[1][1]
                entry['ch_mem'][i]   = monitor_results[1][2]
                entry['uhal_cpu'][i] = monitor_results[0][1]
                entry['uhal_mem'][i] = monitor_results[0][2]

    stop_controlhub(controlhub_ssh_client)

    return data



def plot_n_to_m(data_label_list, bw=True, write=True):
    if isinstance( data_label_list, numpy.ndarray ):
        data_label_list = [(data_label_list, "{0} clients")]
    assert isinstance(data_label_list, list)
    assert all(map(lambda x: (isinstance(x,tuple) and len(x) is 2), data_label_list))

    # Set up figures ...

    fig = plt.figure(figsize=(15,8))
    ax_bw_total = fig.add_subplot(231)
    ax_bw_board = fig.add_subplot(234, sharex=ax_bw_total)
    ax_ch_cpu   = fig.add_subplot(232, sharex=ax_bw_total)
    ax_ch_mem   = fig.add_subplot(235, sharex=ax_bw_total)
    ax_uhal_cpu = fig.add_subplot(233, sharex=ax_bw_total)
    ax_uhal_mem = fig.add_subplot(236, sharex=ax_bw_total, sharey=ax_ch_mem)

    fig.subplots_adjust(left=.06, right=.98, bottom=.07, top=.96)
    fig.canvas.set_window_title('600MB continuous write/read to crate' if bw else 'Polling: multiple clients and targets')

#    import matplotlib
#    matplotlib.rcParams.update({'font.size': 13})

    # Plot the datapoints ...

    for all_data, label_format in data_label_list:
      for data_subset in numpy.swapaxes(all_data, 0, 1):
        nrs_targets = data_subset['n_targets']
        n_clients = data_subset['n_clients'][0]
        assert all(map(lambda x: x == n_clients, data_subset['n_clients']))

        label = label_format.format(n_clients)

        bw_stats       = bootstrap_stats_array( data_subset['y'] )
        ch_cpu_stats   = bootstrap_stats_array( data_subset['ch_cpu'] )
        ch_mem_stats   = bootstrap_stats_array( data_subset['ch_mem'] )
        uhal_cpu_stats = bootstrap_stats_array( data_subset['uhal_cpu'] )
        uhal_mem_stats = bootstrap_stats_array( data_subset['uhal_mem'] )

#        ax_bw_board.errorbar(nrs_clients, bws_per_board_stats.mean, yerr=bws_per_board_stats.mean_errors(), label=label)
        ax_bw_total.errorbar(nrs_targets, bw_stats['50_est'], yerr=bw_stats['50_err'], label=label)
        ax_ch_cpu.errorbar(nrs_targets, ch_cpu_stats['50_est'], yerr=ch_cpu_stats['50_err'], label=label)
        ax_ch_mem.errorbar(nrs_targets, ch_mem_stats['50_est'], yerr=ch_mem_stats['50_err'], label=label)
        ax_uhal_cpu.errorbar(nrs_targets, uhal_cpu_stats['50_est'], yerr=uhal_cpu_stats['50_err'], label=label)
        ax_uhal_mem.errorbar(nrs_targets, uhal_mem_stats['50_est'], yerr=uhal_mem_stats['50_err'], label=label)

        if bw:
            bw_per_tgt_stats = bootstrap_stats_array( data_subset['y'], transforms=[(lambda bw, f=n: bw / f) for n in nrs_targets])
            ax_bw_board.errorbar( nrs_targets, bw_per_tgt_stats['50_est'], yerr=bw_per_tgt_stats['50_err'], label=label)
        else:
            freq_stats = bootstrap_stats_array( data_subset['y'], transforms=[(lambda x, f=n*n_clients: f * 1e3 / x) for n in nrs_targets])
            ax_bw_board.errorbar( nrs_targets, freq_stats['50_est'], yerr=freq_stats['50_err'], label=label)
#        if bw:
#            bw_board_50est = numpy.divide( bw_stats['50_est'], n_clients )
#            bw_board_50err = numpy.divide( bw_stats['50_err'], n_clients )
#            ax_bw_board.errorbar(nrs_targets, bw_board_50est, yerr=bw_board_50err, label=label)
        
    # Labels

    for ax in [ax_bw_board, ax_bw_total, ax_ch_cpu, ax_ch_mem, ax_uhal_cpu, ax_uhal_mem]:
        ax.set_xlabel('Number of targets')
    if bw:
        ax_bw_board.set_ylabel('Throughput per target [Gbit/s]')
        ax_bw_total.set_ylabel('Total throughput [Gbit/s]')
    else:
        ax_bw_total.set_ylabel('Latency [us]')
        ax_bw_board.set_ylabel('Total frequency [kHz]')

    ax_ch_cpu.set_ylabel('ControlHub CPU usage [%]')
    ax_ch_mem.set_ylabel('ControlHub memory usage [%]')
    ax_uhal_cpu.set_ylabel('uHAL client CPU usage [%]')
    ax_uhal_mem.set_ylabel('uHAL client memory usage [%]')

     # Limits

    for ax in [ax_bw_board, ax_bw_total]:
        ax.set_xlim(numpy.min(data_subset['n_targets']))
        ax.set_ylim(0)

    for ax in [ax_ch_cpu, ax_uhal_cpu]:
        ax.set_ylim(0,400)
    for ax in [ax_ch_mem, ax_uhal_mem]:
        ax.set_ylim(0)

    ax_bw_total.legend(loc='best', fancybox=True)

    return [(fig, 'n_to_m_' + ('bw_' if bw else 'lat_') + ('tx' if write else 'rx'))]



#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

def measure_1_to_1_vs_pktLoss(target, controlhub_ssh_client, n_meas=10, depth=1, fractions=[x/1e3 for x in range(0, 11, 2)]):
    '''
    Measures latencies for single-word reads as function of fractional send/receive UDP packet loss on controlhub host.
    Returns structured array containing latencies in micro-secs.

    Arguments:
      target                 --  target hostname (or IP) & port in format "hostname:port"
      controlhub_ssh_client  --  ssh client instance for connecting to controlhub host
      n_meas                 --  number of measurements to take for each fractional 
    '''

    print "\n ---> MEASURING 1 to 1 performance vs fractionsl packet loss to '" + target + "' <---"
    print time.strftime('%l:%M%p %Z on %b %d, %Y')

    data = numpy.zeros(len(fractions), 
                       dtype=[('f','float32'),
                              ('latency', 'float32', (n_meas,)),
                             ]
                      )

    print fractions
    for i in range(len(fractions)):
        data['f'][i] = fractions[i]

    update_controlhub_sys_config(CH_MAX_IN_FLIGHT, controlhub_ssh_client, CH_SYS_CONFIG_LOCATION)
    start_controlhub(controlhub_ssh_client)

    cmd = 'PerfTester.exe -t BandwidthRx -b 0x2001 -w %d -i 10000 -p -d chtcp-2.0://%s:10203?target=%s' % (depth, CH_PC_NAME, target)
    cmd_fmt_add_pkt_loss = 'sudo /sbin/iptables -I {0} -p udp -m statistic --mode random --probability {1} -j DROP'
    cmd_fmt_del_pkt_loss = 'sudo /sbin/iptables -D {0} 1'

    for i in range(n_meas):
        SCRIPT_LOGGER.warning('packet loss measurements: iteration %d of %d' % (i+1,n_meas) )
 
        for entry in data:
            f = entry['f']

            if f != 0.0:
                run_command( cmd_fmt_add_pkt_loss.format('INPUT', f), controlhub_ssh_client )
                run_command( cmd_fmt_add_pkt_loss.format('OUTPUT', f), controlhub_ssh_client )

            entry['latency'][i] = run_command(cmd)[0]

            if f != 0.0:
                run_command( cmd_fmt_del_pkt_loss.format('INPUT'), controlhub_ssh_client )
                run_command( cmd_fmt_del_pkt_loss.format('OUTPUT'), controlhub_ssh_client )

    # Final cleanup
    stop_controlhub(controlhub_ssh_client)

    return data



def plot_1_to_1_vs_pktLoss(data):

    # Set up figure ...

    fig = plt.figure(figsize=(6,4.5))
    fig.subplots_adjust(left=.13, right=.95, bottom=.12, top=.94)
    ax  = fig.add_subplot(111)

    # Plot the datapoints ...

    lat_stats = bootstrap_stats_array( data['latency'] )

    prediction_x = numpy.linspace(min(data['f']), max(data['f']), 100)
    prediction_y = numpy.add( prediction_x * 2 * 2.01e4, lat_stats['50_est'][0] )
    prediction_y = numpy.add( prediction_y, prediction_x * prediction_x * 4 * 2e4 )
    ax.plot(prediction_x, prediction_y, 'g--')

    ax.errorbar(data['f'], lat_stats['50_est'], yerr=lat_stats['50_err'], fmt=' o', markersize=3)

    ax.set_xlabel('Fractional UDP packet loss')
    ax.set_ylabel('Latency [us]')
    ax.set_xlim(min(data['f']), max(data['f']) + 0.01*(max(data['f'])-min(data['f'])))

    return [(fig, '1_to_1_pktLoss')]



############################################################################################################
############################################################################################################
############################################################################################################

def meas_ipbus_extern_performance( target, ch_ssh_client, nrs_in_flight, n_meas):
    print "\n ---> MEASURING IPbus throughput vs number in flight from fixed-packet boost/Erlang clients to '" + target + "' <---"
    print time.strftime('%l:%M%p %Z on %b %d, %Y')

    data = numpy.zeros(len(nrs_in_flight), 
                       dtype=[('n','uint8'),
                              ('erlang', 'float32', (n_meas,)),
                              ('boost', 'float32', (n_meas,)),
                             ]
                      )

    print nrs_in_flight
    for i in range(len(nrs_in_flight)):
        data['n'][i] = nrs_in_flight[i]

    base_cmd_erl = ('/cactusbuild/tswsvn/network-examples/network_client.escript udp '
                     + '/cactusbuild/tswsvn/network-examples/data/ipbus2_pramWrite_send.dat' 
                     + ' /cactusbuild/tswsvn/network-examples/data/ipbus2_pramWrite_recv.dat '
                     + target.replace(':', ' '))
    base_cmd_cpp = ('/cactusbuild/tswsvn/network-examples/bin/udp_async_client '
                     + target.replace(':', ' ')
                     + ' /cactusbuild/tswsvn/network-examples/data/ipbus2_pramWrite_send.dat' 
                     + ' /cactusbuild/tswsvn/network-examples/data/ipbus2_pramWrite_recv.dat')

    for i in range(n_meas):
        SCRIPT_LOGGER.warning('IPbus Erlang/boost measurements: iteration %d of %d' % (i+1,n_meas) )
 
        for entry in data:
            n = entry['n']
            suffix = ' ' + str(6000*min(n,12))+ ' ' + str(n)

            entry['erlang'][i] = run_command( base_cmd_erl + suffix, ch_ssh_client, parser=parse_fixed_packet_client)[1]
            entry['boost'][i]  = run_command( base_cmd_cpp + suffix, ch_ssh_client, parser=parse_fixed_packet_client)[1]

    return data


def plot_ipbus_extern_performance(data, key_label_pairs):

    # Set up figure ...

    fig = plt.figure(figsize=(6,4.5))
    fig.subplots_adjust(left=.13, right=.95, bottom=.12, top=.94)
    ax  = fig.add_subplot(111)

    # Plot the datapoints ...

    for key, label in key_label_pairs:
        bw_stats = bootstrap_stats_array( data[key] )

        ax.errorbar(data['n'], bw_stats['50_est'], yerr=bw_stats['50_err'], fmt='-o', markersize=3, label=label)
 

    ax.set_xlabel('Number in flight')
    ax.set_ylabel('Median throughput [Gbit/s]')
    ax.set_ylim(0.)
    ax.set_xlim(0., max(data['n']) + 0.01*(max(data['n'])-min(data['n'])))

    ax.legend(loc='best', fancybox=True, numpoints=1)

    return [(fig, 'ipbus_extern_vs_nrInFlight')]



############################################################################################################
############################################################################################################
############################################################################################################

def meas_echo_full_frame_performance( target, ch_ssh_client, nrs_in_flight, n_meas):
    print "\n ---> MEASURING Echo throughput vs number in flight from fixed-packet boost/Erlang clients to '" + target + "' <---"
    print time.strftime('%l:%M%p %Z on %b %d, %Y')

    data = numpy.zeros(len(nrs_in_flight), 
                       dtype=[('n','uint8'),
                              ('udp_erlang', 'float32', (n_meas,)),
                              ('udp_boost', 'float32', (n_meas,)),
                              ('tcp_erlang', 'float32', (n_meas,)),
                             ]
                      )

    for i in range(len(nrs_in_flight)):
        data['n'][i] = nrs_in_flight[i]


    base_cmd_udp_erl = ('/cactusbuild/tswsvn/network-examples/network_client.escript udp '
                         + '/cactusbuild/tswsvn/network-examples/data/ipbus2_pramWrite_send.dat' 
                         + ' /cactusbuild/tswsvn/network-examples/data/ipbus2_pramWrite_send.dat '
                         + CH_PC_NAME + ' 10204')
    base_cmd_tcp_erl = base_cmd_udp_erl.replace(' udp ', ' tcp ')
    base_cmd_udp_cpp = ('/cactusbuild/tswsvn/network-examples/bin/udp_async_client '
                        + CH_PC_NAME + ' 10204' 
                        + ' /cactusbuild/tswsvn/network-examples/data/ipbus2_pramWrite_send.dat' 
                        + ' /cactusbuild/tswsvn/network-examples/data/ipbus2_pramWrite_send.dat')

    # Start servers 
    def run_echo_server(type):
        SCRIPT_LOGGER.warning(type + ' echo server starting')
        run_command('/cactusbuild/tswsvn/network-examples/echo_server.escript ' + type + ' 10204', ch_ssh_client, throw_on_bad_exit_code=False)
        SCRIPT_LOGGER.warning(type + ' echo server stopped')
    
    threading.Thread(target=run_echo_server, args=('udp',)).start()
    threading.Thread(target=run_echo_server, args=('tcp',)).start()

    time.sleep(2.0)

    # Run clients
    for i in range(n_meas):
        SCRIPT_LOGGER.warning('IPbus Erlang/boost measurements: iteration %d of %d' % (i+1,n_meas) )
 
        for entry in data:
            n = entry['n']
            suffix = ' ' + str(2000*min(n,30))+ ' ' + str(n)
            SCRIPT_LOGGER.warning('        * n = '+str(n)+'     suffix ='+suffix)

            nr_attempts = 0
            while True:
                nr_attempts += 1
                try:
                    entry['udp_erlang'][i] = run_command( base_cmd_udp_erl + suffix, parser=parse_fixed_packet_client)[1]
                    entry['udp_boost'][i]  = run_command( base_cmd_udp_cpp + suffix, parser=parse_fixed_packet_client)[1]
                except:
                    if nr_attempts <= 3:
                        SCRIPT_LOGGER.warning('Bad exit code / timeout from UDP client. Maybe packet dropped. Re-running ...')
                        continue
                    else:
                        raise
                break

            entry['tcp_erlang'][i] = run_command( base_cmd_tcp_erl + suffix, parser=parse_fixed_packet_client)[1]
            

    # Stop servers
    run_command(r'killall --regexp "\\.*beam\\.smp"', ch_ssh_client)


    return data


def plot_echo_full_frame_performance(data, key_label_pairs):

    # Set up figure ...

    fig = plt.figure(figsize=(6,4.5))
    fig.subplots_adjust(left=.13, right=.95, bottom=.12, top=.94)
    ax  = fig.add_subplot(111)

    # Plot the datapoints ...

    for key, label in key_label_pairs:
        bw_stats = bootstrap_stats_array( data[key] )

        ax.errorbar(data['n'], bw_stats['50_est'], yerr=bw_stats['50_err'], fmt='-o', markersize=3, label=label)
 

    ax.set_xlabel('Number in flight')
    ax.set_ylabel('Median throughput [Gbit/s]')
    ax.set_ylim(0.)
    ax.set_xlim(0., max(data['n']) + 0.01*(max(data['n'])-min(data['n'])))

    ax.legend(loc='best', fancybox=True, numpoints=1)

    return [(fig, 'echo_full_frame')]



############################################################################################################
############################################################################################################
############################################################################################################

def meas_echo_performance_vs_size( target, ch_ssh_client, sizes, n_meas):
    print "\n ---> MEASURING Echo throughput vs size from fixed-packet boost/Erlang clients to '" + target + "' <---"
    print time.strftime('%l:%M%p %Z on %b %d, %Y')

    data = numpy.zeros(len(sizes), 
                       dtype=[('size','uint32'),
                              ('tcp_erlang', 'float32', (n_meas,)),
                             ]
                      )

    for i in range(len(sizes)):
        data['size'][i] = sizes[i]


    # Start servers 
    def run_echo_server(type):
        SCRIPT_LOGGER.warning(type + ' echo server starting')
        run_command('/cactusbuild/tswsvn/network-examples/echo_server.escript ' + type + ' 10204', ch_ssh_client, throw_on_bad_exit_code=False)
        SCRIPT_LOGGER.warning(type + ' echo server stopped')
    
    threading.Thread(target=run_echo_server, args=('udp',)).start()
    threading.Thread(target=run_echo_server, args=('tcp',)).start()

    time.sleep(2.0)

    # Run clients
    for i in range(n_meas):
        SCRIPT_LOGGER.warning('IPbus Erlang/boost measurements: iteration %d of %d' % (i+1,n_meas) )
 
        for entry in data:
            size = entry['size']
            nr_itns = 60000 / (1 + size/1500)
            SCRIPT_LOGGER.warning('     * size: ' + str(size) + ' ;    ' + str(nr_itns) + ' iterations')

            cmd_tcp_erl = ('/cactusbuild/tswsvn/network-examples/network_client.escript tcp '
                           + '/cactusbuild/tswsvn/network-examples/data/zeros_'+str(size)+'.dat' 
                           + ' /cactusbuild/tswsvn/network-examples/data/zeros_'+str(size)+'.dat '
                           + CH_PC_NAME + ' 10204  ' + str(nr_itns) + ' 30')
            entry['tcp_erlang'][i] = run_command( cmd_tcp_erl, parser=parse_fixed_packet_client)[1]
            

    # Stop servers
    run_command(r'killall --regexp "\\.*beam\\.smp"', ch_ssh_client)


    return data


def plot_echo_performance_vs_size(data, key_label_pairs):

    # Set up figure ...

    fig = plt.figure(figsize=(6,4.5))
    fig.subplots_adjust(left=.13, right=.95, bottom=.12, top=.94)
    ax  = fig.add_subplot(111)

    # Plot the datapoints ...

    for key, label in key_label_pairs:
        bw_stats = bootstrap_stats_array( data[key] )

        ax.errorbar(data['size'], bw_stats['50_est'], yerr=bw_stats['50_err'], fmt='-o', markersize=3, label=label)
 

    ax.set_xlabel('Transport layer payload size [bytes]')
    ax.set_ylabel('Median throughput [Gbit/s]')
    ax.set_ylim(0.)
    ax.set_xlim(0.) # , max(data['n']) + 0.01*(max(data['n'])-min(data['n'])))

    ax.legend(loc='best', fancybox=True, numpoints=1)

    return [(fig, 'echo_vs_size')]



####################################################################################################
#  Highest-level plot / measure functions

def take_measurements(file_prefix, multiple_in_flight, externs):

    data = {'start_time' : time.localtime(),
            'fw_version' : FW_VERSION,
            'sw_version' : SW_VERSION,
            'client_host_name' : UHAL_PC_NAME,
            'bridge_host_name' : CH_PC_NAME,
            'controlhub_max_in_flight' : CH_MAX_IN_FLIGHT,
            'targets' : TARGETS,
            'multiple_in_flight' : multiple_in_flight
           }

    ch_ssh_client = ssh_into( CH_PC_NAME, CH_PC_USER )

    if externs:
        data['ipbus_extern_bw'] = meas_ipbus_extern_performance( TARGETS[0], ch_ssh_client, n_meas=20, nrs_in_flight=[1,2,3,4,6,8,10,12,14,16] )
        data['echo_full_frame_bw'] = meas_echo_full_frame_performance(TARGETS[0], ch_ssh_client, n_meas=20, nrs_in_flight=[1,2,4,6,8,12,16,20,24,28,30,32,34])
        data['echo_bw_vs_size'] = meas_echo_performance_vs_size( TARGETS[0], ch_ssh_client, n_meas=20, sizes=[200,400,600,800,1000,1200,1400, 2800, 4200, 5600, 7000] )
     
    else:
        ifmultiple = lambda a,b: a if multiple_in_flight else b

        data['1_to_1_latency'] = measure_1_to_1_latency( TARGETS[0], 
                                                         ch_ssh_client, 
                                                         n_meas = 100, 
                                                         max_depth = ifmultiple(1e7,1e4),
                                                         pkt_depths = ifmultiple([342,343], [250])
                                                       )

        data['1_to_1_vs_pktLoss'] = measure_1_to_1_vs_pktLoss( TARGETS[0], ch_ssh_client, n_meas=10 )

        data['n_to_m_lat'] = measure_n_to_m( TARGETS, ch_ssh_client, n_meas=4,
                                             f_words=lambda n,m: 3e4 / (1. + (n*m)*17./215.),
                                             bw=False, write=False,
                                             nrs_clients=[1,2,4]
                                           )

        n_words = ifmultiple(600,50) * 1000 * 1000 / 4
        data['n_to_m_bw_rx'] = measure_n_to_m( TARGETS, ch_ssh_client, n_meas=ifmultiple(4,3), f_words=n_words, write=False, nrs_clients=[1] )
        data['n_to_m_bw_tx'] = measure_n_to_m( TARGETS, ch_ssh_client, n_meas=ifmultiple(4,3), f_words=n_words, write=True,  nrs_clients=[1] )

        data['end_time'] = time.localtime()

    filename = file_prefix
    if not filename.endswith(".pkl"):
       filename += ".pkl"
    print "Saving measurements to:", filename

    pickle.dump( data, open(filename, "wb") )



def make_plots(input_file, externs):
    
    data = pickle.load( open(filename, "rb") )

    multiple_in_flight = data['multiple_in_flight']

    plots = []

    if externs:
        plots += plot_ipbus_extern_performance( data['ipbus_extern_bw'],
                                                [('erlang', 'Erlang'), ('boost', 'C++ (boost)')] )

        plots += plot_echo_full_frame_performance( data['echo_full_frame_bw'],
                      [('udp_erlang', 'UDP, Erlang'), ('udp_boost', 'UDP, boost'), ('tcp_erlang', 'TCP, Erlang')] )

        plots += plot_echo_performance_vs_size( data['echo_bw_vs_size'],
                                                [('tcp_erlang', 'Erlang')]
                                              )

    else:
        plots += plot_1_to_1_performance( data['1_to_1_latency'] , 
                                         [('ch_tx',  'Write'), ('ch_rx',  'Read')],
                                         words_per_pkt = 343 if multiple_in_flight else 250
                                        )

        plots += plot_1_to_1_vs_pktLoss( data['1_to_1_vs_pktLoss'] )

        plots += plot_n_to_m( data['n_to_m_lat'], bw=False, write=False )

        plots += plot_n_to_m( [(data['n_to_m_bw_tx'], "Write")] )

        plots += plot_n_to_m( [(data['n_to_m_bw_tx'], "Write"), (data['n_to_m_bw_rx'], "Read")], write=False )

    print time.strftime('%l:%M%p %Z on %b %d, %Y')

    while True:
        answer = raw_input('Shall I save this to file? ')

        if answer.lower() in ('y', 'yes'):
           prefix = re.sub('\.pkl$', '', filename)
           for fig, suffix in plots:
               pdfname = prefix + '.' +  time.strftime('%Y%m%d') + '__' + suffix + '.pdf'
               print ' + Saving plot to file:', pdfname
               fig.savefig( pdfname )
           break

        elif answer.lower() in ('n', 'no'):
           break

        else:
           print 'Invalid answer. Please type "y", "yes", "n", or "no" ...'

    plt.show()



####################################################################################################
#  MAIN

if __name__ == "__main__":

    # Parse args ...
    try:
       opts, args = getopt.getopt(sys.argv[1:], "hmp", ["help", "measure", "plot", "single", 'extern'])
    except getopt.GetoptError, e:
       print "ERROR in parsing arguments: ", e, "\n"
       print __doc__
       sys.exit(2)

    measure = False
    plot = False
    multiple_in_flight = True
    externs = False

    for opt, value in opts:
        if opt in ("-h", "--help"):
           print __doc__
           sys.exit(0)
        elif opt in ("-m", "--measure"):
           measure = True
        elif opt in ("-p", "--plot"):
           plot = True
        elif opt in ("--single"):
           multiple_in_flight = False
        elif opt in ("--extern"):
           externs = True


    if measure and plot:
        print "ERROR: Incorrect usage - cannot run in measure and plot modes at same time!"
        sys.exit(1)
    elif not measure and not plot:
        print "ERROR: Incorrect usage - must run either measure mode or plot mode!"
        sys.exit(1)

    if len(args) != 1:
        print "ERROR: Incorrect usage - wrong number of aguments!"
        print __doc__
        sys.exit(1)

    filename = args[0]

    if measure:
        take_measurements(filename, multiple_in_flight=multiple_in_flight, externs=externs)
    if plot:
        make_plots(filename, externs=externs)


