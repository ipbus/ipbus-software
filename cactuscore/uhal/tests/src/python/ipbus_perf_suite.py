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
import threading
import time


####################################################################################################
#  GLOBAL OPTIONS

TEST_CMD_TIMEOUT_S = 120

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
SCRIPT_LOG_FORMATTER = logging.Formatter("%(asctime)-15s [0x%(thread)x] %(levelname)-7s > %(message)s")
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
    SCRIPT_LOGGER.debug("Running (locally): "+cmd)
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
    SCRIPT_LOGGER.debug("Running (remotely): "+cmd)
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


#CPU_MEM_PSAUX_REGEX = re.compile("^\\w+\\s+\\d+\\s+([\\d+\\.]+)\\s+([\\d+\\.]+)\\s+"  # USER PID CPU MEM
#                                 "\\d+\\s+\\d+\\s+\\S+\\s+" # VSZ   RSS TTY      
#                                 "\\S+\\s+\\S+\\s+[\\d:]+\\s+" # STAT START   TIME 
#                                 ".+" # COMMAND
#                                 )

def cpu_mem_usage(cmd_to_check, ssh_client=None):
    assert " " not in cmd_to_check

    output = run_command("top -b -n 1 | grep "+cmd_to_check, ssh_client=ssh_client)[1]
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
                    meas_cpu, meas_mem = cpu_mem_usage(cmd, ssh_client)
#                    print meas_cpu, meas_mem
                    cpu_vals.append(meas_cpu)
                    mem_vals.append(meas_mem)
            except CommandBadExitCode as e:
                if not self._cmd_completed:
                    raise
            time.sleep(0.05) 
        cpu_vals.pop()
        mem_vals.pop() 

        # Wait (w/o monitoring)
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
    for i in range(5):
        for d in depths:
            udp_bandwidths[d].append( run_command("PerfTester.exe -t BandwidthTx -b 0x1000 -w "+str(d)+" -p -i 2000 -d "+udp_uri, 
                                                  ssh_client=controlhub_ssh_client)[1] )
    udp_bws_mean, udp_bws_yerrors = calc_y_with_errors(udp_bandwidths)
    plt.errorbar(depths, udp_bws_mean, yerr=udp_bws_yerrors, label="Direct UDP (CH PC)")

    ch_uri = "chtcp-2.0://" + CH_PC_NAME + ":10203?target=" + target
    for nr_in_flight in [1,4,8,16]:
        ch_bandwidths = dict((x, []) for x in depths)

        update_controlhub_sys_config(nr_in_flight, controlhub_ssh_client, CH_SYS_CONFIG_LOCATION)
        start_controlhub(controlhub_ssh_client)
        for i in range(5):
            for d in depths:
                ch_bandwidths[d].append( run_command("PerfTester.exe -t BandwidthTx -b 0x1000 -w "+str(d)+" -p -i 2000 -d "+ch_uri)[1] )
        ch_bws_mean, ch_bws_yerrors = calc_y_with_errors(ch_bandwidths)
        plt.errorbar(depths, ch_bws_mean, yerr=ch_bws_yerrors, label="Via CH, "+str(nr_in_flight)+" in-flight")

        stop_controlhub(controlhub_ssh_client)

    plt.xlabel("Number of words")
    plt.ylabel("Write bandwidth [Mb/s]")
#    plt.xscale("log")
    plt.legend(loc='lower right')
    plt.title(gethostname() + "  ->  " + CH_PC_NAME + "  ->  " + target)


def measure_bw_vs_nInFlight(target, controlhub_ssh_client):
    '''Measures continuous write/read bandwidth for block writes to given endpoint'''
    print "\n ---> BANDWIDTH vs NR_IN_FLIGHT to '" + target + "' <---"

    nrs_in_flight = [1,2,4,5,6,7,8,10,12,16]
    cmd_base = "PerfTester.exe -b 0x1000 -w 1024 -i 10240 -d chtcp-2.0://" + CH_PC_NAME + ":10203?target=" + target

    ch_tx_bws = dict((x, []) for x in nrs_in_flight)
    ch_rx_bws  = dict((x, []) for x in nrs_in_flight)

    for i in range(3):
        for n in nrs_in_flight:
            update_controlhub_sys_config(n, controlhub_ssh_client, CH_SYS_CONFIG_LOCATION)
            start_controlhub(controlhub_ssh_client)
            ch_tx_bws[n].append( run_command(cmd_base + " -t BandwidthTx")[1] )
            ch_rx_bws[n].append( run_command(cmd_base + " -t BandwidthRx")[1] )
            stop_controlhub(controlhub_ssh_client)

    ch_tx_bws_mean, ch_tx_bws_yerrors = calc_y_with_errors(ch_tx_bws)
    ch_rx_bws_mean, ch_rx_bws_yerrors = calc_y_with_errors(ch_rx_bws)
    
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.errorbar(nrs_in_flight, ch_tx_bws_mean, yerr=ch_tx_bws_yerrors, label='Write via CH')
    ax.errorbar(nrs_in_flight, ch_rx_bws_mean, yerr=ch_rx_bws_yerrors, label='Read via CH')
    ax.set_xlabel('Number in flight over UDP')
    ax.set_ylabel('Bandwidth [Mb/s]')
    ax.legend(loc='lower right')
    ax.set_title('40MB read/write bandwidth')


def measure_bw_vs_nClients(targets, controlhub_ssh_client):
    '''Measures continuous block-write bandwidth from to all endpoints, varying the number of clients.'''
    print "\n ---> BANDWIDTH vs NR_CLIENTS to", targets, "<---"

    cmd_base = "PerfTester.exe -t BandwidthTx -w 1024 -d chtcp-2.0://" + CH_PC_NAME + ":10203?target="
    cmd_runner = CommandRunner( [('PerfTester.exe',None), ('beam.smp',controlhub_ssh_client)] )

    nrs_clients = [1,2,3,4,5,6,8]
    nrs_targets = range(1, len(targets)+1)
    bws_per_board  = dict( ((x,z), []) for x in nrs_clients for z in nrs_targets )
    bws_per_client = dict( ((x,z), []) for x in nrs_clients for z in nrs_targets )
    ch_cpu_vals = dict( ((x,z), []) for x in nrs_clients for z in nrs_targets )
    ch_mem_vals = dict( ((x,z), []) for x in nrs_clients for z in nrs_targets )
    uhal_cpu_vals = dict( ((x,z), []) for x in nrs_clients for z in nrs_targets )
    uhal_mem_vals = dict( ((x,z), []) for x in nrs_clients for z in nrs_targets )
    
    update_controlhub_sys_config(16, controlhub_ssh_client, CH_SYS_CONFIG_LOCATION)
    start_controlhub(controlhub_ssh_client)

    for i in range(5):
        for n_clients in nrs_clients:
            for n_targets in nrs_targets:
                cmds = [cmd_base + t + ' -i ' + str(int(51200/n_clients)) for t in targets[0:n_targets] for x in range(n_clients)]
                monitor_results, cmd_results = cmd_runner.run(cmds)
                bws = [x[1] for x in cmd_results]

                dict_idx = (n_clients, n_targets)
                uhal_cpu_vals[dict_idx].append( monitor_results[0][1] )
                uhal_mem_vals[dict_idx].append( monitor_results[0][2] )
                ch_cpu_vals[dict_idx].append( monitor_results[1][1] )
                ch_mem_vals[dict_idx].append( monitor_results[1][2] )
                bws_per_board[dict_idx].append( sum(bws)/n_targets )
                bws_per_client[dict_idx].append( sum(bws)/len(bws) )
        
    stop_controlhub(controlhub_ssh_client)

    fig = plt.figure()
    ax_bw_board  = fig.add_subplot(231)
    ax_bw_client = fig.add_subplot(234, ylim=[0,300])
    ax_ch_cpu = fig.add_subplot(232, ylim=[0,400])
    ax_ch_mem = fig.add_subplot(235, ylim=[0,100])
    ax_uhal_cpu = fig.add_subplot(233, ylim=[0,400])
    ax_uhal_mem = fig.add_subplot(236, ylim=[0,100])

    for n_targets in nrs_targets:
        label = str(n_targets) + ' targets'
        bws_per_board_mean , bws_per_board_errs  = calc_y_with_errors( dict(x for x in bws_per_board.items() if x[0][1]==n_targets) )
        bws_per_client_mean, bws_per_client_errs = calc_y_with_errors( dict(x for x in bws_per_client.items() if x[0][1]==n_targets) )
        ch_cpu_mean, ch_cpu_errs = calc_y_with_errors( dict(x for x in ch_cpu_vals.items() if x[0][1]==n_targets) )
        ch_mem_mean, ch_mem_errs = calc_y_with_errors( dict(x for x in ch_mem_vals.items() if x[0][1]==n_targets) )
        uhal_cpu_mean, uhal_cpu_errs = calc_y_with_errors( dict(x for x in uhal_cpu_vals.items() if x[0][1]==n_targets) )
        uhal_mem_mean, uhal_mem_errs = calc_y_with_errors( dict(x for x in uhal_mem_vals.items() if x[0][1]==n_targets) )

        ax_bw_board.errorbar(nrs_clients, bws_per_board_mean, yerr=bws_per_board_errs, label=label)
        ax_bw_client.errorbar(nrs_clients, bws_per_client_mean, yerr=bws_per_client_errs, label=label)
        ax_ch_cpu.errorbar(nrs_clients, ch_cpu_mean, yerr=ch_cpu_errs, label=label)
        ax_ch_mem.errorbar(nrs_clients, ch_mem_mean, yerr=ch_mem_errs, label=label)
        ax_uhal_cpu.errorbar(nrs_clients, uhal_cpu_mean, yerr=uhal_cpu_errs, label=label)
        ax_uhal_mem.errorbar(nrs_clients, uhal_mem_mean, yerr=uhal_mem_errs, label=label)
        
    for ax in [ax_bw_board, ax_bw_client, ax_ch_cpu, ax_ch_mem, ax_uhal_cpu, ax_uhal_mem]:
        ax.set_xlabel('Number of clients per board')

    ax_bw_board.set_ylabel('Total bandwidth per board [Mb/s]')
    ax_bw_client.set_ylabel('Bandwidth per client [Mb/s]')
    ax_ch_cpu.set_ylabel('ControlHub CPU usage [%]')
    ax_ch_mem.set_ylabel('ControlHub memory usage [%]')
    ax_uhal_cpu.set_ylabel('uHAL client CPU usage [%]')
    ax_uhal_mem.set_ylabel('uHAL client memory usage [%]')

    ax_ch_mem.legend(loc='lower right')
#    ax.set_title('a title')


####################################################################################################
#  MAIN

if __name__ == "__main__":
    controlhub_ssh_client = ssh_into( CH_PC_NAME, CH_PC_USER )

    measure_bw_vs_nClients(TARGETS, controlhub_ssh_client)

#    measure_bw_vs_nInFlight(TARGETS[0], controlhub_ssh_client)
    #measure_latency(TARGETS[0], controlhub_ssh_client)

#    measure_bandwidth_vs_depth(TARGETS[0], controlhub_ssh_client)

    plt.show()

