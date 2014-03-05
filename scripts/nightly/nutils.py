import logging
import subprocess
import time
import os
import fcntl
import sys

#maximum seconds without logs
SOFT_TIMEOUT_S = 20*60
#maximum seconds to execute a command 
HARD_TIMEOUT_S = 120*60

def report_error(msg,exception,log):
    if exception:
        raise Exception(msg)
    elif log:
        global logger
        logger.error(msg)


def system(cmd, exception = True, log=True):
    '''Execute shell comman    
    '''
    
    if log:
        logger.info("%s..." % cmd)
        
    p  = subprocess.Popen(cmd,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,stdin=None,shell=True)
    fl = fcntl.fcntl(p.stdout, fcntl.F_GETFL)
    fcntl.fcntl(p.stdout, fcntl.F_SETFL, fl | os.O_NONBLOCK)

    content = ""
    
    start = time.time()
    last = start

    while True:
        current = time.time()
        try:
            line = p.stdout.readline()
            if not line:
                break

            content = content + line
            sys.stdout.write(line)
            sys.stdout.flush()
            last = time.time()
        except IOError:
            time.sleep(0.1)

            if (current-start) > HARD_TIMEOUT_S:
                report_error("ERROR: %s (command too slow, timeout of %d sec)" % (cmd,HARD_TIMEOUT_S),exception,log)
                return (content,-1)
            elif (current-last) > SOFT_TIMEOUT_S:
                report_error("ERROR: %s (unresponsive command, missing output for %d sec)" % (cmd,SOFT_TIMEOUT_S),exception,log)
                return (content,-1)
                                
    if p.poll():
        report_error("ERROR: %s (error=%s)" % (cmd, p.poll()),exception,log)

    return (content, p.poll())




def log_setup():
    global logger
    logger = logging.getLogger("CMS-L1OSW")
    logger.setLevel(logging.INFO)
    __formatter = logging.Formatter('%(asctime)s %(levelname)s (%(filename)s:%(lineno)d) %(message)s')
    __h = logging.StreamHandler()
    __h.setFormatter(__formatter)
    logger.addHandler(__h)

    return logger

logger = log_setup()
