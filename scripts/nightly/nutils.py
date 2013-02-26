import string
import sys
import os
import logging
import subprocess
import fcntl
import time

#maximum seconds without logs
SOFT_TIMEOUT_S = 10*60
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
        
    p  = subprocess.Popen(cmd,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,stdin=None,bufsize=0,shell=True)
    fl = fcntl.fcntl(p.stdout, fcntl.F_GETFL)
    fcntl.fcntl(p.stdout, fcntl.F_SETFL, fl | os.O_NONBLOCK)

    content = ""
    
    start = time.time()
    last = start
    current = start
    while True:
        try:
            nextline = filter(lambda x: x in string.printable,p.stdout.readline())
            content = content + nextline
            sys.stdout.write(nextline)
            sys.stdout.flush()
            last = time.time()
        except IOError:
            time.sleep(0.1)

            current = time.time()    
            if (current-start) > HARD_TIMEOUT_S:
                system("kill -9 %d" % p.pid,log=False,exception=False)
                report_error("ERROR: %s (command too slow, timeout of %d sec)" % (cmd,HARD_TIMEOUT_S),exception,log)
                return (content,-9)
            
            if (current-last) > SOFT_TIMEOUT_S:
                system("kill -9 %d" % p.pid,log=False,exception=False)
                report_error("ERROR: %s (unresponsive command, missing output for %d sec)" % (cmd,SOFT_TIMEOUT_S),exception,log)
                return (content,-9)
                
            if p.poll()!= None:
                break
            
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
