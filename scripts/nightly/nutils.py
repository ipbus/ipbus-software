import logging
import subprocess
import time
import Queue
import threading

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

def reader(f,q):
    for line in iter(f.readline,None):
        q.put(line)

def system(cmd, exception = True, log=True):
    '''Execute shell comman    
    '''
    
    if log:
        logger.info("%s..." % cmd)
        
    p  = subprocess.Popen(cmd,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,stdin=None,shell=True)
    q = Queue.Queue()
    t = threading.Thread(target=reader, args=(p.stdout, q))
    t.setDaemon(True)
    t.start() 
   
    start = time.time()
    content = ""
    while True:
        current = time.time()
        try:
            l = q.get(True,SOFT_TIMEOUT_S)
            if l == None:
                break
            
            if log:
                print l,
            content = content + l
        except Queue.Empty:
            
            report_error("ERROR: %s (unresponsive command, missing output for %d sec)" % (cmd,SOFT_TIMEOUT_S),exception,log)
            return (content,-1)

        if p.poll()!= None:
            break
        elif (current-start) > HARD_TIMEOUT_S:
            report_error("ERROR: %s (command too slow, timeout of %d sec)" % (cmd,HARD_TIMEOUT_S),exception,log)
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
