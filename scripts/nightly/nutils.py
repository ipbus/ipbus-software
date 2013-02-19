import time
import logging
import subprocess

TIMEOUT_S = 10*60

def system(cmd, exception = True, log=True):
    '''Execute shell commands

    Note: if no output is received, even if the return value is
    non-zero, nothing is logged! This assumes the user piped the
    output to /dev/null or so to keep it quiet.

    '''

    p  = subprocess.Popen(cmd,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,shell=True)
    content = p.communicate()[0]

    start = time.time()
    current=start
    global TIMEOUT_S
    while ((current-start)<TIMEOUT_S and p.poll() != None):
        time.sleep(1)
        current = time.time()

    if p.poll() == None:
        tmp = "%s (error=timeout)" % cmd
        if exception:
            raise Exception("%s (error=timeout)" % cmd)
        else:
            logger.error("%s (error=timeout)" % cmd)

    if p.poll():
        tmp = "%s (error=%s)" % (cmd, p.poll())
        if len(content):
            tmp = "%s:\n%s" % (tmp, content)
        if exception:
            raise Exception(tmp)
        elif log:
            logger.error(tmp)
    elif log:
        if len(content):
            logger.info("%s\n%s" % (cmd,content))
        else:
            logger.info(cmd)

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
