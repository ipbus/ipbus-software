import string
import sys
import logging
import subprocess


def system(cmd, exception = True, log=True):
    '''Execute shell commands

    Note: if no output is received, even if the return value is
    non-zero, nothing is logged! This assumes the user piped the
    output to /dev/null or so to keep it quiet.

    '''

    if log:
        logger.info("%s..." % cmd)
    
    p  = subprocess.Popen(cmd,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,shell=True)
    content = ""
    while True:
        nextline = filter(lambda x: x in string.printable,p.stdout.readline())
        if nextline:
            content = content + nextline
            sys.stdout.write(nextline)
            sys.stdout.flush()

        if p.poll() != None:
            break

    if p.poll():
        tmp = "%s (error=%s)" % (cmd, p.poll())
        if exception:
            raise Exception(tmp)
        elif log:
            logger.error(tmp)
        
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
