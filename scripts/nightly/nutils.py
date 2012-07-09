import logging
import subprocess


def system(cmd, exception = True, log=True):
    '''Execute shell commands

    Note: if no output is received, even if the return value is
    non-zero, nothing is logged! This assumes the user piped the
    output to /dev/null or so to keep it quiet.

    '''

    p  = subprocess.Popen(cmd,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,shell=True)
    content, err = p.communicate()
    error = p.wait()

    if error:
        if exception:
            tmp = "%s (error=%s)" % (cmd, error)
            if len(content):
                tmp = "%s:\n %s" % (tmp, content)
            raise Exception(tmp)
        else:
            if len(content):
                tmp = "%s (error=%s): \n%s" % (cmd, error, content)
                logger.error(tmp)
    elif log:
        if content:
            logger.info(cmd + "\n" + content)
        else:
            logger.info(cmd)

    return (content, error)

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
