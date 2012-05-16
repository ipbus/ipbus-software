#!/usr/bin/python
"""
Usage: nightly.py [options] [configuration.py]
Executes the nightly build sript.

arguments:
   configuration.py  Python configuration file (default configuration.py)
   
options:
   -s, --silent      Do not create the final report and notifications (just logging)
   -k, --keep        Do not uninstall/wipe everything at the end

"""

from clone_setup import clone_setup
from nutils import logger, log_setup, system
import os
import time
import nanalyzer
import sys
import getopt

def environment():
    logger.info("----++++ENVIRONMENT++++----")
    for cmd in ENVIRONMENT_CMDS:
        system(cmd,log=True, exception=False)
    
def checkout():
    logger.info("----++++CHECKOUT++++----")

    for cmd in CHECKOUT_CMDS:
        system(cmd)

def dependencies():
    logger.info("----++++DEPENDENCIES++++----")
    
    for cmd in DEPENDENCIES_CMDS:
        system(cmd)

def build():
    logger.info("----++++BUILD++++----")

    for cmd in BUILD_CMDS:
        system(cmd, log=True, exception=False)

def release():
    logger.info("----++++RELEASE RPMS++++----")

    for cmd in RELEASE_CMDS:
        system(cmd)

def install():
    logger.info("----++++INSTALL++++----")

    for cmd in INSTALL_CMDS:
        system(cmd)


def test():
    logger.info("----++++TEST++++----")

    for cmd in TEST_CMDS:
        system(cmd,exception=False)
        

def uninstall():
    logger.info("----++++UNINSTALLING++++----")

    for cmd in UNINSTALL_CMDS:
        system(cmd, log=True, exception=False)

def processLogs():
    logger.info("----++++PROCESSING LOGS++++----")
    ### Copy all log files to LOG_DIR.
    src = "/var/log/*.{log,out,stderr,stdout}"
    dest = LOG_DIR
    system("mkdir -p %s" % dest, exception=False)
    system("sudo cp %s %s" % (src, dest), exception=False)

if __name__== "__main__":
    try:
        opts, args = getopt.getopt(sys.argv[1:], "sk", ["silent", "keep"])
    except getopt.GetoptError, err:
        print __doc__
        sys.exit(2)

    silent = False
    keep = False
    for o, a in opts:
        if o in ("-s", "--silent"):
            silent=True
        if o in ("-k", "--keep"):
            keep=True

    if len(args) == 0:
        global
    else:
        sys.stderr.write("ERROR: Wrong number of arguments\n\n")
        print __doc__
        sys.exit(2)
        
    # Execute build/test/etc.
    try:
        try:
            uninstall()
            environment()
            checkout()
            dependencies()
            build()
            release()
            install()
            test()
        except KeyboardInterrupt,e:
            logger.warning('Aborting after CTRL-C...\n')
            sys.exit(1)
        except Exception,e:
            logger.error(e)
    finally:
        if not keep:
            uninstall()

    # Reporting.
    try:
        if not silent:
            logger.info("----++++REPORTING++++----")
            nanalyzer.report()
            processLogs()
        else:
            logger.info("Final reporting and email notifications were disabled")

    except KeyboardInterrupt, e:
        logger.warning('Aborting after CTRL-C...\n')
        sys.exit(1)
    except Exception, e:
        logger.error(e)
