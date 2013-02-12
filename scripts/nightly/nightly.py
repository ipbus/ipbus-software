#!/usr/bin/python
"""
Usage: nightly.py [options] [configuration.py]
Executes the nightly build sript. The system is not cleaned up after its use. 

arguments:
   configuration.py  Python configuration file (default configuration.py)
   
options:
   -h, --help        help
   -s, --silent      Do not create the final report and notifications (just logging)
"""

from nutils import logger, log_setup, system
import os
import sys
import getopt

def environment():
    logger.info("----++++ENVIRONMENT++++----")
    for cmd in CONF.ENVIRONMENT_CMDS:
        system(cmd,log=True, exception=False)
    
def checkout():
    logger.info("----++++CHECKOUT++++----")

    for cmd in CONF.CHECKOUT_CMDS:
        system(cmd)

def dependencies():
    logger.info("----++++DEPENDENCIES++++----")
    
    for cmd in CONF.DEPENDENCIES_CMDS:
        system(cmd, exception=False)

def build():
    logger.info("----++++BUILD++++----")

    for cmd in CONF.BUILD_CMDS:
        system(cmd, log=True, exception=False)

def release():
    logger.info("----++++RELEASE RPMS++++----")

    for cmd in CONF.RELEASE_CMDS:
        system(cmd)

def install():
    logger.info("----++++INSTALL++++----")

    for cmd in CONF.INSTALL_CMDS:
        system(cmd)


def test():
    logger.info("----++++TEST++++----")

    for cmd in CONF.TEST_CMDS:
        system(cmd,exception=False)
        

def uninstall():
    logger.info("----++++UNINSTALLING++++----")

    for cmd in CONF.UNINSTALL_CMDS:
        system(cmd, log=False, exception=False)

def report():
    logger.info("----++++REPORTING++++----")

    for cmd in CONF.REPORT_CMDS:
        system(cmd,exception=False)
        

if __name__== "__main__":
    try:
        opts, args = getopt.getopt(sys.argv[1:], "skh", ["silent", "keep","help"])
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
        if o in ("-h", "--help"):
            print __doc__
            sys.exit(0)
            
    if len(args) == 1:
        global CONF
        try:
            p,fn = os.path.split(args[0])
            n,ext = os.path.splitext(fn)
            CONF = __import__(n)
        except ImportError,e:
            sys.stderr.write("ERROR: Failed to import '%s': %s\n\n" % (args[0],str(e)))
    else:
        sys.stderr.write("ERROR: Wrong number of arguments\n\n")
        print __doc__
        sys.exit(2)
        
    # Execute build/test/etc.
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

    # Reporting.
    try:
        if not silent:
            report()
        else:
            logger.info("Final reporting and email notifications were disabled")

    except KeyboardInterrupt, e:
        logger.warning('Aborting after CTRL-C...\n')
        sys.exit(1)
    except Exception, e:
        logger.error(e)
