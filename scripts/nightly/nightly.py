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
import nanalyzer

def execute():
    logger.info("The following sections will be executed: %s" % ",".join([s for s,c in CONF.COMMANDS]))
    for section,cmds in CONF.COMMANDS:
        logger.info("----++++%s++++----" % section)
        for cmd in cmds:
            system(cmd,log=True, exception=False)

if __name__== "__main__":
    try:
        opts, args = getopt.getopt(sys.argv[1:], "sh", ["silent", "help"])
    except getopt.GetoptError, err:
        print __doc__
        sys.exit(2)

    silent = False
    for o, a in opts:
        if o in ("-s", "--silent"):
            silent=True
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
        execute()
    except KeyboardInterrupt,e:
        logger.warning('Aborting after CTRL-C...\n')
        sys.exit(1)
    except Exception,e:
        logger.error(e)


    if silent:
        logger.info("Final reporting and email notifications were disabled")
    else:
        logger.info("----++++REPORTING++++----")
        nanalyzer.report(CONF)
        system("mkdir -p %s" % CONF.NIGHTLY_LOG_DIR,log=True, exception=False)
        system( "sudo cp -r %s %s" % ("/var/log/*",CONF.NIGHTLY_LOG_DIR),log=True, exception=False)
