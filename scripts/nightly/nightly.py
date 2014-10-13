#!/usr/bin/python
"""
Usage: nightly.py [options] [configuration.py]
Executes the nightly build script. The system is not cleaned up after its use. 

arguments:
   configuration.py  Python configuration file (default configuration.py)
   
options:
   -h, --help                Help
   -s, --silent              Do not create the final report and notifications (just logging)
   -l, --list                Print the list of available commands in the given configuration file
   
   --group=<group>           Choose which group of nightly build targets to execute. 
                             Available options are: TS_DEV, SUBSYSTEM_DEV, SUBSYSTEM_904
   --commands=<list>         Provide list of commands to execute. If empty, the default set of 
                             commands for the nightly builds will be executed
   --emailto=<email>         Where to send the email report (-s will override this)
   --checkout_as=<username>  Use a different username to checkout the code than the current user
   --build_home=<path>       Specify a custom path to use as build home                     
"""

from nutils import logger, log_setup, system
import os
import sys
import getopt
import nanalyzer
import getpass

def execute(this_nightly):
    logger.info("The following set of commands is available for execution: %s" % ",".join([s for s,c in this_nightly.COMMANDS]))
    logger.info("The following set of commands will be executed: %s" % ",".join(commandsToRun))
    
    if ( "CLEANUP_WWW_AREA" in commandsToRun ) :
      logger.info("Cleaning up logs area...")
      this_nightly.cleanupLogs()
    for command in commandsToRun :
      for section,cmds in this_nightly.COMMANDS:
        if section == command : 
          logger.info("----++++ %s ++++----" % section)
          for cmd in cmds:
              system(cmd,log=True, exception=False)

if __name__== "__main__":
    try:
        opts, args = getopt.getopt(sys.argv[1:], "shl", ["silent", "help", "list", "group=", "commands=", "emailto=", "checkout_as=", "build_home="])
    except getopt.GetoptError, err:
        print __doc__
        sys.exit(2)

    global commandsToRun
    commands_via_param = ""
    
    silent = False
    listonly = False
    nightly_group = False
    USER_NAME = getpass.getuser()
    checkout_as = USER_NAME
    to_email = USER_NAME +  "@cern.ch"
    build_home="/build/"+USER_NAME+"/cactus"
    
    for o, a in opts:
        if o in ("-s", "--silent"):
            silent=True
        if o in ("-h", "--help"):
            print __doc__
            sys.exit(0)
        if o in ("-l", "--list"):
            listonly=True
        if o in ("--commands"):
            commands_via_param=a.split(",")
        if o in ("--emailto"):
            to_email=a
        if o in ("--checkout_as"):
            checkout_as=a
        if o in ("--build_home"):
            build_home=a
        if o in ("--group"):
            nightly_group=a
            
    if len(args) != 1:
	sys.stderr.write("ERROR: Wrong number of arguments\n\n")
        print __doc__
        sys.exit(2)	

    try:
	p,fn = os.path.split(args[0])
        n,ext = os.path.splitext(fn)
            
        if n == "ts_dev" and nightly_group not in ("TS_DEV","SUBSYSTEM_904","SUBSYSTEM_DEV"):
            sys.stderr.write("ERROR: Need to define group to run on\n\n")
            print __doc__
            sys.exit(2)
              
        if n == "uhal_config" : 
            nightly_group="UHAL"
        
	CONF = __import__(n)      
        this_nightly = CONF.Nightly(nightly_group, to_email, checkout_as, build_home)
            
    except ImportError,e:
        sys.stderr.write("ERROR: Failed to import '%s': %s\n\n" % (args[0],str(e)))
          
    # If no command list is given, use the default set
    commandsToRun = this_nightly.DEFAULT_COMMANDS
    if commands_via_param:
        commandsToRun = commands_via_param

      
    if listonly :
      print "The following set of commands is available for execution: %s" % ",".join([s for s,c in this_nightly.COMMANDS])
      sys.exit(0)

    # Check if command list given is valid
    for command in commandsToRun:
      
        if command not in [s for s,c in this_nightly.COMMANDS]:
            sys.stderr.write( "ERROR: Command '" + command + "' is not available.\n" )
            sys.stderr.write( "List of available commands : %s\n" % str([s for s,c in this_nightly.COMMANDS]) )
            sys.exit(2)
    
    if not silent:
        logger.info("Reports will be sent to: %s" % this_nightly.TO_EMAIL )
    
    # Execute build/test/etc.
    try:
        execute(this_nightly)
    except KeyboardInterrupt,e:
        logger.warning('Aborting after CTRL-C...\n')
        sys.exit(1)
    except Exception,e:
        logger.error(e)


    if silent:
        logger.info("Final reporting and email notifications were disabled")
    else:
        logger.info("----++++REPORTING++++----")
        system("mkdir -p %s" % this_nightly.NIGHTLY_LOG_DIR,log=True, exception=False)
        system("sudo mv %s %s" % ("/tmp/nightly.log",this_nightly.NIGHTLY_LOG_DIR),log=True, exception=False)
        system("sudo cp -r %s %s" % ("/var/log/*",this_nightly.NIGHTLY_LOG_DIR),log=True, exception=False)
        nanalyzer.report(this_nightly)
        
