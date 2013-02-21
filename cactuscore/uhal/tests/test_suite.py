#!/usr/bin/python

import sys , os, subprocess, string, inspect, time

path_to_here = os.path.dirname( os.path.abspath( __file__ ) )


cactus_py = os.path.join( path_to_here , "../../../scripts/nightly/cactus.py" )

cactus_py_path , cactus_py_file = os.path.split( cactus_py )
cactus_py_module , cactus_py_ext = os.path.splitext( cactus_py_file )
sys.path.insert(0, cactus_py_path )

LD_LIBRARY_PATH = os.environ['LD_LIBRARY_PATH']
PATH = os.environ['PATH']


try:
  CONF = __import__( cactus_py_module )
except ImportError,e:
  sys.stderr.write("ERROR: Failed to import '%s': %s\n\n" % (cactus_py,str(e)))
  exit( 1 )


try:
  CONF.environ['LD_LIBRARY_PATH'] += ":"+LD_LIBRARY_PATH
  CONF.environ['PATH'] += ":"+PATH

  for section,cmds in CONF.COMMANDS:
    if not "TEST IPBUS 2.0" in section:
      print "Skipping section:" , section
    else:
      print "Entering section:" , section
      for cmd in cmds:

        split_cmds = cmd.split()
      
        if "pycohal" in split_cmds[0]:
          continue

        if( len(sys.argv) ):
          if "test_" in split_cmds[0]:
            split_cmds.append( " ".join(sys.argv) );

        if ".exe" in split_cmds[0]:
          split_cmds[0] = os.path.join( path_to_here , "bin" , split_cmds[0] )

        split_cmds = [ i.replace( "file:///opt/cactus" , "file://"+path_to_here ) for i in split_cmds ]

        print "-----------------------------------------------------------------------------------------------------------------------------------------------------------"
        cmd = " ".join( split_cmds )
        p  = subprocess.Popen( cmd ,stdout=subprocess.PIPE,stderr=subprocess.STDOUT, shell=True)

        while True:
          line = p.stdout.readline()
          print line,
          nextline = filter(lambda x: x in string.printable,line)

          if p.poll()!= None and not nextline:
            break


        if p.poll():
					tmp = ("\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"+
								 "%s (error=%s)"+
								 "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n") % (cmd, p.poll())
					raise Exception( tmp )

except Exception as inst:
		print inst.args[0]
		subprocess.Popen( "pkill -f \"DummyHardwareUdp.exe\"" , shell=True)
		subprocess.Popen( "pkill -f \"DummyHardwareTcp.exe\"" , shell=True)



