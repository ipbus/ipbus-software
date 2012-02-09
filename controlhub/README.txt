
-----------------
IPbus Control Hub
-----------------

  Robert Frazier
    Bristol University



Please note that the Control Hub software is still in the development stage and currently
you run it from "user-land" by downloading the code and starting it yourself.  Ultimately,
it will be provided as a daemon/service that can start up on boot, but that is a feature
for the future! 



Quickstart Guide (Linux & OSX):
-------------------------------

1)  Install the Erlang runtime for your system

  See here for details: http://www.erlang.org/download.html

  You'll probably have to install from source in the standard way (configure, make, make install, etc)


2)  Checkout the Control Hub code

  The following command will check the code into your home directory:

    svn co http://svn.hepforge.org/cactus/trunk/ControlHub ~/ControlHub
  

3)  Compile the ControlHub code

  Go to your ControlHub installation folder, then to compile the code do:

    escript make.escript
    

4)  Set the CONTROL_HUB environment variable

  Put the following line in your .bashrc or .bash_profile, or put it into a setup.sh file that you
  can source before running the ControlHub:
  
    export CONTROL_HUB=~/ControlHub

  If you didn't install the Control Hub within your home directory, edit the above command as appropriate.


5)  Create a device address file

  Create a device address file for the boards/devices attached to your Control Hub machine.
  An example of this can be found within your ControlHub install directory here:
  
    test/config/DeviceAddressFile.erl 
  
  The device address file simply maps a unique, user-defined device ID number to that device's
  IP address and port number. 


6)  Running the Control Hub

  If it isn't already executable, change the permissions on the 'startControlHub.sh' script:
  
    chmod u+x startControlHub.sh 

  Now start the Control Hub using the above script along with the path to your device address file.
  For example:
  
    ./startControlHub.sh $CONTROL_HUB/test/config/DeviceAddressFile.erl
    
  Now just leave it running.  The Control Hub answers incoming MicroHAL/Redwood requests on port 10203.
  

7)  Stopping the Control Hub

  Press ctrl-c, then press 'a' and hit return. The Erlang virtual machine will now shut down.
  

8)  Resolving problems with the Control Hub

  If you are suffering problems with the Control Hub, it is possible to run it in debug mode.  This
  dramatically affects the performance of the Control Hub (because it prints many messages to the
  console), and thus should only be used when necessary.
  
  Running in debug mode requires enabling some debug macros in the source code and then recompiling.  To
  do this, open the 'include/trace_macro.hrl' file and uncomment (remove the leading % symbol) line 3 and
  line 6.  The comments above these lines should make it clear what these macros do.  Now recompile the
  Control Hub by repeating step 3 above.


Quickstart Guide (Windows):
---------------------------
