"""
usage: uhalgui.py [options] <guis python list>
Instantiates all GUIs being part of the list
                                                        
where:                                      
<guis python list> is a space separated list containing all GUIs the user wants to instantiate.
                                            
options:
-h, --help          prints the documentation
"""

import getopt
import sys
import os
from uhal.gui.guiloader import loader


try:
    opts, args = getopt.getopt(sys.argv[1:], "h", ["help"])
except getopt.GetoptError as err:
    print(__doc__)
    sys.exit(2)


directory = ""

for o, a in opts:
    if o in ("-h","--help"):
        print(__doc__)
        sys.exit(0)


gui_list = []
try:
    path = os.path.abspath(os.getcwd())
    if not path in sys.path:
        sys.path.append(path)

    for gui in args:
        print(gui)
        gui_list.append(__import__(gui))

    guis = loader(guilist=gui_list)
    guis.start()
except Exception as e:
    sys.stderr.write("ERROR: %s\n" % str(e))
    sys.exit(1)
