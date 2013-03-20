import os
import sys
from distutils.core import setup

VERSION_STRING = ( os.environ['PACKAGE_VER_MAJOR'] + '.' + os.environ['PACKAGE_VER_MINOR'] + '.' + os.environ['PACKAGE_VER_PATCH'] +
'_python' + str(sys.version_info[0]) + '.' + str(sys.version_info[1]) )


setup(name = 'cactuscore-uhal-gui',
      version = VERSION_STRING,
      description = 'Python GUI for uTCA HW access based on uHAL',
      author = 'Carlos Ghabrous Larrea',
      author_email = 'carlos.ghabrous@cern.ch',
      url = 'http://cactus.web.cern.ch/cactus',
      packages = ['uhal.gui','uhal.gui.guis','uhal.gui.customguis','uhal.gui.utilities','uhal.gui.test'], 
      scripts = ['testuhalgui.exe']
      )
