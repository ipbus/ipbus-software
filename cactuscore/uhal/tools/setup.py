import os
import sys
from distutils.core import setup

VERSION_STRING = ( os.environ['PACKAGE_VER_MAJOR'] + '.' + os.environ['PACKAGE_VER_MINOR'] + '.' + os.environ['PACKAGE_VER_PATCH'] +
'_python' + str(sys.version_info[0]) + '.' + str(sys.version_info[1]) )


setup(name = 'cactuscore-uhal-tools',
      version = VERSION_STRING,
      description = 'uTCA HW Development Tools that depend on uHAL',
      author = 'Marc Magrans de Arbil',
      author_email = 'marc@cern.ch',
      url = 'http://cactus.web.cern.ch/cactus',
      packages = ['uhal.tools'],
      scripts = ['ipbus_addr_map']
      )
