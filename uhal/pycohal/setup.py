
import os, sys
from distutils.core import setup
from os.path import join

CACTUS_ROOT    = os.environ['CACTUS_ROOT']
INSTALL_PREFIX = os.environ['INSTALL_PREFIX']
VERSION_STRING = (os.environ['PACKAGE_VER_MAJOR']+'.'+os.environ['PACKAGE_VER_MINOR']+'.'+os.environ['PACKAGE_VER_PATCH']+
                   '_python'+str(sys.version_info[0])+'.'+str(sys.version_info[1]) )


setup(name='cactuscore-uhal-pycohal',
      version = VERSION_STRING,
      description = 'Python bindings for the CACTUS uhal libraries.',
      author = 'Tom Williams', 
      author_email = 'T.Williams@cern.ch',
      url = 'http://cactus.web.cern.ch/cactus',

      packages = ['pycohal'], 
      package_dir = {'' : 'pkg'},
      package_data = {'pycohal':['*.so']}, # Need matching line in MANIFEST.in for python 2.4 (distutils bug)
 
      scripts=['test_pycohal'] # Need corresponding line in MANIFEST.in for python 2.4 (distutils bug)
 )

