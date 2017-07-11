
import os, sys
from distutils.core import setup
from os.path import join

PYCOHAL_VERSION = (os.environ['PACKAGE_VER_MAJOR']+'.'+os.environ['PACKAGE_VER_MINOR']+'.'+os.environ['PACKAGE_VER_PATCH'])

setup(name='cactuscore-uhal-pycohal',
      version = PYCOHAL_VERSION,
      description = 'Python bindings for the CACTUS uhal libraries.',
      author = 'Tom Williams', 
      author_email = 'T.Williams@cern.ch',
      url = 'http://cactus.web.cern.ch/cactus',

      packages = ['uhal'], 
      package_dir = {'' : 'pkg'},
      package_data = {'uhal' : ['*.so']}
 
 )

