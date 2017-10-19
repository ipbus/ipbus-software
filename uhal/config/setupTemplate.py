import os, sys
from distutils.core import setup
from os.path import join

_rpmVersion='__version__'
_name='__packagename__'
_author='__author__'
_author_email='__author_email__'
_description='__description__'
_url='__url__'
_packages=__python_packages__
_scripts=__scripts__


setup(name=_name,
      version = _rpmVersion,
      description = _description,
      author = _author,
      author_email = _author_email,
      url = _url,
      license = 'GPLv3',

      packages = _packages,
      package_dir = {'' : ''},
      package_data = dict((pkg,['*.so']) for pkg in _packages),
      scripts = _scripts
 )

