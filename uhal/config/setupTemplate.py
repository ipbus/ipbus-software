
import os, sys
from distutils.core import setup
from os import walk
from os.path import join, relpath

_rpmVersion='__version__'
_name='__packagename__'
_author='__author__'
_author_email=''
_description='__description__'
_url='__url__'
_packages=__python_packages__
_project='__project__'
_install_dir='__install_dir__'
_package_build_dir='__package_build_dir__'


# Find standard data files: scripts
scripts_base_dir = join(_package_build_dir, 'scripts')
scripts_map = dict((path, [join(path, f) for f in files]) for (path, _, files) in walk(scripts_base_dir) if len(files) > 0)
scripts_map = dict((join(_install_dir, 'bin', _project, relpath(path, scripts_base_dir)), scripts_map[path]) for path in scripts_map)

etc_base_dir = join(_package_build_dir, 'etc')
etc_map = dict((path, [join(path, f) for f in files]) for (path, _, files) in walk(etc_base_dir) if len(files) > 0)
etc_map = dict((join(_install_dir, 'etc', relpath(path, etc_base_dir)), etc_map[path]) for path in etc_map)

data_files = [(k, scripts_map[k]) for k in scripts_map]
data_files += [(k, etc_map[k]) for k in etc_map]


setup(name=_name,
      version = _rpmVersion,
      description = _description,
      author = _author,
      author_email = _author_email,
      url = _url,
      license = 'GPLv3',
      data_files = data_files,

      packages = _packages,
      package_dir = {'' : '.'},
      zip_safe=False,
      package_data = dict((pkg,['*.so']) for pkg in _packages)
 )

