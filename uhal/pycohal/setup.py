
import os, sys
from distutils.core import setup, Extension
from os.path import join

CACTUS_ROOT    = os.environ['CACTUS_ROOT']
INSTALL_PREFIX = os.environ['INSTALL_PREFIX']
VERSION_STRING = (os.environ['PACKAGE_VER_MAJOR']+'.'+os.environ['PACKAGE_VER_MINOR']+'.'+os.environ['PACKAGE_VER_PATCH']+
                   '_python'+str(sys.version_info[0])+'.'+str(sys.version_info[1]) )


INCLUDE_DIRS = [join(CACTUS_ROOT,'extern/boost/RPMBUILD/SOURCES/include'),
                join(CACTUS_ROOT,'extern/pugixml/RPMBUILD/SOURCES/include'),
                join(CACTUS_ROOT,'uhal/uhal/RPMBUILD/SOURCES/include'),
                join(CACTUS_ROOT,'uhal/log/RPMBUILD/SOURCES/include'),
                join(CACTUS_ROOT,'uhal/grammars/RPMBUILD/SOURCES/include'),
                join(CACTUS_ROOT,'pycohal/include') ]
LIB_DIRS_LINKING = [join(CACTUS_ROOT,'extern/boost/RPMBUILD/SOURCES/lib'),
                    join(CACTUS_ROOT,'extern/pugixml/RPMBUILD/SOURCES/lib'),
                    join(CACTUS_ROOT,'uhal/uhal/RPMBUILD/SOURCES/lib'),
                    join(CACTUS_ROOT,'uhal/log/RPMBUILD/SOURCES/lib'),
                    join(CACTUS_ROOT,'uhal/grammars/RPMBUILD/SOURCES/lib') ]

SRC_DIR = 'src/common'
SRC_FILES = [ join(SRC_DIR, fname) for fname in os.listdir(SRC_DIR) if fname.endswith('.cpp') ]


setup(name='cactus-pycohal',
      version=VERSION_STRING,
      description='Python bindings for the CACTUS uhal libraries.',
      author='Tom Williams', author_email='T.Williams@cern.ch',
      url='http://cactus.web.cern.ch/cactus',
      ext_modules=[Extension('pycohal', SRC_FILES,
                             include_dirs = INCLUDE_DIRS, library_dirs = LIB_DIRS_LINKING, runtime_library_dirs=[join(INSTALL_PREFIX,'lib')],
                             #extra_compile_args=['-g -Wall -fPIC'], 
                             #extra_link_args=['-fPIC -Wall -g -Wl,-h -Wl,-Bstatic -Wl,-Bdynamic'], 
                             libraries=['boost_python','pthread','dl','util','cactus_uhal_uhal','cactus_uhal_log']
                             )
                  ],
      scripts=['test_pycohal']
 )

