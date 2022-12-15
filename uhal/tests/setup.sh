#!/bin/bash

#HERE=$(readlink -f $(dirname $BASH_SOURCE))
HERE=$(${PYTHON:-python} -c "import os.path; print(os.path.dirname(os.path.abspath('$BASH_SOURCE')))")

REPO_BASE_DIR=$HERE/../..
UHAL_BASE_DIR=$HERE/..

export PATH=$UHAL_BASE_DIR/tests/bin:$PATH
export PATH=$UHAL_BASE_DIR/tests/src/python:$PATH
export PATH=$UHAL_BASE_DIR/tests/scripts:$PATH
export PATH=$UHAL_BASE_DIR/tools/scripts:$PATH
export PATH=$REPO_BASE_DIR/extern/erlang/RPMBUILD/SOURCES/bin:$PATH
export PATH=$REPO_BASE_DIR/controlhub/scripts:$PATH

export LD_LIBRARY_PATH=$REPO_BASE_DIR/extern/boost/RPMBUILD/SOURCES/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$REPO_BASE_DIR/extern/pugixml/RPMBUILD/SOURCES/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$UHAL_BASE_DIR/log/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$UHAL_BASE_DIR/grammars/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$UHAL_BASE_DIR/uhal/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$UHAL_BASE_DIR/tests/lib:$LD_LIBRARY_PATH

UNAME=$(uname -s)
if [ "$UNAME" == "Darwin" ]; then 
  export DYLD_LIBRARY_PATH=$REPO_BASE_DIR/extern/boost/RPMBUILD/SOURCES/lib:$LD_LIBRARY_PATH
  export DYLD_LIBRARY_PATH=$REPO_BASE_DIR/extern/pugixml/RPMBUILD/SOURCES/lib:$LD_LIBRARY_PATH
  export DYLD_LIBRARY_PATH=$UHAL_BASE_DIR/log/lib:$LD_LIBRARY_PATH
  export DYLD_LIBRARY_PATH=$UHAL_BASE_DIR/grammars/lib:$LD_LIBRARY_PATH
  export DYLD_LIBRARY_PATH=$UHAL_BASE_DIR/uhal/lib:$LD_LIBRARY_PATH
  export DYLD_LIBRARY_PATH=$UHAL_BASE_DIR/tests/lib:$LD_LIBRARY_PATH
fi

export PYTHONPATH=$UHAL_BASE_DIR/python/pkg:$PYTHONPATH
#export PYTHONPATH=$UHAL_BASE_DIR/gui:$PYTHONPATH

export UHAL_ENABLE_IPBUS_MMAP=

