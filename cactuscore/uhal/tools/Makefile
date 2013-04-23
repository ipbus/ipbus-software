BUILD_HOME = $(shell pwd)/../../..

include $(BUILD_HOME)/config/Makefile.macros

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 0
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 0

export CACTUS_ROOT 
export PACKAGE_VER_MAJOR PACKAGE_VER_MINOR PACKAGE_VER_PATCH

PYCHECKER=$(shell which pychecker)
SOURCES=$(shell find . -name "*.py")
PYCHECKER_RESULTS=$(patsubst %.py,pychecker/%.txt,${SOURCES})

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall 

default: build

clean: _cleanall
_cleanall:
	-rm -rf ${RPMBUILD_DIR}
	-rm -rf build dist MANIFEST
	-rm -rf pychecker

all: _all
build: _all
buildall: _all
_all: 

rpm: _rpmall
_rpmall:
	-rm -rf /var/tmp/cactus*
	python setup.py bdist_rpm --release ${PACKAGE_RELEASE} --binary-only
