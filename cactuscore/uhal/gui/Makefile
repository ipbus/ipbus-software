BUILD_HOME = $(shell pwd)/../../..

include $(BUILD_HOME)/config/Makefile.macros

PACKAGE_VER_MAJOR = 1
PACKAGE_VER_MINOR = 0
PACKAGE_VER_PATCH = 0
PACKAGE_RELEASE = 0

export CACTUS_ROOT 
export PACKAGE_VER_MAJOR PACKAGE_VER_MINOR PACKAGE_VER_PATCH



.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall 

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	rm -rf build dist MANIFEST

all: _all
build: _all
buildall: _all
_all: 

rpm: _rpmall
_rpmall:
	rm -rf /var/tmp/cactus*
	python setup.py bdist_rpm --release ${PACKAGE_RELEASE} --binary-only

install: _install
_install:
	find . -name "*.rpm" -exec sudo rpm -iv {} \;
