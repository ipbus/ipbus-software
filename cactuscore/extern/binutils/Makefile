BUILD_HOME = $(shell pwd)/../../..

include $(BUILD_HOME)/config/Makefile.macros

TARBALL_NAME = binutils-2.23
TARBALL_FILE = ${TARBALL_NAME}.tar.gz

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall

default: build

clean: _cleanall
_cleanall:
	rm -rf ${TARBALL_NAME}

all: _all
build: _all
buildall: _all
_all:
	if [ ! -f  ${TARBALL_NAME}/.build_done ]; then \
		echo "Extracting: ${TARBALL_FILE}";												 \
		source $(BUILD_HOME)/config/progress.sh 'tar -tf ${TARBALL_FILE} | wc -l' 'tar -xvzf ${TARBALL_FILE}';                         \
		cd ${TARBALL_NAME};													\
		CFLAGS="-fPIC -g -O3" CXXFLAGS="-fPIC -g -O3" ./configure;\
		make; \
 		date > .build_done;										\
	fi;	


