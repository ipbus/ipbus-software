#
# Makefile for uhal tutorial 1
#

SOURCES = 
EXECUTABLES = $(patsubst src/common/%.cpp,bin/%.exe,${SOURCES})

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update

default: build

clean: _cleanall
_cleanall:
	rm -rf bin

all: _all
build: _all
buildall: _all
_all: ${EXECUTABLES}

${EXECUTABLES}: bin/%.exe : src/common/%.cpp 
	mkdir -p bin
	g++ ${LINK_FLAGS} ${LIBRARY_PATH} ${LIBRARIES} ${INCLUDE_PATH} $< -o $@
