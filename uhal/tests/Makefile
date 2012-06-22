#
# Makefile for uhal tests
#

CACTUS_ROOT = $(shell pwd)/../..

include $(CACTUS_ROOT)/config/Makefile.macros

Package = uhal/tests
PackagePath = $(CACTUS_BUILD_ROOT)/${Package}
PackageName = cactus-uhal-tests

Packager = Andrew Rose

PACKAGE_VER_MAJOR = 0
PACKAGE_VER_MINOR = 0
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 1

SOURCES = src/common/test_uhal.cpp \
			src/common/test_timing.cpp \
			src/common/DummyHardwareUdp.cpp \
			src/common/DummyHardwareTcp.cpp \
			src/common/test_log.cpp

EXECUTABLES = $(patsubst src/common/%.cpp,bin/%.exe,${SOURCES})
EXECUTABLE_OBJECT_FILES = $(patsubst src/common/%.cpp,obj/%.o,${SOURCES})

LIBRARY_PATH = 	-L${EXTERN_BOOST_LIB_PREFIX} \
		-L${EXTERN_PUGIXML_LIB_PREFIX} \
		-L${UHAL_LOG_LIB_PREFIX} \
		-L${UHAL_GRAMMARS_LIB_PREFIX} \
		-L${UHAL_UHAL_LIB_PREFIX} 

LIBRARIES = 	\
		-lpthread \
		\
		-lboost_thread \
		-lboost_filesystem \
		-lboost_regex \
		-lboost_system \
		-lboost_thread \
		\
		-lcactus_extern_pugixml \
		-lcactus_uhal_log \
		-lcactus_uhal_grammars \
		-lcactus_uhal_uhal

INCLUDE_PATH = 	-Iinclude  \
		-I${UHAL_LOG_INCLUDE_PREFIX} \
		-I${UHAL_GRAMMARS_INCLUDE_PREFIX} \
		-I${EXTERN_BOOST_INCLUDE_PREFIX} \
		-I${EXTERN_PUGIXML_INCLUDE_PREFIX} \
		-I${UHAL_UHAL_INCLUDE_PREFIX}

CPP_FLAGS = -O3 -Wall -fPIC ${INCLUDE_PATH}
LINK_FLAGS = -fPIC -Wall -O3 ${LIBRARY_PATH} ${LIBRARIES}

RPMBUILD_DIR = ${PackagePath}/RPMBUILD

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	rm -rf obj
	rm -rf bin

all: _all
build: _all
buildall: _all
_all: ${EXECUTABLES}

${EXECUTABLES}: bin/%.exe: obj/%.o 
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p ${RPMBUILD_DIR}/SOURCES/bin
	mkdir -p bin
	g++ ${LINK_FLAGS} $< -o $@
	cp -p bin/*.exe ${RPMBUILD_DIR}/SOURCES/bin/.

obj/%.o: src/common/%.cpp
	mkdir -p obj
	g++ -c ${CPP_FLAGS}  $< -o $@

rpm: _rpmall
_rpmall: 
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD						\
			--define  "_topdir ${RPMBUILD_DIR}"						\
			--define "_prefix ${INSTALL_PREFIX}"							\
			--define "sources_dir ${RPMBUILD_DIR}/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}"	\
			--define "release ${PACKAGE_RELEASE}"							\
			--define "packager ${Packager}"								\
			cactus-uhal-tests.spec

