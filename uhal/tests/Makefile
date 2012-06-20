#
# Makefile for uhal tests
#
BUILD_HOME = $(shell pwd)/../..

include ${BUILD_HOME}/config/Makefile.macros

Package = uhal/tests
PackagePath = ${BUILD_HOME}/${Package}
PackageName = cactus-uhal-tests

Packager = Andrew Rose

PACKAGE_VER_MAJOR = 0
PACKAGE_VER_MINOR = 0
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 1

SOURCES = src/common/test_uhal.cpp
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

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update

default: build

clean: _cleanall
_cleanall:
	rm -rf RPMBUILD
	rm -rf obj
	rm -rf bin

all: _all
build: _all
buildall: _all
_all: ${EXECUTABLES}

${EXECUTABLES}: ${EXECUTABLE_OBJECT_FILES} 
	mkdir -p RPMBUILD/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p RPMBUILD/SOURCES/bin
	mkdir -p bin
	g++ ${LINK_FLAGS} $< -o $@
	cp -p bin/*.exe RPMBUILD/SOURCES/bin/.

${EXECUTABLE_OBJECT_FILES}: obj/%.o : src/common/%.cpp
	mkdir -p obj
	g++ -c ${CPP_FLAGS}  $< -o $@

rpm: _rpmall
_rpmall: 
	rpmbuild -bb -bl --buildroot=${PackagePath}/RPMBUILD/BUILD						\
			--define  "_topdir ${PackagePath}/RPMBUILD"						\
			--define "_prefix ${INSTALL_PREFIX}"							\
			--define "sources_dir ${PackagePath}/RPMBUILD/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}"	\
			--define "release ${PACKAGE_RELEASE}"							\
			--define "packager ${Packager}"								\
			cactus-uhal-uhal.spec

