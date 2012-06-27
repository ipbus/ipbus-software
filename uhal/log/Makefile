#
# Makefile for boost
#

CACTUS_ROOT = $(shell pwd)/../..

include $(CACTUS_ROOT)/config/Makefile.macros

Package = uhal/log
PackagePath = $(CACTUS_BUILD_ROOT)/${Package}
PackageName = cactus-uhal-log

Packager = Andrew Rose

PACKAGE_VER_MAJOR = 0
PACKAGE_VER_MINOR = 0
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 1

LIBRARY = lib/libcactus_uhal_log.so
LIBRARY_SOURCES = $(wildcard src/common/*.cpp)
LIBRARY_OBJECT_FILES = $(patsubst src/common/%.cpp,obj/%.o,${LIBRARY_SOURCES})

EXECUTABLE_SOURCES = 	src/common/generator.cxx

EXECUTABLE_OBJECT_FILES = $(patsubst src/common/%.cxx,obj/%.o,${EXECUTABLE_SOURCES})
EXECUTABLES = $(patsubst src/common/%.cxx,bin/%.exe,${EXECUTABLE_SOURCES})

LIBRARY_PATH = 	

LIBRARIES =  

INCLUDE_PATH = 	-Iinclude  \
		-I${EXTERN_BOOST_INCLUDE_PREFIX} 

CPP_FLAGS = -g -Wall -fPIC -D MAX_NUM_ARGS=32 ${INCLUDE_PATH}

LINK_LIBRARY_FLAGS = -shared -fPIC -Wall -g ${LIBRARY_PATH} ${LIBRARIES}

LINK_EXECUTABLE_FLAGS = -Wall -g ${LIBRARY_PATH} ${LIBRARIES}

RPMBUILD_DIR = ${PackagePath}/RPMBUILD

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update generator

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	rm -rf obj
	rm -rf bin
	rm -rf lib

all: _all
build: _all
buildall: _all
_all: ${EXECUTABLES} ${LIBRARY} 

#Executables will be compiled but they won't be packaged
${EXECUTABLES}: bin/%.exe: obj/%.o ${EXECUTABLE_OBJECT_FILES}
	mkdir -p bin
	g++ ${LINK_EXECUTABLE_FLAGS} $< -o $@
	./bin/generator.exe	
	
${EXECUTABLE_OBJECT_FILES}: obj/%.o : src/common/%.cxx
	mkdir -p obj
	g++ -c ${CPP_FLAGS}  $< -o $@
	
#Library will be compiled and will be packaged
${LIBRARY}: ${LIBRARY_OBJECT_FILES}
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p ${RPMBUILD_DIR}/SOURCES/{lib,include/uhal/log}
	mkdir -p lib
	g++ ${LINK_LIBRARY_FLAGS} ${LIBRARY_OBJECT_FILES} -o $@
	cp -p lib/*.so ${RPMBUILD_DIR}/SOURCES/lib/.
	cp -p include/uhal/log/*.{hpp,hxx} ${RPMBUILD_DIR}/SOURCES/include/uhal/log/.

${LIBRARY_OBJECT_FILES}: obj/%.o : src/common/%.cpp 
	mkdir -p obj
	g++ -c ${CPP_FLAGS} $< -o $@

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
			cactus-uhal-log.spec

