#
# Makefile for boost
#

CACTUS_ROOT = $(shell pwd)/../..

include $(CACTUS_ROOT)/config/Makefile.macros

Package = uhal/log
PackagePath = $(CACTUS_BUILD_ROOT)/${Package}
PackageName = cactus-uhal-log

Packager = Andrew Rose

PACKAGE_VER_MAJOR = 1
PACKAGE_VER_MINOR = 0
PACKAGE_VER_PATCH = 0
PACKAGE_RELEASE = 1

INCLUDES = $(wildcard include/uhal/log/*.hpp)
LIBRARY = lib/libcactus_uhal_log.so

FRONTEND_SOURCES = $(wildcard src/common/log_inserters.*.cpp)

#BACKEND_SOURCES = $(wildcard src/common/log_backend.files.cpp)
BACKEND_SOURCES = $(wildcard src/common/log_backend.colour_terminal.cpp)

LIBRARY_OBJECT_FILES = $(patsubst src/common/%.cpp,obj/%.o,${FRONTEND_SOURCES} ${BACKEND_SOURCES})

EXECUTABLE_SOURCES = 	src/common/generator.cxx

EXECUTABLE_OBJECT_FILES = $(patsubst src/common/%.cxx,obj/%.o,${EXECUTABLE_SOURCES})
EXECUTABLES = $(patsubst src/common/%.cxx,bin/%.exe,${EXECUTABLE_SOURCES})

LIBRARY_PATH = 	-L${EXTERN_BOOST_LIB_PREFIX}

LIBRARIES =  	-lpthread \
		-lboost_thread 


INCLUDE_PATH = 	-Iinclude  \
		-I${EXTERN_BOOST_INCLUDE_PREFIX} 

CPP_FLAGS = -g -Wall -fPIC -D MAX_NUM_ARGS=32 ${INCLUDE_PATH}

LINK_LIBRARY_FLAGS = -shared -fPIC -Wall -g ${LIBRARY_PATH} ${LIBRARIES}

LINK_EXECUTABLE_FLAGS = -Wall -g 

RPMBUILD_DIR = ${PackagePath}/RPMBUILD

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update generator

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	rm -rf obj
	rm -rf bin
	rm -rf lib
#These files are created by the generator which runs as part of make and so should be removed by make clean
	rm -rf src/common/log.cpp
	rm -rf include/uhal/log/log.{hpp,hxx}

all: _all
build: _all
buildall: _all
_all: make_dirs ${EXECUTABLES} ${LIBRARY} copy_to_build

make_dirs:
	mkdir -p {bin,obj,lib}
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p ${RPMBUILD_DIR}/SOURCES/{lib,include/uhal/log}

${EXECUTABLES}: bin/%.exe: obj/%.o ${EXECUTABLE_OBJECT_FILES} 
	g++ ${LINK_EXECUTABLE_FLAGS} $< -o $@
	./bin/generator.exe	
	rm -rf obj/log.o

${EXECUTABLE_OBJECT_FILES}: obj/%.o : src/common/%.cxx
	g++ -c ${CPP_FLAGS}  $< -o $@

${LIBRARY}: ${LIBRARY_OBJECT_FILES}
#Make evaluates the list of files before it starts and does not pick up the existence of the file created during the ${EXECUTABLES} stage. We must, therefore, explicitly specify that it is built.
	g++ -c ${CPP_FLAGS} src/common/log.cpp -o obj/log.o
#And similarly, we must manually specify the inclusion of the object file created in the line above into the shared object file
	g++ ${LINK_LIBRARY_FLAGS} ${LIBRARY_OBJECT_FILES} obj/log.o -o $@

${LIBRARY_OBJECT_FILES}: obj/%.o : src/common/%.cpp ${INCLUDES}  
	g++ -c ${CPP_FLAGS} $< -o $@

copy_to_build:
	cp -p lib/*.so ${RPMBUILD_DIR}/SOURCES/lib/.
	cp -p include/uhal/log/*.{hpp,hxx} ${RPMBUILD_DIR}/SOURCES/include/uhal/log/.

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

