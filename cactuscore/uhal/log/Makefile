#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
# SET TO EITHER TRUE OR FALSE, DEPENDING ON WHETHER YOU WANT TO COMPILE CMSSW INTO CODE
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
USE_BACKTRACE = FALSE
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------


BUILD_HOME = $(shell pwd)/../../..

include $(BUILD_HOME)/config/Makefile.macros

Project = cactuscore
Package = uhal/log
PackagePath = $(CACTUS_RPM_ROOT)/$(Project)/${Package}
PackageName = cactuscore-uhal-log

Packager = Andrew Rose

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 4
PACKAGE_VER_PATCH = 0
PACKAGE_RELEASE = 0

LIBRARY = lib/libcactus_uhal_log.so

LIBRARY_SOURCES = $(wildcard src/common/*.cpp)

LIBRARY_OBJECT_FILES = $(patsubst src/common/%.cpp,obj/%.o,${LIBRARY_SOURCES})

EXECUTABLE_SOURCES = 	src/common/generator.cxx

EXECUTABLE_OBJECT_FILES = $(patsubst src/common/%.cxx,obj/%.o,${EXECUTABLE_SOURCES})
EXECUTABLES = $(patsubst src/common/%.cxx,bin/%.exe,${EXECUTABLE_SOURCES})

ifeq ($(USE_BACKTRACE),TRUE)

	LIBRARY_PATH = 	-L${EXTERN_BOOST_LIB_PREFIX}\
									-L$(BUILD_HOME)/cactuscore/extern/binutils/binutils-2.23/bfd\
									-L$(BUILD_HOME)/cactuscore/extern/binutils/binutils-2.23/libiberty 
	
	LIBRARIES = -lpthread \
			-lboost_thread \
			-Wl,--whole-archive -liberty -lbfd -lz -Wl,--no-whole-archive\
	
	INCLUDE_PATH = 	-Iinclude  \
			-I${EXTERN_BOOST_INCLUDE_PREFIX} \
			-I$(BUILD_HOME)/cactuscore/extern/binutils/binutils-2.23/include/\
			-I$(BUILD_HOME)/cactuscore/extern/binutils/binutils-2.23/binutils/\
			-I$(BUILD_HOME)/cactuscore/extern/binutils/binutils-2.23/bfd/
	
	CPP_FLAGS = -g  -Wall -O3 -MMD -MP -fPIC -D MAX_NUM_ARGS=32 -DCOURTEOUS_EXCEPTIONS -DUSE_BACKTRACE \
							#-fprofile-arcs -ftest-coverage

else

	LIBRARY_PATH = 	-L${EXTERN_BOOST_LIB_PREFIX}
	
	LIBRARIES = -lpthread \
			-lboost_thread \
			-lboost_system
	
	INCLUDE_PATH = 	-Iinclude  \
			-I${EXTERN_BOOST_INCLUDE_PREFIX}
	
	CPP_FLAGS = -g  -Wall -O3 -MMD -MP -fPIC -D MAX_NUM_ARGS=32 -DCOURTEOUS_EXCEPTIONS \
                        #-fprofile-arcs -ftest-coverage

endif		


# Hide c++11-extensions warning when building on osx
ifeq ($(CACTUS_OS),osx)
CPP_FLAGS += -Wno-c++11-extensions
endif


LINK_LIBRARY_FLAGS = -shared -fPIC -Wall -g ${LIBRARY_PATH} ${LIBRARIES} \
										 #-lgcov -coverage

LINK_EXECUTABLE_FLAGS = -Wall -g\
										 #-lgcov -coverage 

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
_all: ${EXECUTABLES} ${LIBRARY} 


${EXECUTABLES}: bin/%.exe: obj/%.o ${EXECUTABLE_OBJECT_FILES} 
	g++ ${LINK_EXECUTABLE_FLAGS} $< -o $@
	./bin/generator.exe	
	rm -rf obj/log.o

${EXECUTABLE_OBJECT_FILES}: obj/%.o : src/common/%.cxx
	mkdir -p {bin,obj,lib}
	g++ -c ${CPP_FLAGS}  ${INCLUDE_PATH}  $< -o $@

-include $(EXECUTABLE_OBJECT_FILES:.o=.d)

${LIBRARY}: ${LIBRARY_OBJECT_FILES}
#Make evaluates the list of files before it starts and does not pick up the existence of the file created during the ${EXECUTABLES} stage. We must, therefore, explicitly specify that it is built.
	g++ -c ${CPP_FLAGS}  ${INCLUDE_PATH} src/common/log.cpp -o obj/log.o
#And similarly, we must manually specify the inclusion of the object file created in the line above into the shared object file
	g++ ${LINK_LIBRARY_FLAGS} ${LIBRARY_OBJECT_FILES} obj/log.o -o $@

${LIBRARY_OBJECT_FILES}: obj/%.o : src/common/%.cpp ${INCLUDES}  
	mkdir -p {bin,obj,lib}
	g++ -c ${CPP_FLAGS}  ${INCLUDE_PATH} $< -o $@

-include $(LIBRARY_OBJECT_FILES:.o=.d)

rpm: _rpmall
_rpmall: 
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p ${RPMBUILD_DIR}/SOURCES/{lib,include/${Package}}
	cp -p lib/*.so ${RPMBUILD_DIR}/SOURCES/lib/.
	cp -p include/${Package}/*.{hpp,hxx} ${RPMBUILD_DIR}/SOURCES/include/${Package}/.
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD						\
			--define  "_topdir ${RPMBUILD_DIR}"						\
			--define "_prefix ${CACTUS_ROOT}"							\
			--define "sources_dir ${RPMBUILD_DIR}/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}"	\
			--define "release ${PACKAGE_RELEASE}.${CACTUS_OS}"							\
			--define "packager ${Packager}"								\
			${PackageName}.spec

