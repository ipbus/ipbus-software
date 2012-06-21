#
# Makefile for boost
#

include $(shell pwd)/../../config/Makefile.macros

Package = uhal/grammars
PackagePath = $(CACTUS_BUILD_ROOT)/${Package}
PackageName = cactus-uhal-grammars

Packager = Andrew Rose

PACKAGE_VER_MAJOR = 0
PACKAGE_VER_MINOR = 0
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 1

LIBRARY = lib/libcactus_uhal_grammars.so

SOURCES = $(wildcard src/common/*.cpp)
OBJECT_FILES = $(patsubst src/common/%.cpp,obj/%.o,${SOURCES})

LIBRARY_PATH = 	

LIBRARIES =  -L${UHAL_LOG_LIB_PREFIX} -lcactus_uhal_log

INCLUDE_PATH = 	-Iinclude  \
		-I${EXTERN_BOOST_INCLUDE_PREFIX} \
		-I${UHAL_LOG_INCLUDE_PREFIX}


CPP_FLAGS = -O3 -Wall -fPIC
LINK_FLAGS = -shared -fPIC -Wall -O3

RPMBUILD_DIR = ${PackagePath}/RPMBUILD

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	rm -rf obj
	rm -rf lib

all: _all
build: _all
buildall: _all
_all: ${LIBRARY}

${LIBRARY}: ${OBJECT_FILES}
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p ${RPMBUILD_DIR}/SOURCES/{lib,include/uhal/grammars}
	mkdir -p lib
	g++ ${LINK_FLAGS} ${LIBRARY_PATH} ${LIBRARIES} ${OBJECT_FILES} -o $@
	cp -p lib/*.so ${RPMBUILD_DIR}/SOURCES/lib/.
	cp -p include/uhal/grammars/*.hpp ${RPMBUILD_DIR}/SOURCES/include/uhal/grammars/.

${OBJECT_FILES}: obj/%.o : src/common/%.cpp 
	mkdir -p obj
	g++ ${INCLUDE_PATH} -c $< ${CPP_FLAGS} -o $@



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
			cactus-uhal-grammars.spec

