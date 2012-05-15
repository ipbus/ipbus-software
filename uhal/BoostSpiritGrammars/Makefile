#
# Makefile for boost
#
BUILD_HOME = $(shell pwd)/../..

include ${BUILD_HOME}/config/Makefile.macros

Package = uhal/BoostSpiritGrammars
PackagePath = ${BUILD_HOME}/${Package}
PackageName = cactus-extern-boostspiritGramars

Packager = Andrew Rose, Marc Magrans de Abril

PACKAGE_VER_MAJOR = 0
PACKAGE_VER_MINOR = 0
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 1

LIBRARY = lib/libuhal_spirit_grammars.so

SOURCES = $(wildcard src/common/*.cpp)
OBJECT_FILES = $(patsubst src/common/%.cpp,obj/%.o,${SOURCES})

LIBRARY_PATH = 	

LIBRARIES =  

INCLUDE_PATH = 	-Iinclude  \
		-I${EXTERN_BOOST_INCLUDE_PREFIX} 

CPP_FLAGS = -O3 -Wall -fPIC
LINK_FLAGS = -shared -fPIC -Wall -O3

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update

default: build

clean: _cleanall
_cleanall:
	rm -rf RPMBUILD
	rm -rf obj

all: _all
build: _all
buildall: _all
_all: ${LIBRARY}

${LIBRARY}: ${OBJECT_FILES}
	mkdir -p RPMBUILD/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p RPMBUILD/SOURCES/{lib,include/BoostSpiritGrammars}
	mkdir -p lib
	g++ ${LINK_FLAGS} ${LIBRARY_PATH} ${LIBRARIES} ${OBJECT_FILES} -o $@
	cp -p lib/*.so RPMBUILD/SOURCES/lib/.
	cp -p include/BoostSpiritGrammars/*.hpp RPMBUILD/SOURCES/include/BoostSpiritGrammars/.

${OBJECT_FILES}: obj/%.o : src/common/%.cpp 
	mkdir -p obj
	g++ ${INCLUDE_PATH} -c $< ${CPP_FLAGS} -o $@



rpm: _rpmall
_rpmall: 
	mkdir -p RPMBUILD/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	rpmbuild -bb -bl --buildroot=${PackagePath}/RPMBUILD/BUILD						\
			--define  "_topdir ${PackagePath}/RPMBUILD"						\
			--define "_prefix ${INSTALL_PREFIX}"							\
			--define "sources_dir ${PackagePath}/RPMBUILD/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}"	\
			--define "release ${PACKAGE_RELEASE}"							\
			--define "packager ${Packager}"								\
			cactus-uhal-spirit-grammars.spec

