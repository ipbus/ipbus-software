#
# Makefile for uhal 
#
BUILD_HOME = $(shell pwd)/../..

include ${BUILD_HOME}/config/Makefile.macros

Package = uhal/uhal
PackagePath = ${BUILD_HOME}/${Package}
PackageName = cactus-uhal-uhal

Packager = Andrew Rose

PACKAGE_VER_MAJOR = 0
PACKAGE_VER_MINOR = 0
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 1

LIBRARY = lib/libcactus_uhal_uhal.so

SOURCES = $(wildcard src/common/*.cpp)
OBJECT_FILES = $(patsubst src/common/%.cpp,obj/%.o,${SOURCES})

LIBRARY_PATH = 	-L${EXTERN_BOOST_LIB_PREFIX} \
		-L${EXTERN_PUGIXML_LIB_PREFIX} \
		-L${UHAL_GRAMMARS_LIB_PREFIX} \
		-L${UHAL_LOG_LIB_PREFIX} 

LIBRARIES = 	-lpthread \
		\
		-lcactus_extern_pugixml \
		\
		-lboost_thread \
		-lboost_system \
		-lboost_filesystem \
		-lboost_regex \
		\
		-lcactus_uhal_grammars \
		-lcactus_uhal_log	

INCLUDE_PATH = 	-Iinclude  \
		-I${UHAL_GRAMMARS_INCLUDE_PREFIX} \
		-I${UHAL_LOG_INCLUDE_PREFIX} \
		-I${EXTERN_BOOST_INCLUDE_PREFIX} \
		-I${EXTERN_PUGIXML_INCLUDE_PREFIX} 

CPP_FLAGS = -O3 -Wall -fPIC -D USE_TCP_MULTITHREADED -D USE_UDP_MULTITHREADED  ${INCLUDE_PATH}
LINK_FLAGS = -shared -fPIC -Wall -O3 ${LIBRARY_PATH} ${LIBRARIES}

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall

default: build

clean: _cleanall
_cleanall:
	rm -rf RPMBUILD
	rm -rf obj
	rm -rf lib

all: _all
build: _all
buildall: _all
_all: ${LIBRARY}

${LIBRARY}: ${OBJECT_FILES}
	mkdir -p RPMBUILD/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p RPMBUILD/SOURCES/{lib,include/uhal,include/uhal/TemplateDefinitions}
	mkdir -p lib
	g++ ${LINK_FLAGS} ${OBJECT_FILES} -o $@
	cp -p lib/*.so RPMBUILD/SOURCES/lib/.
	cp -p include/uhal/*.hpp RPMBUILD/SOURCES/include/uhal/.
	cp -p include/uhal/TemplateDefinitions/*.hxx RPMBUILD/SOURCES/include/uhal/TemplateDefinitions/.

${OBJECT_FILES}: obj/%.o : src/common/%.cpp 
	mkdir -p obj
	g++ ${CPP_FLAGS} -c $< -o $@



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

