# 
# Makefile for uhal 
#

CACTUS_ROOT = $(shell pwd)/../..

include $(CACTUS_ROOT)/config/Makefile.macros

Package = uhal/uhal
PackagePath = $(CACTUS_BUILD_ROOT)/${Package}
PackageName = cactuscore-uhal-uhal

Packager = Andrew Rose

PACKAGE_VER_MAJOR = 1
PACKAGE_VER_MINOR = 0
PACKAGE_VER_PATCH = 6
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
		-lboost_thread \
		\
		-lcactus_uhal_grammars \
		-lcactus_uhal_log	

INCLUDE_PATH = 	-Iinclude  \
		-I${UHAL_GRAMMARS_INCLUDE_PREFIX} \
		-I${UHAL_LOG_INCLUDE_PREFIX} \
		-I${EXTERN_BOOST_INCLUDE_PREFIX} \
		-I${EXTERN_PUGIXML_INCLUDE_PREFIX} 

CPP_FLAGS = -g -O3 -Wall -MMD -MP -fPIC ${INCLUDE_PATH}
#-D USE_TCP_MULTITHREADED -D USE_UDP_MULTITHREADED  
#-D THROW_ON_ADDRESS_SPACE_OVERLAP 

LINK_FLAGS = -g -shared -fPIC -Wall -O3 ${LIBRARY_PATH} ${LIBRARIES}

RPMBUILD_DIR = ${PackagePath}/RPMBUILD

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	rm -rf obj
	rm -rf lib

all: _all
build: _all
buildall: _all
_all: make_dirs ${LIBRARY} copy_to_build

make_dirs:
	mkdir -p {lib,obj}
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p ${RPMBUILD_DIR}/SOURCES/{lib,include/uhal,include/uhal/TemplateDefinitions}

${LIBRARY}: ${OBJECT_FILES}
	g++ ${LINK_FLAGS} ${OBJECT_FILES} -o $@

${OBJECT_FILES}: obj/%.o : src/common/%.cpp 
	g++ ${CPP_FLAGS} -c $< -o $@

-include $(OBJECT_FILES:.o=.d)

copy_to_build:
	cp -p lib/*.so ${RPMBUILD_DIR}/SOURCES/lib/.
	cp -p include/uhal/*.hpp ${RPMBUILD_DIR}/SOURCES/include/uhal/.
	cp -p include/uhal/TemplateDefinitions/*.hxx ${RPMBUILD_DIR}/SOURCES/include/uhal/TemplateDefinitions/.



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
			cactuscore-uhal-uhal.spec

