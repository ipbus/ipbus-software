BUILD_HOME = $(shell pwd)/../../..

include $(BUILD_HOME)/config/Makefile.macros

Project = cactuscore
Package = uhal/grammars
PackagePath = $(CACTUS_RPM_ROOT)/$(Project)/${Package}
PackageName = cactuscore-uhal-grammars

Packager = Andrew Rose

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 4
PACKAGE_VER_PATCH = 0
PACKAGE_RELEASE = 0

LIBRARY = lib/libcactus_uhal_grammars.so

SOURCES = $(wildcard src/common/*.cpp)
OBJECT_FILES = $(patsubst src/common/%.cpp,obj/%.o,${SOURCES})

LIBRARY_PATH = 	

LIBRARIES =  -L${UHAL_LOG_LIB_PREFIX} -lcactus_uhal_log

INCLUDE_PATH = 	-Iinclude  \
		-I${EXTERN_BOOST_INCLUDE_PREFIX} \
		-I${UHAL_LOG_INCLUDE_PREFIX}


CPP_FLAGS = -g -O3 -Wall -MMD -MP -fPIC \
            #-fprofile-arcs -ftest-coverage

LINK_FLAGS = -g -shared -fPIC -Wall -O3\
	    #-lgcov -coverage

# Hide c++11-extensions warning when building on osx
ifeq ($(CACTUS_OS),osx)
CPP_FLAGS += -Wno-c++11-extensions
endif


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
	g++ ${LINK_FLAGS} ${LIBRARY_PATH} ${LIBRARIES} ${OBJECT_FILES} -o $@

${OBJECT_FILES}: obj/%.o : src/common/%.cpp ${INCLUDES} 
	mkdir -p {lib,obj}
	g++ ${INCLUDE_PATH} -c $< ${CPP_FLAGS} -o $@

-include $(OBJECT_FILES:.o=.d)

rpm: _rpmall
_rpmall: 
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p ${RPMBUILD_DIR}/SOURCES/{lib,include/${Package}}
	cp -p lib/*.so ${RPMBUILD_DIR}/SOURCES/lib/.
	cp -p include/${Package}/*.hpp ${RPMBUILD_DIR}/SOURCES/include/${Package}/.

	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD						\
			--define  "_topdir ${RPMBUILD_DIR}"						\
			--define "_prefix ${CACTUS_ROOT}"							\
			--define "sources_dir ${RPMBUILD_DIR}/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}"	\
			--define "release ${PACKAGE_RELEASE}.${CACTUS_OS}"							\
			--define "packager ${Packager}"								\
			${PackageName}.spec

