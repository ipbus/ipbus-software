BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros

Project = uhal
Package = uhal/uhal
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-uhal

Packager = Andrew Rose, Tom Williams

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 5
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 1

LIBRARY = cactus_uhal_uhal


INCLUDE_PATH = include  \
		${UHAL_GRAMMARS_INCLUDE_PREFIX} \
		${UHAL_LOG_INCLUDE_PREFIX} \
		${EXTERN_BOOST_INCLUDE_PREFIX} \
		${EXTERN_PUGIXML_INCLUDE_PREFIX} 

LIBRARY_PATH = ${EXTERN_BOOST_LIB_PREFIX} \
		${EXTERN_PUGIXML_LIB_PREFIX} \
		${UHAL_GRAMMARS_LIB_PREFIX} \
		${UHAL_LOG_LIB_PREFIX}

LIBRARIES = pthread \
		\
		${PUGIXML_LIB_NAME} \
		\
		boost_thread \
		boost_system \
		boost_filesystem \
		boost_regex \
		boost_thread \
		\
		cactus_uhal_grammars \
		cactus_uhal_log	




####################################
###   COMMON DERIVED VARIABLES   ###


LIBRARY := lib/lib${LIBRARY}.so

LIBRARY_SOURCES = $(wildcard src/common/*.cpp)
LIBRARY_OBJECT_FILES = $(patsubst src/common/%.cpp,obj/%.o,${LIBRARY_SOURCES})

EXECUTABLE_SOURCES = $(wildcard src/common/*.cxx)
EXECUTABLE_OBJECT_FILES = $(patsubst src/common/%.cxx,obj/%.o,${EXECUTABLE_SOURCES})
EXECUTABLES = $(patsubst src/common/%.cxx,bin/%.exe,${EXECUTABLE_SOURCES})

INCLUDE_PATH := $(addprefix -I,$(INCLUDE_PATH))
LIBRARY_PATH := $(addprefix -L,$(LIBRARY_PATH))
LIBRARIES := $(addprefix -l,$(LIBRARIES))

CPP_FLAGS = -g -Wall -pedantic -O3 -MMD -MP -fPIC \
#						-fprofile-arcs -ftest-coverage
LINK_LIBRARY_FLAGS = -shared -fPIC -Wall -g -O3 \
#										 -lgcov -coverage
LINK_EXECUTABLE_FLAGS = -Wall -g -O3 \
#										 -lgcov -coverage

RPMBUILD_DIR = ${PackagePath}/RPMBUILD


#######################################
###   PACKAGE-SPECIFIC EXTENSIONS   ###

CPP_FLAGS += -DDISABLE_PACKET_COUNTER_HACK\
		-DRUN_ASIO_MULTITHREADED # \
#					 -DNO_PREEMPTIVE_DISPATCH 

#CPP_FLAGS += -fdelayed-template-parsing 

#-D THROW_ON_ADDRESS_SPACE_OVERLAP 

# NOTE: Adding this flag is a dirty hack to make uhal/uhal compile on 
# OS X. The fact that this is necessary really means the code is 
# fishy. 
ifeq ($(CACTUS_OS),osx) 
CPP_FLAGS += -Wno-c++11-extensions -fdelayed-template-parsing
endif 


########################
###   COMMON RULES   ###


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
_all: ${LIBRARY}


${LIBRARY}: ${OBJECT_FILES}
	g++ ${LINK_LIBRARY_FLAGS} ${LIBRARY_PATH} ${LIBRARIES} ${OBJECT_FILES} -o $@

${OBJECT_FILES}: obj/%.o : src/common/%.cpp 
	mkdir -p {lib,obj}
	g++ ${CPP_FLAGS} ${INCLUDE_PATH} -c $< -o $@

-include $(OBJECT_FILES:.o=.d)

rpm: _rpmall
_rpmall: 
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p ${RPMBUILD_DIR}/SOURCES/{lib,include/uhal,include/uhal/TemplateDefinitions}
	cp -p lib/*.so ${RPMBUILD_DIR}/SOURCES/lib/.
	cp -p include/uhal/*.hpp ${RPMBUILD_DIR}/SOURCES/include/uhal/.
	cp -p include/uhal/TemplateDefinitions/*.hxx ${RPMBUILD_DIR}/SOURCES/include/uhal/TemplateDefinitions/.
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD						\
			--define  "_topdir ${RPMBUILD_DIR}"						\
			--define "_prefix ${CACTUS_ROOT}"							\
			--define "sources_dir ${RPMBUILD_DIR}/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}"	\
			--define "release ${PACKAGE_RELEASE}.${CACTUS_OS}"							\
			--define "packager ${Packager}"								\
			${PackageName}.spec

