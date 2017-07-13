BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros

Project = uhal
Package = uhal/uhal
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-uhal

Packager = Andrew Rose

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 5
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 1

LIBRARY = lib/libcactus_uhal_uhal.so

SOURCES = $(wildcard src/common/*.cpp)
OBJECT_FILES = $(patsubst src/common/%.cpp,obj/%.o,${SOURCES})

LIBRARY_PATH = ${EXTERN_BOOST_LIB_PREFIX} \
		${EXTERN_PUGIXML_LIB_PREFIX} \
		${UHAL_GRAMMARS_LIB_PREFIX} \
		${UHAL_LOG_LIB_PREFIX}
LIBRARY_PATH := $(addprefix -L,$(LIBRARY_PATH))

LIBRARIES = 	-lpthread \
		\
		-l${PUGIXML_LIB_NAME} \
		\
		-lboost_thread \
		-lboost_system \
		-lboost_filesystem \
		-lboost_regex \
		-lboost_thread \
		\
		-lcactus_uhal_grammars \
		-lcactus_uhal_log	

INCLUDE_PATH = include  \
		${UHAL_GRAMMARS_INCLUDE_PREFIX} \
		${UHAL_LOG_INCLUDE_PREFIX} \
		${EXTERN_BOOST_INCLUDE_PREFIX} \
		${EXTERN_PUGIXML_INCLUDE_PREFIX} 
INCLUDE_PATH := $(addprefix -I,$(INCLUDE_PATH))

CPP_FLAGS = -g -O3 -Wall -pedantic -MMD -MP -fPIC\
					 -DDISABLE_PACKET_COUNTER_HACK\
					 -DRUN_ASIO_MULTITHREADED # \
#					 -fprofile-arcs -ftest-coverage
#					 -DNO_PREEMPTIVE_DISPATCH 

#CPP_FLAGS += -fdelayed-template-parsing 

#-D THROW_ON_ADDRESS_SPACE_OVERLAP 

# NOTE: Adding this flag is a dirty hack to make uhal/uhal compile on 
# OS X. The fact that this is necessary really means the code is 
# fishy. 
ifeq ($(CACTUS_OS),osx) 
CPP_FLAGS += -Wno-c++11-extensions -fdelayed-template-parsing
endif 

LINK_FLAGS = -g -shared -fPIC -Wall -O3 ${LIBRARY_PATH} ${LIBRARIES}\
#										 -lgcov -coverage

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
_all: ${LIBRARY}


${LIBRARY}: ${OBJECT_FILES}
	g++ ${LINK_FLAGS} ${OBJECT_FILES} -o $@

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

