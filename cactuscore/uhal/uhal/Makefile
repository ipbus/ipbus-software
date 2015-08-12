BUILD_HOME = $(shell pwd)/../../..

include $(BUILD_HOME)/config/Makefile.macros

Project = cactuscore
Package = uhal/uhal
PackagePath = $(CACTUS_RPM_ROOT)/$(Project)/${Package}
PackageName = cactuscore-uhal-uhal

Packager = Andrew Rose

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 4
PACKAGE_VER_PATCH = 2
PACKAGE_RELEASE = 0

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

CPP_FLAGS = -g -O3 -Wall -MMD -MP -fPIC\
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
	g++ ${INCLUDE_PATH} ${CPP_FLAGS} -c $< -o $@

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

