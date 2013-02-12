#
# Makefile for pycohal
#

CACTUS_ROOT = $(shell pwd)/../..
export CACTUS_ROOT

include $(CACTUS_ROOT)/config/Makefile.macros
export INSTALL_PREFIX

PACKAGE_VER_MAJOR = 1
PACKAGE_VER_MINOR = 0
PACKAGE_VER_PATCH = 0
PACKAGE_RELEASE = 1

export CACTUS_ROOT INSTALL_PREFIX 
export PACKAGE_VER_MAJOR PACKAGE_VER_MINOR PACKAGE_VER_PATCH


BINDINGS_SOURCES = $(wildcard src/common/*.cpp)
BINDINGS_OBJECT_FILES = $(patsubst src/common/%.cpp,obj/%.o,${BINDINGS_SOURCES})
BINDINGS = lib/pycohal.so

LIBRARY_PATH = -L${EXTERN_BOOST_LIB_PREFIX} \
				-L${EXTERN_PUGIXML_LIB_PREFIX} \
				-L${UHAL_UHAL_LIB_PREFIX} \
				-L${UHAL_LOG_LIB_PREFIX} \
				-L${UHAL_GRAMMARS_LIB_PREFIX}


BINDING_LIBRARIES = 	\
			-lpthread \
			-ldl \
			-lutil \
			\
			-lboost_filesystem \
			-lboost_python \
			-lboost_regex \
			-lboost_system \
			-lboost_thread \
			\
			-lcactus_extern_pugixml \
			-lcactus_uhal_grammars \
			-lcactus_uhal_log \
			-lcactus_uhal_uhal


INCLUDE_PATH = 	-Iinclude  \
				-I${EXTERN_BOOST_INCLUDE_PREFIX} \
				-I${PYTHON_INCLUDE_PREFIX} \
				-I${EXTERN_PUGIXML_INCLUDE_PREFIX}  \
				-I${UHAL_UHAL_INCLUDE_PREFIX}  \
				-I${UHAL_LOG_INCLUDE_PREFIX}  \
				-I${UHAL_GRAMMARS_INCLUDE_PREFIX} 

CPP_FLAGS = -g -Wall -MMD -MP -fPIC ${INCLUDE_PATH}

LINK_BINDINGS_FLAGS = -shared -fPIC -Wall -g -Wl,-h -Wl,-Bstatic -Wl,-Bdynamic ${LIBRARY_PATH} ${BINDING_LIBRARIES}

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update generator

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	rm -rf obj
	rm -rf lib
	rm -rf build dist MANIFEST

all: _all
build: _all
buildall: _all
_all: ${BINDINGS}	

#Library will be compiled and will be packaged
${BINDINGS}: ${BINDINGS_OBJECT_FILES}
	mkdir -p lib
	g++ ${LINK_BINDINGS_FLAGS} ${BINDINGS_OBJECT_FILES} -o $@

${BINDINGS_OBJECT_FILES}: obj/%.o : src/common/%.cpp
	mkdir -p obj
	g++ -c ${CPP_FLAGS} -ftemplate-depth-128 -O3 -finline-functions -Wno-inline -DNDEBUG  $< -o $@	

-include $(BINDINGS_SOURCES:.cpp=.d)	

rpm: _rpmall
_rpmall:
	rm -rf /var/tmp/cactus*
	python setup.py bdist_rpm --release ${PACKAGE_RELEASE} --binary-only

