BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

PYTHON_VERSION_LESS_THAN_36=$(shell ${PYTHON} -c "import sys; print('1' if (sys.version_info.major, sys.version_info.minor) < (3,6) else '0')")
PYTHON_VERSION_MAJOR_MINOR=$(shell ${PYTHON} -c "import sys; print(str(sys.version_info.major)+str(sys.version_info.minor))")
ifeq (${PYTHON_VERSION_LESS_THAN_36},1)
  EXTERN_PYBIND11_INCLUDE_PREFIX=pybind11-python2/include
else
  EXTERN_PYBIND11_INCLUDE_PREFIX=pybind11/include
endif


Project = uhal
Package = uhal/python
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-python${PYTHON_VERSION_MAJOR_MINOR}

PackageDescription = Python bindings for the uHAL library
PackageURL = https://ipbus.web.cern.ch/ipbus
Packager = Tom Williams
PackagerEmail = tom.williams@cern.ch


PythonModules = ["uhal"]
LibraryFile = pkg/uhal/_core.so


IncludePaths = include  \
		${EXTERN_PYBIND11_INCLUDE_PREFIX} \
		${EXTERN_BOOST_INCLUDE_PREFIX} \
		${EXTERN_PUGIXML_INCLUDE_PREFIX} \
		${PYTHON_INCLUDE_PREFIX} \
		${BUILD_HOME}/uhal/log/include  \
		${BUILD_HOME}/uhal/grammars/include \
		${BUILD_HOME}/uhal/uhal/include \
		${BUILD_HOME}/uhal/tests/include

LibraryPaths = \
		${EXTERN_BOOST_LIB_PREFIX} \
		${EXTERN_PUGIXML_LIB_PREFIX} \
		${PYTHON_LIB_PREFIX} \
		${BUILD_HOME}/uhal/log/lib \
		${BUILD_HOME}/uhal/grammars/lib \
		${BUILD_HOME}/uhal/uhal/lib \
		${BUILD_HOME}/uhal/tests/lib

Libraries = \
		cactus_uhal_tests \
		cactus_uhal_uhal \
		cactus_uhal_log \
		cactus_uhal_grammars \
		pugixml \
		\
		boost_filesystem \
		boost_regex \
		boost_system \
		\
		pthread \
		dl \
		util


CXXFLAGS += -ftemplate-depth-128 -O0 -rdynamic -finline-functions -Wno-inline -DNDEBUG $(shell ${PYTHON}-config --includes)
LDFLAGS += $(shell ${PYTHON}-config --libs)

ifeq (${CACTUS_OS},osx)
  CXXFLAGS += -Wno-c++11-extensions
  CXXFLAGS += $(shell python-config --cflags)
  LDFLAGS += $(shell python-config --ldflags)
else
  LDFLAGS += -Wl,-h -Wl,-Bstatic -Wl,-Bdynamic
endif

ifndef BUILD_STATIC
  CXXFLAGS += -DBOOST_TEST_DYN_LINK
  IncludePaths += ${BUILD_HOME}/uhal/uhal/include/_static
endif


include $(BUILD_HOME)/uhal/config/mfRules.mk
include $(BUILD_HOME)/uhal/config/mfPythonRPMRules.mk
