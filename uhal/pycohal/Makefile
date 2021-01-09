BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal
Package = uhal/pycohal
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-pycohal

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 7
PACKAGE_VER_PATCH = 9
PACKAGE_RELEASE = 1${PACKAGE_RELEASE_SUFFIX}

PackageDescription = Python bindings for the uHAL library
PackageURL = https://ipbus.web.cern.ch/ipbus
Packager = Tom Williams
PackagerEmail = tom.williams@cern.ch


PythonModules = ["uhal"]
LibraryFile = pkg/uhal/_core.so


IncludePaths = include  \
		pybind11/include \
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
		$(BOOST_THREAD_LIB) \
		\
		pthread \
		dl \
		util


CXXFLAGS += -ftemplate-depth-128 -O0 -rdynamic -finline-functions -Wno-inline -DNDEBUG

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
