BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal/tests
Package = uhal/tests
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-tests

PackageSummary = uHAL Library Tests
PackageDescription = uHAL Library Tests
PackageURL = https://ipbus.web.cern.ch/ipbus
Packager = Andrew Rose, Marc Magrans de Arbil, Tom Williams
PackageRequires = which iproute


Library = cactus_uhal_tests
LIBRARY_VER_ABI = $(PACKAGE_VER_MAJOR).$(PACKAGE_VER_MINOR)


IncludePaths = include  \
		${EXTERN_BOOST_INCLUDE_PREFIX} \
		${EXTERN_PUGIXML_INCLUDE_PREFIX} \
		${BUILD_HOME}/uhal/log/include \
		${BUILD_HOME}/uhal/grammars/include \
		${BUILD_HOME}/uhal/uhal/include

LibraryPaths = ${EXTERN_BOOST_LIB_PREFIX} \
		${EXTERN_PUGIXML_LIB_PREFIX} \
		${BUILD_HOME}/uhal/log/lib \
		${BUILD_HOME}/uhal/grammars/lib \
		${BUILD_HOME}/uhal/uhal/lib \
		lib

LIBFLAGPREFIX = -Wl,

Libraries = \
		cactus_uhal_uhal \
		cactus_uhal_grammars \
		cactus_uhal_log \
		$(if ${BUILD_STATIC}, ${LIBFLAGPREFIX}--no-whole-archive,) \
		boost_filesystem \
		boost_program_options \
		boost_unit_test_framework \
		boost_regex \
		boost_system \
		\
		pugixml \
		\
		rt \
		pthread \

ExecutableLibraries = \
		$(if ${BUILD_STATIC}, ${LIBFLAGPREFIX}--whole-archive,) \
		cactus_uhal_tests \
		${Libraries} \
		stdc++


# Hide c++11-extensions warning when building on osx
ifeq ($(CACTUS_OS),osx)
CXXFLAGS += -Wno-c++11-extensions
endif

ifndef BUILD_STATIC
  CXXFLAGS += -DBOOST_TEST_DYN_LINK
else
  IncludePaths += ${BUILD_HOME}/uhal/uhal/include/_static
  Libraries := $(filter-out pugixml, ${Libraries})
endif

include $(BUILD_HOME)/uhal/config/mfRules.mk
include $(BUILD_HOME)/uhal/config/mfRPMRules.mk
include $(BUILD_HOME)/uhal/config/mfInstallRules.mk
