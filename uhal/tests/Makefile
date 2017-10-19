BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal/tests
Package = uhal/tests
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-tests

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 5
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 1

PackageSummary = uHAL Library Tests
PackageDescription = uHAL Library Tests
PackageURL = https://ipbus.web.cern.ch/ipbus
Packager = Andrew Rose, Marc Magrans de Arbil, Tom Williams


Library = cactus_uhal_tests


IncludePaths = include  \
		${UHAL_LOG_INCLUDE_PREFIX} \
		${UHAL_GRAMMARS_INCLUDE_PREFIX} \
		${EXTERN_BOOST_INCLUDE_PREFIX} \
		${EXTERN_PUGIXML_INCLUDE_PREFIX} \
		${UHAL_UHAL_INCLUDE_PREFIX}

LibraryPaths = ${EXTERN_BOOST_LIB_PREFIX} \
		${EXTERN_PUGIXML_LIB_PREFIX} \
		${UHAL_LOG_LIB_PREFIX} \
		${UHAL_GRAMMARS_LIB_PREFIX} \
		${UHAL_UHAL_LIB_PREFIX} \
		lib 

Libraries = \
		cactus_uhal_uhal \
		cactus_uhal_grammars \
		cactus_uhal_log \
		\
		boost_thread \
		boost_filesystem \
		boost_thread \
		boost_program_options \
		boost_unit_test_framework \
		\
		pthread \

ExecutableLibraries = \
		cactus_uhal_tests \
		${Libraries} \
		boost_regex \
		boost_system \
		${PUGIXML_LIB_NAME}


# Hide c++11-extensions warning when building on osx
ifeq ($(CACTUS_OS),osx)
CXXFLAGS += -Wno-c++11-extensions
endif



include $(BUILD_HOME)/uhal/config/mfRules.mk
include $(BUILD_HOME)/uhal/config/mfRPMRules.mk
