BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal/tests
Package = uhal/tests
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-tests

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 6
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 1${PACKAGE_RELEASE_SUFFIX}

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

Libraries = \
		cactus_uhal_uhal \
		cactus_uhal_grammars \
		cactus_uhal_log \
		\
		boost_filesystem \
		boost_thread \
		boost_program_options \
		boost_unit_test_framework \
		\
		pthread \

ExecutableLibraries = \
		cactus_uhal_tests \
		${Libraries} \
		boost_chrono \
		boost_regex \
		boost_system \
		pugixml \
		stdc++


# Hide c++11-extensions warning when building on osx
ifeq ($(CACTUS_OS),osx)
CXXFLAGS += -Wno-c++11-extensions
endif



include $(BUILD_HOME)/uhal/config/mfRules.mk
include $(BUILD_HOME)/uhal/config/mfRPMRules.mk
include $(BUILD_HOME)/uhal/config/mfInstallRules.mk
