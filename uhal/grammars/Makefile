BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal
Package = uhal/grammars
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-grammars

PackageSummary = uHAL Boost Spirit Grammars
PackageDescription = uHAL Boost Spirit Grammars
PackageURL = https://ipbus.web.cern.ch/ipbus
Packager = Andrew Rose, Tom Williams


Library = cactus_uhal_grammars
LIBRARY_VER_ABI = $(PACKAGE_VER_MAJOR).$(PACKAGE_VER_MINOR)


IncludePaths = include  \
		${EXTERN_BOOST_INCLUDE_PREFIX} \
		${BUILD_HOME}/uhal/log/include

LibraryPaths = ${BUILD_HOME}/uhal/log/lib

Libraries = 


# Hide c++11-extensions warning when building on osx
ifeq ($(CACTUS_OS),osx)
CXXFLAGS += -Wno-c++11-extensions
endif



include $(BUILD_HOME)/uhal/config/mfRules.mk
include $(BUILD_HOME)/uhal/config/mfRPMRules.mk
include $(BUILD_HOME)/uhal/config/mfInstallRules.mk
