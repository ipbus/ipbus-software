BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal
Package = uhal/grammars
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-grammars

PackageURL = https://ipbus.web.cern.ch/ipbus
Packager = Andrew Rose, Tom Williams

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 5
PACKAGE_VER_PATCH = 0
PACKAGE_RELEASE = 1

Library = cactus_uhal_grammars


IncludePaths = include  \
		${EXTERN_BOOST_INCLUDE_PREFIX} \
		${UHAL_LOG_INCLUDE_PREFIX}

LibraryPaths = ${UHAL_LOG_LIB_PREFIX}	

Libraries = cactus_uhal_log


# Hide c++11-extensions warning when building on osx
ifeq ($(CACTUS_OS),osx)
CPP_FLAGS += -Wno-c++11-extensions
endif



include $(BUILD_HOME)/uhal/config/mfRules.mk
include $(BUILD_HOME)/uhal/config/mfRPMRules.mk