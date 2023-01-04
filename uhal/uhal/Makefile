BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal
Package = uhal/uhal
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-uhal

PackageSummary = uHAL Library
PackageDescription = uHAL Library
PackageURL = https://ipbus.web.cern.ch/ipbus
Packager = Andrew Rose, Tom Williams


Library = cactus_uhal_uhal
LIBRARY_VER_ABI = $(PACKAGE_VER_MAJOR).$(PACKAGE_VER_MINOR)


IncludePaths = include  \
		${EXTERN_BOOST_INCLUDE_PREFIX} \
		${EXTERN_PUGIXML_INCLUDE_PREFIX} \
		${BUILD_HOME}/uhal/log/include \
		${BUILD_HOME}/uhal/grammars/include

LibraryPaths = ${EXTERN_BOOST_LIB_PREFIX} \
		${EXTERN_PUGIXML_LIB_PREFIX} \
		${BUILD_HOME}/uhal/log/lib \
		${BUILD_HOME}/uhal/grammars/lib

Libraries = pthread \
		rt \
		\
		pugixml \
		\
		boost_filesystem \
		boost_regex \
		boost_system \
		boost_chrono \
		\
		cactus_uhal_grammars \
		cactus_uhal_log

#-DNO_PREEMPTIVE_DISPATCH

# NOTE: Adding this flag is a dirty hack to make uhal/uhal compile on
# OS X. The fact that this is necessary really means the code is
# fishy.
ifeq ($(CACTUS_OS),osx)
CXXFLAGS += -Wno-c++11-extensions -fdelayed-template-parsing
endif

ifdef BUILD_STATIC
  IncludePaths += include/_static
  Libraries = $(filter-out pugixml, ${Libraries})
else
  IgnoreSources = src/common/pugixml.cpp
endif


include $(BUILD_HOME)/uhal/config/mfRules.mk
include $(BUILD_HOME)/uhal/config/mfRPMRules.mk
include $(BUILD_HOME)/uhal/config/mfInstallRules.mk
