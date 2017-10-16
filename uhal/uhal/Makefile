BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal
Package = uhal/uhal
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-uhal

PackageURL = https://ipbus.web.cern.ch/ipbus
Packager = Andrew Rose, Tom Williams

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 5
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 1

Library = cactus_uhal_uhal


IncludePaths = include  \
		${UHAL_GRAMMARS_INCLUDE_PREFIX} \
		${UHAL_LOG_INCLUDE_PREFIX} \
		${EXTERN_BOOST_INCLUDE_PREFIX} \
		${EXTERN_PUGIXML_INCLUDE_PREFIX} 

LibraryPaths = ${EXTERN_BOOST_LIB_PREFIX} \
		${EXTERN_PUGIXML_LIB_PREFIX} \
		${UHAL_GRAMMARS_LIB_PREFIX} \
		${UHAL_LOG_LIB_PREFIX}

Libraries = pthread \
		\
		${PUGIXML_LIB_NAME} \
		\
		boost_thread \
		boost_system \
		boost_filesystem \
		boost_regex \
		boost_thread \
		\
		cactus_uhal_grammars \
		cactus_uhal_log	



CXXFLAGS += -DDISABLE_PACKET_COUNTER_HACK\
		-DRUN_ASIO_MULTITHREADED # \
#					 -DNO_PREEMPTIVE_DISPATCH 

#CPP_FLAGS += -fdelayed-template-parsing 

#-D THROW_ON_ADDRESS_SPACE_OVERLAP 

# NOTE: Adding this flag is a dirty hack to make uhal/uhal compile on 
# OS X. The fact that this is necessary really means the code is 
# fishy. 
ifeq ($(CACTUS_OS),osx) 
CPP_FLAGS += -Wno-c++11-extensions -fdelayed-template-parsing
endif 



include $(BUILD_HOME)/uhal/config/mfRules.mk
include $(BUILD_HOME)/uhal/config/mfRPMRules.mk