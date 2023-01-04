BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal
Package = uhal/log
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-log

PackageSummary = uHAL Logging Library
PackageDescription = uHAL Logging Library
PackageURL = https://ipbus.web.cern.ch/ipbus
Packager = Andrew Rose, Tom Williams


Library = cactus_uhal_log
LIBRARY_VER_ABI = $(PACKAGE_VER_MAJOR).$(PACKAGE_VER_MINOR)


IncludePaths = include  \
	${EXTERN_BOOST_INCLUDE_PREFIX}

LibraryPaths = ${EXTERN_BOOST_LIB_PREFIX}

Libraries = pthread

CXXFLAGS += -D MAX_NUM_ARGS=32


# Hide c++11-extensions warning when building on osx
ifeq ($(CACTUS_OS),osx)
  CXXFLAGS += -Wno-c++11-extensions
endif


IgnoreSources = src/common/generator.cxx

include $(BUILD_HOME)/uhal/config/mfRules.mk
include $(BUILD_HOME)/uhal/config/mfRPMRules.mk
include $(BUILD_HOME)/uhal/config/mfInstallRules.mk


# Extra rules for compiling the log.hxx / generator

GENERATOR_EXE = ${PackagePath}/generator.exe
GENERATOR_OBJ = ${PackagePath}/obj/generator.o
GENERATED_HEADERS = ${PackagePath}/include/uhal/log/log.hpp ${PackagePath}/include/uhal/log/log.hxx
GENERATED_FILES = ${GENERATED_HEADERS} ${PackagePath}/src/common/log.cpp

${LibraryObjectFiles} : ${GENERATED_HEADERS}
ifeq (,$(findstring ${PackagePath}/obj/log.o,${LibraryObjectFiles}))
    ${LibraryFile} : ${PackagePath}/obj/log.o
    LibraryObjectFiles += ${PackagePath}/obj/log.o
endif

${GENERATOR_EXE} : ${GENERATOR_OBJ}
	${LD} ${LDFLAGS} -lstdc++ $< -o $@

.PHONY: generate_files
generate_files : ${GENERATOR_EXE}
	./generator.exe

.PHONY: clean_generated_files
clean_generated_files:
	rm -rf src/common/log.cpp
	rm -rf include/uhal/log/log.hpp include/uhal/log/log.hxx
