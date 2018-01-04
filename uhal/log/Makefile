BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal
Package = uhal/log
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-log

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 5
PACKAGE_VER_PATCH = 0
PACKAGE_RELEASE = 1${PACKAGE_RELEASE_SUFFIX}

PackageSummary = uHAL Logging Library
PackageDescription = uHAL Logging Library
PackageURL = https://ipbus.web.cern.ch/ipbus
Packager = Andrew Rose, Tom Williams


Library = cactus_uhal_log
LIBRARY_VER_ABI = $(PACKAGE_VER_MAJOR).$(PACKAGE_VER_MINOR)


USE_BACKTRACE ?= FALSE

ifeq ($(USE_BACKTRACE),TRUE)

  LibraryPaths = ${EXTERN_BOOST_LIB_PREFIX}\
		$(BUILD_HOME)/extern/binutils/binutils-2.23/bfd\
		$(BUILD_HOME)/extern/binutils/binutils-2.23/libiberty 
	
  Libraries = pthread \
		boost_thread
	
  IncludePaths = include  \
		${EXTERN_BOOST_INCLUDE_PREFIX} \
		$(BUILD_HOME)/extern/binutils/binutils-2.23/include/\
		$(BUILD_HOME)/extern/binutils/binutils-2.23/binutils/\
		$(BUILD_HOME)/extern/binutils/binutils-2.23/bfd/

  CXXFLAGS += -D MAX_NUM_ARGS=32 -DCOURTEOUS_EXCEPTIONS -DUSE_BACKTRACE \

  LDFLAGS += -Wl,--whole-archive -liberty -lbfd -lz -Wl,--no-whole-archive\

else

  IncludePaths = include  \
		${EXTERN_BOOST_INCLUDE_PREFIX}

  LibraryPaths = ${EXTERN_BOOST_LIB_PREFIX}
	
  Libraries = pthread \
		boost_thread

  CXXFLAGS += -D MAX_NUM_ARGS=32 -DCOURTEOUS_EXCEPTIONS \

endif		


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
${LibraryFile} : ${PackagePath}/obj/log.o
LibraryObjectFiles += ${PackagePath}/obj/log.o

${GENERATOR_EXE} : ${GENERATOR_OBJ}
	${LD} ${LDFLAGS} $< -o $@

${GENERATED_FILES} : ${GENERATOR_EXE}
	./generator.exe

.PHONY: _generator_clean
clean: _generator_clean
_generator_clean:
	rm -rf src/common/log.cpp
	rm -rf include/uhal/log/log.hpp include/uhal/log/log.hxx
