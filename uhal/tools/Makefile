BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal/tools
Package = uhal/tools
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-tools

PackageSummary = uTCA HW Development Tools that depend on uHAL
PackageDescription = uTCA HW Development Tools that depend on uHAL
PackageURL = https://ipbus.web.cern.ch/ipbus
Packager = Kristian Harder, Dave Newbold, Tom Williams
PackageBuildArch = noarch


.PHONY: _all build clean
clean: ;
_all: ;
build: ;

BuildDebuginfoRPM = 0

include $(BUILD_HOME)/uhal/config/mfRPMRules.mk
include $(BUILD_HOME)/uhal/config/mfInstallRules.mk
