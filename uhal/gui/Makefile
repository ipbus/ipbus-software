BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal
Package = uhal/gui
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-gui
PackageURL = https://ipbus.web.cern.ch/ipbus
PackageDescription = Python GUI for uTCA HW access based on uHAL
PythonModules = ["uhal.gui","uhal.gui.guis","uhal.gui.customguis","uhal.gui.utilities","uhal.gui.test"]
PackageScripts = ["uhal_gui.exe"]

Packager = Carlos Ghabrous Larrea, Tom Williams

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 3
PACKAGE_VER_PATCH = 0
PACKAGE_RELEASE = 0



.PHONY: _all build clean
clean: ;
_all: ;
build: ;

include $(BUILD_HOME)/uhal/config/mfPythonRPMRules.mk