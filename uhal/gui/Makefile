BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal
Package = uhal/gui
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-gui

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 7
PACKAGE_VER_PATCH = 5
PACKAGE_RELEASE = 1${PACKAGE_RELEASE_SUFFIX}

PackageDescription = Python GUI for uTCA HW access based on uHAL
PackageURL = https://ipbus.web.cern.ch/ipbus
Packager = Carlos Ghabrous Larrea, Tom Williams
PackagerEmail = tom.williams@cern.ch


PythonModules = ["uhal.gui","uhal.gui.guis","uhal.gui.customguis","uhal.gui.utilities","uhal.gui.test"]


.PHONY: _all build clean
default: build
clean: ;
_all: ;
build: ;

include $(BUILD_HOME)/uhal/config/mfPythonRPMRules.mk
