BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

PYTHON_VERSION_MAJOR_MINOR=$(shell ${PYTHON} -c "import sys; print(str(sys.version_info.major)+str(sys.version_info.minor))")


Project = uhal
Package = uhal/gui
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-python${PYTHON_VERSION_MAJOR_MINOR}-gui

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
