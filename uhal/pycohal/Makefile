BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/uhal/config/mfCommonDefs.mk

Project = uhal
Package = uhal/pycohal
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-uhal-pycohal

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 5
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 1

PackageDescription = Python bindings for the CACTUS uhal library
PackageURL = https://ipbus.web.cern.ch/ipbus
Packager = Tom Williams
PackagerEmail = tom.williams@cern.ch


PythonModules = ["uhal"]
LibraryTarget = pkg/uhal/_core.so


IncludePaths = include  \
		${EXTERN_BOOST_INCLUDE_PREFIX} \
		${PYTHON_INCLUDE_PREFIX} \
		${EXTERN_PUGIXML_INCLUDE_PREFIX}  \
		${UHAL_UHAL_INCLUDE_PREFIX}  \
		${UHAL_LOG_INCLUDE_PREFIX}  \
		${UHAL_GRAMMARS_INCLUDE_PREFIX}

LibraryPaths = \
		${EXTERN_BOOST_LIB_PREFIX} \
		${EXTERN_PUGIXML_LIB_PREFIX} \
		${UHAL_UHAL_LIB_PREFIX} \
		${UHAL_LOG_LIB_PREFIX} \
		${UHAL_GRAMMARS_LIB_PREFIX} \
		${PYTHON_LIB_PREFIX}

Libraries = \
		pthread \
		dl \
		util \
		${PYTHON_LIB} \
		\
		boost_filesystem \
		boost_python \
		boost_regex \
		boost_system \
		boost_thread \
		\
		${PUGIXML_LIB_NAME} \
		cactus_uhal_grammars \
		cactus_uhal_log \
		cactus_uhal_uhal


CXXFLAGS += -ftemplate-depth-128 -O0 -rdynamic -finline-functions -Wno-inline -DNDEBUG \
		-DDISABLE_PACKET_COUNTER_HACK \
		-DRUN_ASIO_MULTITHREADED

ifeq (${CACTUS_OS},osx)
  CXXFLAGS += -Wno-c++11-extensions
else
  LDFLAGS += -Wl,-h -Wl,-Bstatic -Wl,-Bdynamic
endif


include $(BUILD_HOME)/uhal/config/mfRules.mk
include $(BUILD_HOME)/uhal/config/mfPythonRPMRules.mk


# FIXME: Add missing lib dependencies (hopefully in a generic way) ...

# rpm: _rpmall
# _rpmall: ${BINDINGS_LIB}
# 	rm -rf /var/tmp/cactus*
# 	$(eval LIB_DEPENDENCIES := $(shell ldd ${BINDINGS_LIB} | sed -n 's/\s*\(libboost_python.so[^ ]*\).*/\1()(64bit)/p'))
# 	$(eval LIB_DEPENDENCIES += $(shell ldd ${BINDINGS_LIB} | sed -n 's/\s*\(libcactus_uhal_uhal.so[^ ]*\).*/\1()(64bit)/p'))
# 	$(info LIB_DEPENDENCIES=${LIB_DEPENDENCIES})
# 	python setup.py bdist_rpm --binary-only --force-arch=`uname -m` --requires "${LIB_DEPENDENCIES}" \
# 	        --release ${PACKAGE_RELEASE}.${CACTUS_OS}.python${PYTHON_VERSION}
