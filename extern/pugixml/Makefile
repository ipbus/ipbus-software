BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/extern/config/mfCommonDefs.mk

Project =
Package = extern/pugixml
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-extern-pugixml

Packager = Andrew Rose, Marc Magrans de Abril

PACKAGE_VER_MAJOR = 1
PACKAGE_VER_MINOR = 2
PACKAGE_RELEASE = 2${PACKAGE_RELEASE_SUFFIX}

LIBRARY = libpugixml.so

ZIP_NAME = pugixml-${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}
ZIP_FILE = ${ZIP_NAME}.zip

RPMBUILD_DIR = ${PackagePath}/RPMBUILD


CXX = g++
CXXFLAGS = -fPIC -O3


.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	rm -rf ${ZIP_NAME}

all: _all
build: _all
buildall: _all
_all:
	if [ ! -f  ${ZIP_NAME}/.build_done ]; then \
	unzip -q -u ${ZIP_FILE} -d ${ZIP_NAME};                     \
	chmod -R a+r ${ZIP_NAME};                                   \
	cd ${ZIP_NAME};                                             \
	${CXX} ${CXXFLAGS} -shared -o ${LIBRARY} src/pugixml.cpp;   \
	date > .build_done;											\
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS};	\
	mkdir -p ${RPMBUILD_DIR}/SOURCES/{lib,include};								\
	cp ${LIBRARY} ${RPMBUILD_DIR}/SOURCES/lib;								\
	cp src/*.hpp ${RPMBUILD_DIR}/SOURCES/include;	\
	fi;	
rpm: _rpmall
_rpmall: 
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD				\
			--define  "_topdir ${RPMBUILD_DIR}"				\
			--define "_prefix ${CACTUS_ROOT}"				\
			--define "zip_file ${ZIP_FILE}"					\
			--define "sources_dir ${RPMBUILD_DIR}/SOURCES"			\
			--define "name ${PackageName}"					\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}"	\
			--define "release ${PACKAGE_RELEASE}.${CACTUS_OS}.${CPP_VERSION_TAG}"		\
			--define "packager ${Packager}"					\
			${PackageName}.spec

check-version:
