#
# Makefile for pugixml
#
BUILD_HOME = $(shell pwd)/../..

include ${BUILD_HOME}/config/Makefile.macros

Package = extern/pugixml
PackagePath = ${BUILD_HOME}/${Package}
PackageName = cactus-extern-pugixml

Packager = Andrew Rose, Marc Magrans de Abril

PACKAGE_VER_MAJOR = 1
PACKAGE_VER_MINOR = 0
PACKAGE_RELEASE = 0

ZIP_FILE = pugixml-${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.zip
ZIP_DIR = pugixml-${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update

default: build

clean: _cleanall
_cleanall:
	rm -rf RPMBUILD
	rm -rf ${ZIP_DIR}

all: _all
build: _all
buildall: _all
_all:
	mkdir -p RPMBUILD/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	unzip ${ZIP_FILE} -d ${ZIP_DIR}
	cd ${ZIP_DIR}; g++ -fPIC -O3 -shared -o libcactus_extern_pugixml.so src/pugixml.cpp
	mkdir -p RPMBUILD/SOURCES/{lib,include/pugixml}
	cp ${ZIP_DIR}/libpugixml.so RPMBUILD/SOURCES/lib
	cp ${ZIP_DIR}/src/*.hpp RPMBUILD/SOURCES/include/pugixml

rpm: _rpmall
_rpmall: 
	rpmbuild -bb -bl --buildroot=${PackagePath}/RPMBUILD/BUILD						\
			--define  "_topdir ${PackagePath}/RPMBUILD"						\
			--define "_prefix ${INSTALL_PREFIX}"							\
			--define "zip_file ${ZIP_FILE}"							\
			--define "sources_dir ${PackagePath}/RPMBUILD/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}"	\
			--define "release ${PACKAGE_RELEASE}"							\
			--define "packager ${Packager}"								\
			cactus-extern-pugixml.spec

