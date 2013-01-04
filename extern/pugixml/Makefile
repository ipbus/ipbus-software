#
# Makefile for pugixml
#

CACTUS_ROOT = $(shell pwd)/../..

include $(CACTUS_ROOT)/config/Makefile.macros

Package = extern/pugixml
PackagePath = $(CACTUS_BUILD_ROOT)/${Package}
PackageName = cactus-extern-pugixml

Packager = Andrew Rose, Marc Magrans de Abril

PACKAGE_VER_MAJOR = 1
PACKAGE_VER_MINOR = 0
PACKAGE_RELEASE = 0

LIBRARY = libcactus_extern_pugixml.so

ZIP_NAME = pugixml-${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}
ZIP_FILE = ${ZIP_NAME}.zip

RPMBUILD_DIR = ${PackagePath}/RPMBUILD

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
	if [ ! -d  ${ZIP_NAME}/.build_done ]; then											\
	unzip -u ${ZIP_FILE} -d ${ZIP_NAME};												\
	cd ${ZIP_NAME};																		\
	g++ -fPIC -O3 -shared -o ${LIBRARY} src/pugixml.cpp;								\
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS};	\
	mkdir -p ${RPMBUILD_DIR}/SOURCES/{lib,include/pugixml};								\
	cp ${LIBRARY} ${RPMBUILD_DIR}/SOURCES/lib;											\
	cp src/*.hpp ${RPMBUILD_DIR}/SOURCES/include/pugixml;								\
	date > .build_done;																	\
	fi; 

rpm: _rpmall
_rpmall: 
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD				\
			--define  "_topdir ${RPMBUILD_DIR}"				\
			--define "_prefix ${INSTALL_PREFIX}"				\
			--define "zip_file ${ZIP_FILE}"					\
			--define "sources_dir ${RPMBUILD_DIR}/SOURCES"			\
			--define "name ${PackageName}"					\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}"	\
			--define "release ${PACKAGE_RELEASE}"				\
			--define "packager ${Packager}"					\
			cactus-extern-pugixml.spec

