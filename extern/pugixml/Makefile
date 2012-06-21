#
# Makefile for pugixml
#

include $(shell pwd)/../../config/Makefile.macros

Package = extern/pugixml
PackagePath = $(CACTUS_BUILD_ROOT)/${Package}
PackageName = cactus-extern-pugixml

Packager = Andrew Rose, Marc Magrans de Abril

PACKAGE_VER_MAJOR = 1
PACKAGE_VER_MINOR = 0
PACKAGE_RELEASE = 0

LIBRARY = libcactus_extern_pugixml.so

ZIP_FILE = pugixml-${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.zip
ZIP_DIR = pugixml-${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}

RPMBUILD_DIR = ${PackagePath}/RPMBUILD

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	rm -rf ${ZIP_DIR}

all: _all
build: _all
buildall: _all
_all:
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	unzip -u ${ZIP_FILE} -d ${ZIP_DIR}
	cd ${ZIP_DIR}; g++ -fPIC -O3 -shared -o ${LIBRARY} src/pugixml.cpp
	mkdir -p ${RPMBUILD_DIR}/SOURCES/{lib,include/pugixml}
	cp ${ZIP_DIR}/${LIBRARY} ${RPMBUILD_DIR}/SOURCES/lib
	cp ${ZIP_DIR}/src/*.hpp ${RPMBUILD_DIR}/SOURCES/include/pugixml

rpm: _rpmall
_rpmall: 
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD						\
			--define  "_topdir ${RPMBUILD_DIR}"						\
			--define "_prefix ${INSTALL_PREFIX}"							\
			--define "zip_file ${ZIP_FILE}"							\
			--define "sources_dir ${RPMBUILD_DIR}/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}"	\
			--define "release ${PACKAGE_RELEASE}"							\
			--define "packager ${Packager}"								\
			cactus-extern-pugixml.spec

