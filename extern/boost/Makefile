#
# Makefile for boost
#

CACTUS_ROOT = $(shell pwd)/../..

include $(CACTUS_ROOT)/config/Makefile.macros

Package = extern/boost
PackagePath = $(CACTUS_BUILD_ROOT)/${Package}
PackageName = cactus-extern-boost

Packager = Andrew Rose, Marc Magrans de Abril

PACKAGE_VER_MAJOR = 1
PACKAGE_VER_MINOR = 48
PACKAGE_VER_PATCH = 0
PACKAGE_RELEASE = 2

TARBALL_NAME = boost_${PACKAGE_VER_MAJOR}_${PACKAGE_VER_MINOR}_${PACKAGE_VER_PATCH}

RPMBUILD_DIR = ${PackagePath}/RPMBUILD

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	rm -rf ${TARBALL_NAME}

all: _all
build: _all
buildall: _all
_all:
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	tar -xvjf ${TARBALL_NAME}.tar.bz2 | sed 's/^/extracted: /'
	cd ${TARBALL_NAME};./bootstrap.sh
	cd ${TARBALL_NAME};./b2 --without-mpi					\
				--prefix=${RPMBUILD_DIR}/SOURCES	\
				--exec-prefix=${RPMBUILD_DIR}/SOURCES	\
				variant=release					\
				link=shared					\
				threading=multi					\
				install

rpm: _rpmall
_rpmall: 
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD						\
			--define  "_topdir ${RPMBUILD_DIR}"						\
			--define "_prefix ${INSTALL_PREFIX}"							\
			--define "tarball_file ${TARBALL_NAME}.tar.bz2"							\
			--define "sources_dir ${RPMBUILD_DIR}/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}"	\
			--define "release ${PACKAGE_RELEASE}"							\
			--define "packager ${Packager}"								\
			cactus-extern-boost.spec

