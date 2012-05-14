#
# Makefile for boost
#
BUILD_HOME = $(shell pwd)/../..
INSTALL_PREFIX = /opt/cactus

Package = extern/boost
PackagePath = ${BUILD_HOME}/${Package}
PackageName = cactus-extern-boost

Packager = Andrew Rose, Marc Magrans de Abril
License = Boost Software License 

PACKAGE_VER_MAJOR = 1
PACKAGE_VER_MINOR = 48
PACKAGE_VER_PATCH = 0
PACKAGE_RELEASE = 2

TARBALL_NAME = boost_$(PACKAGE_VER_MAJOR)_$(PACKAGE_VER_MINOR)_$(PACKAGE_VER_PATCH)

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update

default: build

clean: _cleanall
_cleanall:
	rm -rf RPMBUILD
	rm -rf ${TARBALL_NAME}

all: _all
build: _all
buildall: _all
_all:
	mkdir -p RPMBUILD/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	tar -xvjf ${TARBALL_NAME}.tar.bz2
	cd ${TARBALL_NAME};./bootstrap.sh
	cd ${TARBALL_NAME};./b2 --without-mpi					\
				--prefix=${PackagePath}/RPMBUILD/SOURCES	\
				--exec-prefix=${PackagePath}/RPMBUILD/SOURCES	\
				variant=release					\
				link=shared					\
				threading=multi					\
				install
	find ${TARBALL_NAME}/. -name '*.html' -o -name '*.css' -o -name '*.htm' | cpio -p --make-directories RPMBUILD/SOURCES/doc

rpm: _rpmall
_rpmall: 
	mkdir -p RPMBUILD/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	rpmbuild -bb -bl --buildroot=${PackagePath}/RPMBUILD/BUILD						\
			--define  "_topdir ${PackagePath}/RPMBUILD"						\
			--define "_prefix ${INSTALL_PREFIX}"							\
			--define "sources_dir ${PackagePath}/RPMBUILD/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}"	\
			--define "release ${PACKAGE_RELEASE}"							\
			--define "packager ${Packager}"								\
			cactus-extern-boost.spec

