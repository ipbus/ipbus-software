#
# Makefile for ErlangOTP
#
BUILD_HOME = $(shell pwd)/../..

include ${BUILD_HOME}/config/Makefile.macros

Package = extern/erlang
PackagePath = ${BUILD_HOME}/${Package}
PackageName = cactus-extern-erlang

Packager = Robert Frazier

# ErlangOTP version numbers.  Erlang is versioned like so: R14B, R14B01, R14B02, .., R15B, etc etc.
PACKAGE_VER_MAJOR = 15
# If no minor version number, leave blank rather than "00"
PACKAGE_VER_MINOR = 01

# This is the version number for the RPM packaging.
# According to RPM spec-file docs, first time a given software version (as above) is packaged up,
# the release number should start at 1.  Each time the same software version is repackaged, you
# increment this number.
PACKAGE_RELEASE = 1

TARBALL_DIR = otp_src_R${PACKAGE_VER_MAJOR}B${PACKAGE_VER_MINOR}
TARBALL_FILE = ${TARBALL_DIR}.tar.gz


.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update

default: build

clean: _cleanall
_cleanall:
	rm -rf RPMBUILD
	rm -rf ${TARBALL_DIR}

all: _all
build: _all
buildall: _all
_all:
	mkdir -p RPMBUILD/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	tar -zxvf ${TARBALL_FILE}
	cd ${TARBALL_DIR}; ./configure --prefix=${PackagePath}/RPMBUILD/SOURCES --exec-prefix=${PackagePath}/RPMBUILD/SOURCES --without-javac
	cd ${TARBALL_DIR}; make
	cd ${TARBALL_DIR}; make install
	# Now replace some paths within the Erlang release so that they use the path for
	# the RPM installation directory rather than the path where the RPM was created.
	cd ${PackagePath}/RPMBUILD/SOURCES/lib/erlang; sed -i "s|${PackagePath}/RPMBUILD/SOURCES|${INSTALL_PREFIX}|" erts*/bin/{erl,start} releases/RELEASES bin/{erl,start}
	

rpm: _rpmall
_rpmall: 
	rpmbuild -bb -bl --buildroot=${PackagePath}/RPMBUILD/BUILD \
    --define  "_topdir ${PackagePath}/RPMBUILD" \
    --define "_prefix ${INSTALL_PREFIX}" \
    --define "tarball_file ${TARBALL_FILE}" \
    --define "sources_dir ${PackagePath}/RPMBUILD/SOURCES" \
    --define "name ${PackageName}" \
    --define "version R${PACKAGE_VER_MAJOR}B${PACKAGE_VER_MINOR}" \
    --define "release ${PACKAGE_RELEASE}" \
    --define "packager ${Packager}" \
    ${PackageName}.spec

