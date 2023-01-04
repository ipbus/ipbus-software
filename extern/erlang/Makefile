BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros

Project =
Package = extern/erlang
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-extern-erlang

Packager = Robert Frazier

# ErlangOTP version numbers.  Erlang is versioned like so: R14B, R14B01, R14B02, .., R15B, etc etc.
PACKAGE_VER_MAJOR = 16
# If no minor version number, leave blank rather than "00"
PACKAGE_VER_MINOR = 01

# This is the version number for the RPM packaging.
# According to RPM spec-file docs, first time a given software version (as above) is packaged up,
# the release number should start at 1.  Each time the same software version is repackaged, you
# increment this number.
PACKAGE_RELEASE = 1${PACKAGE_RELEASE_SUFFIX}

TARBALL_DIR = otp_src_R${PACKAGE_VER_MAJOR}B${PACKAGE_VER_MINOR}
TARBALL_FILE = ${TARBALL_DIR}.tar.gz

RPMBUILD_DIR = ${PackagePath}/RPMBUILD

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update check-version

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	rm -rf ${TARBALL_DIR}

all: _all
build: _all
buildall: _all
_all:
	if [ ! -f ${TARBALL_DIR}/.build_done ]; then                                                     \
		mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS};     \
		echo "Extracting: ${TARBALL_FILE}";												 \
		source $(BUILD_HOME)/config/progress.sh 'tar -tf ${TARBALL_FILE} | wc -l' 'tar -xvzf ${TARBALL_FILE}'; \
		cd ${TARBALL_DIR};																\
		./configure                                                         \
			--prefix=${RPMBUILD_DIR}/SOURCES                                               \
			--exec-prefix=${RPMBUILD_DIR}/SOURCES                                          \
			--without-ssl                                                                  \
			--without-javac;                                                               \
		make;                                                                                  \
		make install;                                                                          \
		date > .build_done;																	\
	fi;

rpm: _rpmall
_rpmall: 
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD \
		--define  "_topdir ${RPMBUILD_DIR}" \
		--define "_prefix ${CACTUS_ROOT}" \
		--define "tarball_file ${TARBALL_FILE}" \
		--define "sources_dir ${RPMBUILD_DIR}/SOURCES" \
		--define "name ${PackageName}" \
		--define "version R${PACKAGE_VER_MAJOR}B${PACKAGE_VER_MINOR}" \
		--define "release ${PACKAGE_RELEASE}.${CACTUS_OS}" \
		--define "packager ${Packager}" \
		${PackageName}.spec

check-version:
