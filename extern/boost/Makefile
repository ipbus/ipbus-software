BUILD_HOME = $(shell pwd)/../..

include $(BUILD_HOME)/config/Makefile.macros
include $(BUILD_HOME)/extern/config/mfCommonDefs.mk

Project = 
Package = extern/boost
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-extern-boost

Packager = Andrew Rose, Marc Magrans de Abril

PACKAGE_VER_MAJOR = 1
PACKAGE_VER_MINOR = 53
PACKAGE_VER_PATCH = 0
PACKAGE_RELEASE = 2${PACKAGE_RELEASE_SUFFIX}

TARBALL_NAME = boost_${PACKAGE_VER_MAJOR}_${PACKAGE_VER_MINOR}_${PACKAGE_VER_PATCH}
TARBALL_FILE = ${TARBALL_NAME}.tar.bz2

RPMBUILD_DIR = ${PackagePath}/RPMBUILD

.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall check-version

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	rm -rf ${TARBALL_NAME}

all: _all
build: _all
buildall: _all
_all:
	if [ ! -f ${TARBALL_NAME}/.build_done ]; then \
		mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}; \
		echo "Extracting: ${TARBALL_FILE}"; \
		source $(BUILD_HOME)/config/progress.sh 'tar -tf ${TARBALL_FILE} | wc -l' 'tar -xvjf ${TARBALL_FILE}'; \
		cd ${TARBALL_NAME}; \
		./bootstrap.sh; \
		./b2 -d0 \
			-sNO_ZLIB=0 \
			--without-mpi \
			--prefix=${RPMBUILD_DIR}/SOURCES \
			--exec-prefix=${RPMBUILD_DIR}/SOURCES \
			variant=release \
			link=shared \
			threading=multi \
			install; \
		date > .build_done; \
	fi;

rpm: _rpmall
_rpmall: 
	rm -rf ${RPMBUILD_DIR}/BUILD/*
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD						\
			--define  "_topdir ${RPMBUILD_DIR}"						\
			--define "_prefix ${CACTUS_ROOT}"							\
			--define "tarball_file ${TARBALL_NAME}.tar.bz2"							\
			--define "sources_dir ${RPMBUILD_DIR}/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}"	\
			--define "release ${PACKAGE_RELEASE}.${CACTUS_OS}.${CPP_VERSION_TAG}"					\
			--define "packager ${Packager}"								\
			${PackageName}.spec

cmsrpm: _cmsrpmall
_cmsrpmall:
	rm -rf ${RPMBUILD_DIR}/BUILD/*
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD                                            \
	                --define  "_topdir ${RPMBUILD_DIR}"                                             \
                        --define "_prefix /usr"                                                       \
                        --define "tarball_file ${TARBALL_NAME}.tar.bz2"                                                 \
                        --define "sources_dir ${RPMBUILD_DIR}/SOURCES"                                  \
                        --define "name ${PackageName}"                                                          \
                        --define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}"       \
                        --define "release ${PACKAGE_RELEASE}.cms.${CACTUS_OS}"                                      \
                        --define "packager ${Packager}"                                                         \
                        ${PackageName}.spec

check-version:
