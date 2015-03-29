BUILD_HOME = $(shell pwd)/../../..

include $(BUILD_HOME)/config/Makefile.macros

Project = cactuscore
Package = uhal/tools
PackagePath = $(CACTUS_RPM_ROOT)/$(Project)/${Package}
PackageName = cactuscore-uhal-tools

Packager = Kristian Harder, Dave Newbold, Tom Williams

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 4
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 0


RPMBUILD_DIR = ${PackagePath}/RPMBUILD


.PHONY: all _all clean _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update

default: build

clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}


all: _all
build: _all
buildall: _all
_all: ${LIBRARY} ${EXECUTABLES}


rpm: _rpmall
_rpmall: 
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p ${RPMBUILD_DIR}/SOURCES/{bin/${Package},etc/${Package}}
	cp -p scripts/* ${RPMBUILD_DIR}/SOURCES/bin/${Package}/.
	cp -p etc/${Package}/* ${RPMBUILD_DIR}/SOURCES/etc/${Package}
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD						\
			--define  "_topdir ${RPMBUILD_DIR}"						\
			--define "_prefix ${CACTUS_ROOT}"							\
			--define "sources_dir ${RPMBUILD_DIR}/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}"	\
			--define "release ${PACKAGE_RELEASE}.${CACTUS_OS}"							\
			--define "packager ${Packager}"								\
			${PackageName}.spec

