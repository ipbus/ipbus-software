BUILD_HOME=$(shell pwd)/..

include Makefile.macros

Project = cactus
Package = config
PackagePath = ${BUILD_HOME}/config
PackageName = cactus-config

Packager = Alesandro Thea

PACKAGE_VER_MAJOR = 0
PACKAGE_VER_MINOR = 1
PACKAGE_VER_PATCH = 0
PACKAGE_RELEASE = 0


# -- Move this into a separate mkfile?
RPMBUILD_DIR=${PackagePath}/rpm/RPMBUILD

ifndef PackageName
ERROR="Missing PackageName for '$(Package)'
endif

ifndef PACKAGE_VER_MAJOR
ERROR="Missing major version number for '$(Package)'
endif

ifndef PACKAGE_VER_MINOR
ERROR="Missing minor version number for '$(Package)'
endif

ifndef PACKAGE_VER_PATCH
ERROR="Missing patch version number for '$(Package)'
endif


ifneq ($(ERROR),)
_rpmall: fail
fail:
    $(error $(ERROR))
endif


.PHONY: all _all build _buildall 
default: build

all: _all
build: _all
buildall: _all
_all:

.PHONY: rpm _rpmall

rpm: _rpmall
_rpmall: _spec_update
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD --define  "_topdir ${RPMBUILD_DIR}" rpm/${PackageName}.spec
	find  ${RPMBUILD_DIR} -name "*.rpm" -exec mv {} $(PackagePath)/rpm \;

.PHONY: _spec_update   
_spec_update:
	mkdir -p ${PackagePath}/rpm
	cp ${PackagePath}/configTemplate.spec ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__package__#${Package}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__packagename__#${PackageName}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__version__#$(PACKAGE_VER_MAJOR).$(PACKAGE_VER_MINOR).$(PACKAGE_VER_PATCH)#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__release__#${PACKAGE_RELEASE}.${CACTUS_OS}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__prefix__#${CACTUS_ROOT}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__sources_dir__#${RPMBUILD_DIR}/SOURCES#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__packagedir__#${PackagePath}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__os__#${CACTUS_OS}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__platform__#None#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__project__#${Project}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__author__#${Packager}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__summary__#None#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__description__#None#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__url__#None#' ${PackagePath}/rpm/${PackageName}.spec

.PHONY: clean _cleanall
clean: _cleanall
_cleanall:
	-rm -r rpm