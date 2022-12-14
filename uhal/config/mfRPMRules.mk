RPMBUILD_DIR=${PackagePath}/rpm/RPMBUILD


BuildDebuginfoRPM ?= 1
IncludePaths := $(if ${IncludePaths}, ${IncludePaths}, %{nil})
PackageSummary := $(if ${PackageSummary}, ${PackageSummary}, None)
PackageDescription := $(if ${PackageDescription}, ${PackageDescription}, None)
BUILD_REQUIRES_TAG = $(if ${PackageBuildRequires} ,BuildRequires: ${PackageBuildRequires} ,\# No BuildRequires tag )
REQUIRES_TAG = $(if ${PackageRequires} ,Requires: ${PackageRequires} ,\# No Requires tag )

RPM_DIST = $(patsubst %.cern,%,$(shell rpm --eval "%{dist}"))
RPM_RELEASE_SUFFIX = ${RPM_DIST}$(if ${CXX_VERSION_TAG},.${CXX_VERSION_TAG},)

PYTHON_VERSIONED_COMMAND := $(shell ${PYTHON} -c "from sys import version_info; print('python' + str(version_info[0]))")

export BUILD_HOME

.PHONY: rpm _rpmall
rpm: _rpmall
_rpmall: _all _spec_update _rpmbuild

.PHONY: _rpmbuild
_rpmbuild: _spec_update
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	rpmbuild --quiet -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD --define  "_topdir ${RPMBUILD_DIR}" rpm/${PackageName}.spec
	find ${RPMBUILD_DIR} -name "*.rpm" -print0 | xargs -0 -I {} mv {} rpm/

.PHONY: _spec_update
_spec_update:
	mkdir -p ${PackagePath}/rpm
	cp ${BUILD_HOME}/uhal/config/specTemplate.spec ${PackagePath}/rpm/${PackageName}.spec
	sed -i -e 's#__package__#${Package}#' \
	       -e 's#__packagename__#${PackageName}#' \
	       -e 's#__version__#$(PACKAGE_VER_MAJOR).$(PACKAGE_VER_MINOR).$(PACKAGE_VER_PATCH)#' \
	       -e 's#__release__#${PACKAGE_RELEASE}${RPM_RELEASE_SUFFIX}#' \
	       -e 's#__prefix__#${CACTUS_ROOT}#' \
	       -e 's#__sources_dir__#${RPMBUILD_DIR}/SOURCES#' \
	       -e 's#__packagedir__#${PackagePath}#' \
	       -e 's#__platform__#None#' \
	       -e 's#__project__#${Project}#' \
	       -e 's#__author__#${Packager}#' \
	       -e 's#__summary__#${PackageSummary}#' \
	       -e 's#__description__#${PackageDescription}#' \
	       -e 's#__url__#${PackageURL}#' \
	       -e 's#__includedirs__#${IncludePaths}#' \
	       -e 's|^.*__build_requires__.*|${BUILD_REQUIRES_TAG}|' \
	       -e 's|^.*__requires__.*|${REQUIRES_TAG}|' \
	       -e 's|^BuildArch:.*|$(if ${PackageBuildArch},BuildArch: ${PackageBuildArch},\# BuildArch not specified)|' \
	       -e 's#__python_versioned_command__#${PYTHON_VERSIONED_COMMAND}#' \
	       ${PackagePath}/rpm/${PackageName}.spec
	if [ "${BuildDebuginfoRPM}" == "1" ]; then sed -i '1 i\%define _build_debuginfo_package %{nil}' ${PackagePath}/rpm/${PackageName}.spec; fi

.PHONY: cleanrpm _cleanrpm
cleanrpm: _cleanrpm
_cleanrpm:
	rm -rf rpm

