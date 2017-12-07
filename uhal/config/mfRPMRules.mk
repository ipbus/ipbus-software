RPMBUILD_DIR=${PackagePath}/rpm/RPMBUILD


BuildDebuginfoRPM ?= 1
IncludePaths := $(if ${IncludePaths}, ${IncludePaths}, %{nil})
PackageSummary := $(if ${PackageSummary}, ${PackageSummary}, None)
PackageDescription := $(if ${PackageDescription}, ${PackageDescription}, None)
BUILD_REQUIRES_TAG = $(if ${PackageBuildRequires} ,BuildRequires: ${PackageBuildRequires} ,\# No BuildRequires tag )
REQUIRES_TAG = $(if ${PackageRequires} ,Requires: ${PackageRequires} ,\# No Requires tag )

RPM_RELEASE_SUFFIX = ${CACTUS_OS}$(if ${CPP_VERSION_TAG},.${CPP_VERSION_TAG},)

export BUILD_HOME

.PHONY: rpm _rpmall
rpm: _rpmall
_rpmall: _all _spec_update _rpmbuild

.PHONY: _rpmbuild
_rpmbuild: _spec_update
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	rpmbuild --quiet -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD --define  "_topdir ${RPMBUILD_DIR}" rpm/${PackageName}.spec
	find  ${RPMBUILD_DIR} -name "*.rpm" -exec mv {} $(PackagePath)/rpm \;

.PHONY: _spec_update	
_spec_update:
	mkdir -p ${PackagePath}/rpm
	cp ${BUILD_HOME}/uhal/config/specTemplate.spec ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__package__#${Package}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__packagename__#${PackageName}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__version__#$(PACKAGE_VER_MAJOR).$(PACKAGE_VER_MINOR).$(PACKAGE_VER_PATCH)#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__release__#${PACKAGE_RELEASE}.${RPM_RELEASE_SUFFIX}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__prefix__#${CACTUS_ROOT}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__sources_dir__#${RPMBUILD_DIR}/SOURCES#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__packagedir__#${PackagePath}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__os__#${CACTUS_OS}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__platform__#None#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__project__#${Project}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__author__#${Packager}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__summary__#${PackageSummary}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__description__#${PackageDescription}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__url__#${PackageURL}#' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's#__includedirs__#${IncludePaths}#' $(PackagePath)/rpm/$(PackageName).spec
	sed -i 's|^.*__build_requires__.*|${BUILD_REQUIRES_TAG}|' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's|^.*__requires__.*|${REQUIRES_TAG}|' ${PackagePath}/rpm/${PackageName}.spec
	sed -i 's|^BuildArch:.*|$(if ${PackageBuildArch},BuildArch: ${PackageBuildArch},\# BuildArch not specified)|' ${PackagePath}/rpm/${PackageName}.spec
	if [ "${BuildDebuginfoRPM}" == "1" ]; then sed -i '1 i\%define _build_debuginfo_package %{nil}' ${PackagePath}/rpm/${PackageName}.spec; fi

.PHONY: cleanrpm _cleanrpm
cleanrpm: _cleanrpm
_cleanrpm:
	-rm -r rpm

