BUILD_UTILS_DIR = $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
RPMBUILD_DIR=${PackagePath}/rpm/RPMBUILD

ifndef PythonModules
	$(error Python module names missing "PythonModules")
endif

PackageDescription ?= None
PackageURL ?= None

CACTUS_ROOT ?= /opt/cactus

RPM_DIST = $(patsubst %.cern,%,$(shell rpm --eval "%{dist}"))
RPM_RELEASE_SUFFIX = ${RPM_DIST}$(if ${CXX_VERSION_TAG},.${CXX_VERSION_TAG},)

PYTHON_MAJOR_COMMAND := $(shell ${PYTHON} -c "import sys; print('python{}'.format(sys.version_info.major))")
PYTHON_VERSIONED_COMMAND := $(shell ${PYTHON} -c "import sys; print('python{}.{}'.format(sys.version_info.major,sys.version_info.minor))")
PYTHON_RPM_CAPABILITY := $(shell ${PYTHON} -c "import sys; print('python' + str(sys.version_info.major) + (str(sys.version_info.minor) if sys.version_info.major > 2 else ''))")
BUILD_ARCH = $(shell rpm --eval "%{_target_cpu}")

# By default, install Python bindings using same prefix & exec_prefix as main Python installation
ifdef prefix
	CUSTOM_INSTALL_PREFIX = true
endif
ifdef exec_prefix
	CUSTOM_INSTALL_EXEC_PREFIX = true
endif
include $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/mfInstallVariables.mk


.PHONY: rpm _rpmall
rpm: _rpmall
_rpmall: _all _setup_update _rpmbuild

# Copy the package skeleton
# Ensure the existence of the module directory
# Copy the libraries into python module
.PHONY: _rpmbuild
_rpmbuild: _setup_update
	find pkg -not -type d -printf '%P\0' | xargs -0 -n1 -I {} install -D pkg/{} ${RPMBUILD_DIR}/{}
	if [ -d etc -o -d scripts ]; then find scripts etc -not -type d -printf '%p\0' | xargs -0 -n1 -I {} cp --parents {} ${RPMBUILD_DIR}; fi
	find ${RPMBUILD_DIR} -type f -exec sed -i "s|#!/usr/bin/env python|#!/usr/bin/env "${PYTHON_VERSIONED_COMMAND}"|" {} \;
	# Add a manifest file
	echo "include */*.so" > ${RPMBUILD_DIR}/MANIFEST.in
	# Change into rpm/pkg to finally run the customized setup.py
	cd ${RPMBUILD_DIR} && \
	  LIB_REQUIRES=$$(find ${RPMBUILD_DIR} -type f -print0 | xargs -0 -n1 -I {} file {} \; | grep -v text | cut -d: -f1 | /usr/lib/rpm/find-requires | tr '\n' ' ') && \
	  ${PYTHON} ${PackageName}.py bdist_rpm --spec-only \
	    --release ${PACKAGE_RELEASE}${RPM_RELEASE_SUFFIX} \
	    --requires "${PYTHON_RPM_CAPABILITY} $$LIB_REQUIRES" \
	    --force-arch=${BUILD_ARCH} \
	    --binary-only
	sed -i "s|^"${PYTHON_MAJOR_COMMAND}" |"${PYTHON_VERSIONED_COMMAND}" |" ${RPMBUILD_DIR}/dist/*.spec
	cd ${RPMBUILD_DIR} && \
	  bindir=$(bindir) ${PYTHON} ${PackageName}.py sdist
	mkdir -p ${RPMBUILD_DIR}/build/bdist.linux-${BUILD_ARCH}/rpm/SOURCES
	cp ${RPMBUILD_DIR}/dist/${PackageName}-*.tar.gz ${RPMBUILD_DIR}/build/bdist.linux-${BUILD_ARCH}/rpm/SOURCES/
	cd ${RPMBUILD_DIR} && \
	  bindir=$(bindir) rpmbuild -bb \
	    --define 'debug_package %{nil}' \
	    --define '_topdir '${RPMBUILD_DIR}'/build/bdist.linux-${BUILD_ARCH}/rpm' \
	    --clean dist/${PackageName}.spec
	# Harvest the crop
	find ${RPMBUILD_DIR} -name "*.rpm" -print0 | xargs -0 -n1 -I {} mv {} rpm/


.PHONY: _setup_update	
_setup_update:
	${MakeDir} ${RPMBUILD_DIR}
	cp ${BUILD_UTILS_DIR}/setupTemplate.py ${RPMBUILD_DIR}/${PackageName}.py
	sed -i -e 's#__python_packages__#${PythonModules}#' \
	       -e 's#__packagename__#${PackageName}#' \
	       -e 's#__version__#${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}#' \
	       -e 's#__author__#${Packager}#' \
	       -e 's#__description__#${PackageDescription}#' \
	       -e 's#__url__#${PackageURL}#' \
	       -e 's#__project__#${Project}#' \
	       -e 's#__install_dir__#${CACTUS_ROOT}#' \
	       -e 's#__package_build_dir__#${RPMBUILD_DIR}#' \
	       ${RPMBUILD_DIR}/${PackageName}.py


.PHONY: cleanrpm _cleanrpm
cleanrpm: _cleanrpm
_cleanrpm:
	rm -rf rpm


.PHONY: install
install: _setup_update
	# Change directory into pkg and copy everything into rpm/pkg
	cd pkg && \
	  find . -name "*" -exec install -D \{\} ${RPMBUILD_DIR}/\{\} \;
	# Add a manifest file
	echo "include */*.so" > ${RPMBUILD_DIR}/MANIFEST.in
	# Change into rpm/pkg to finally run the customized setup.py
	if [ -f setup.cfg ]; then cp setup.cfg ${RPMBUILD_DIR}/ ; fi
	cd ${RPMBUILD_DIR} && \
	  bindir=$(bindir) ${PYTHON} ${PackageName}.py install $(if ${CUSTOM_INSTALL_PREFIX},--prefix=${prefix},) $(if ${CUSTOM_INSTALL_PREFIX}${CUSTOM_INSTALL_EXEC_PREFIX},--exec-prefix=${exec_prefix},)
