
ifndef PythonModules
    $(error Python module names missing "PythonModules")
endif
PackageScripts ?= []
PackageDescription ?= None

# By default, install Python bindings using same prefix & exec_prefix as main Python installation
ifdef prefix
    CUSTOM_INSTALL_PREFIX = true
endif
ifdef exec_prefix
	CUSTOM_INSTALL_EXEC_PREFIX = true
endif
include $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/mfInstallVariables.mk

RPMBUILD_DIR=${PackagePath}/rpm/RPMBUILD



.PHONY: rpm _rpmall
rpm: _rpmall
_rpmall: _all _setup_update _rpmbuild

# Copy the package skeleton
# Ensure the existence of the module directory
# Copy the libraries into python module
.PHONY: _rpmbuild
_rpmbuild: _setup_update
	# Change directory into pkg and copy everything into rpm/pkg
	cd pkg && \
	  find . -name "*" -exec install -D \{\} ${RPMBUILD_DIR}/\{\} \;
	# Add a manifest file
	echo "include */*.so" > ${RPMBUILD_DIR}/MANIFEST.in
	# Change into rpm/pkg to finally run the customized setup.py
	if [ -f setup.cfg ]; then cp setup.cfg ${RPMBUILD_DIR}/ ; fi
	echo '%debug_package %{nil}' >> ~/.rpmmacros
	cd ${RPMBUILD_DIR} && bindir=$(bindir) python ${PackageName}.py bdist_rpm \
	  --release ${PACKAGE_RELEASE}.${CACTUS_OS}.${CXX_VERSION_TAG}.python${PYTHON_VERSION} \
	  --requires "python `find ${RPMBUILD_DIR} -type f -exec file {} \; | grep -v text | cut -d: -f1 | /usr/lib/rpm/find-requires | tr '\n' ' '`" \
	  --binary-only --force-arch=`uname -m` 
	sed -i '$ d' ~/.rpmmacros
	# Harvest the crop
	find ${RPMBUILD_DIR} -name "*.rpm" -exec mv {} ${PackagePath}/rpm \;


.PHONY: _setup_update
_setup_update:
	${MakeDir} ${RPMBUILD_DIR}
	cp ${BUILD_HOME}/uhal/config/setupTemplate.py ${RPMBUILD_DIR}/${PackageName}.py
	sed -i 's#__python_packages__#${PythonModules}#' ${RPMBUILD_DIR}/${PackageName}.py
	sed -i 's#__packagename__#${PackageName}#' ${RPMBUILD_DIR}/${PackageName}.py
	sed -i 's#__version__#$(PACKAGE_VER_MAJOR).$(PACKAGE_VER_MINOR).$(PACKAGE_VER_PATCH)#' ${RPMBUILD_DIR}/${PackageName}.py
	sed -i 's#__author__#${Packager}#' ${RPMBUILD_DIR}/${PackageName}.py
	sed -i 's#__author_email__#${PackagerEmail}#' ${RPMBUILD_DIR}/${PackageName}.py
	sed -i 's#__description__#${PackageDescription}#' ${RPMBUILD_DIR}/${PackageName}.py
	sed -i 's#__url__#${PackageURL}#' ${RPMBUILD_DIR}/${PackageName}.py
	sed -i 's#__scripts__#${PackageScripts}#' ${RPMBUILD_DIR}/${PackageName}.py


.PHONY: cleanrpm _cleanrpm
cleanrpm: _cleanrpm
_cleanrpm:
	-rm -r rpm


.PHONY: install
install: _setup_update
	# Change directory into pkg and copy everything into rpm/pkg
	cd pkg && \
	  find . -name "*" -exec install -D \{\} ${RPMBUILD_DIR}/\{\} \;
	# Add a manifest file
	echo "include */*.so" > ${RPMBUILD_DIR}/MANIFEST.in
	# Change into rpm/pkg to finally run the customized setup.py
	if [ -f setup.cfg ]; then cp setup.cfg ${RPMBUILD_DIR}/ ; fi
	cd ${RPMBUILD_DIR} && bindir=$(bindir) python ${PackageName}.py install $(if ${CUSTOM_INSTALL_PREFIX},--prefix=${prefix},) $(if ${CUSTOM_INSTALL_PREFIX}${CUSTOM_INSTALL_EXEC_PREFIX},--exec-prefix=${exec_prefix},)
