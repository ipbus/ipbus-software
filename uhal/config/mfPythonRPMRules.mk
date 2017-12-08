RPMBUILD_DIR=${PackagePath}/rpm/RPMBUILD
# 
ifndef PythonModules
    $(error Python module names missing "PythonModules")
endif
PackageScripts ?= []
PackageDescription ?= None


.PHONY: rpm _rpmall
rpm: _rpmall
_rpmall: _all _setup_update _rpmbuild

# Copy the package skeleton
# Insure the existence of the module directory
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
	cd ${RPMBUILD_DIR} && CACTUS_ROOT=${CACTUS_ROOT} python ${PackageName}.py bdist_rpm \
	  --release ${PACKAGE_RELEASE}.${CACTUS_OS}.${CPP_VERSION_TAG}.python${PYTHON_VERSION} \
	  --requires "python `find ${RPMBUILD_DIR} -type f -exec file {} \; | grep -v text | cut -d: -f1 | /usr/lib/rpm/find-requires | tr '\n' ' '`" \
	  --binary-only --force-arch=`uname -m`
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
