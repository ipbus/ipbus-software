#
# Makefile for uhal tutorials
#

CACTUS_ROOT = $(shell pwd)/../..

include $(CACTUS_ROOT)/config/Makefile.macros

Package = uhal/tutorials
PackagePath = $(CACTUS_BUILD_ROOT)/${Package}
PackageName = cactus-uhal-tutorials

Packager = Andrew Rose

PACKAGE_VER_MAJOR = 0
PACKAGE_VER_MINOR = 0
PACKAGE_VER_PATCH = 1
PACKAGE_RELEASE = 1

LIBRARY_PATH = 	-L${EXTERN_BOOST_LIB_PREFIX} \
		-L${EXTERN_PUGIXML_LIB_PREFIX} \
		-L${UHAL_LOG_LIB_PREFIX} \
		-L${UHAL_GRAMMARS_LIB_PREFIX} \
		-L${UHAL_UHAL_LIB_PREFIX} 

LIBRARIES = 	\
		-lpthread \
		\
		-lboost_filesystem \
		-lboost_regex \
		-lboost_system \
		-lboost_thread \
		\
		-lcactus_extern_pugixml \
		-lcactus_uhal_log \
		-lcactus_uhal_grammars \
		-lcactus_uhal_uhal

INCLUDE_PATH = 	-Iinclude  \
		-I${UHAL_LOG_INCLUDE_PREFIX} \
		-I${UHAL_GRAMMARS_INCLUDE_PREFIX} \
		-I${EXTERN_BOOST_INCLUDE_PREFIX} \
		-I${EXTERN_PUGIXML_INCLUDE_PREFIX} \
		-I${UHAL_UHAL_INCLUDE_PREFIX}

CPP_FLAGS = -O3 -Wall -fPIC
LINK_FLAGS = -fPIC -Wall -O3

RPMBUILD_DIR = ${PackagePath}/RPMBUILD

export

MAKE=make
PACKAGES = \
	tutorial_1 \
	tutorial_2 

TARGETS=all _all build _buildall install _installall test _testall spec_update
.PHONY: $(TARGETS) rpm _rpmall clean _cleanall
default: build

$(TARGETS):
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	mkdir -p ${RPMBUILD_DIR}/SOURCES/bin
	for p in $(PACKAGES) ; do $(MAKE) -C $$p $@ ; done
	for p in `find . -iname "*.exe"` ; do cp -p $$p ${RPMBUILD_DIR}/SOURCES/bin/. ; done


clean: _cleanall
_cleanall:
	rm -rf ${RPMBUILD_DIR}
	for p in $(PACKAGES) ; do $(MAKE) -C $$p $@ ; done
	
rpm: _rpmall
_rpmall: 
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD						\
			--define  "_topdir ${RPMBUILD_DIR}"						\
			--define "_prefix ${INSTALL_PREFIX}"							\
			--define "sources_dir ${RPMBUILD_DIR}/SOURCES"					\
			--define "name ${PackageName}"								\
			--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}"	\
			--define "release ${PACKAGE_RELEASE}"							\
			--define "packager ${Packager}"								\
			cactus-uhal-tutorials.spec

