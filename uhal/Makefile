
.NOTPARALLEL:


PACKAGES = \
	log \
	grammars \
	uhal \
	tests \
	python \
	gui \
	tools


BUILD_UHAL_TESTS?=1
ifneq ($(BUILD_UHAL_TESTS), 1)
	PACKAGES := $(filter-out tests, $(PACKAGES))
endif

BUILD_UHAL_PYTHON?=1
ifneq ($(BUILD_UHAL_PYTHON), 1)
	PACKAGES := $(filter-out python, $(PACKAGES))
endif

BUILD_UHAL_GUI?=1
ifneq ($(BUILD_UHAL_GUI), 1)
	PACKAGES := $(filter-out gui, $(PACKAGES))
endif

BUILD_UHAL_TOOLS?=1
ifneq ($(BUILD_UHAL_TOOLS), 1)
	PACKAGES := $(filter-out tools, $(PACKAGES))
endif



VIRTUAL_PACKAGES = $(addsuffix /.virtual.Makefile,${PACKAGES})

FLAGS = $(ifeq $(MAKEFLAGS) "","",-$(MAKEFLAGS))

TARGETS=all clean build cleanrpm rpm install

.PHONY: $(TARGETS)
default: build

$(TARGETS): ${VIRTUAL_PACKAGES}

${VIRTUAL_PACKAGES}:
	${MAKE} ${FLAGS} -C $(@D) $(MAKECMDGOALS)



# Uninstall target: Defined here, rather than in Makefiles for each individual uhal package (e.g. log, grammars, etc),
#     because removing installed files from one of the individual packages without removing any from other uhal packages
#     would be much harder to implement (especially when coping with changes to filenames between different releases)

PYTHON ?= python
ifndef prefix
  PYTHON_DIRECTORIES = $(shell ${PYTHON} -c "import site,sys; print(' '.join([x+'/*uhal*' for x in site.getsitepackages()]))")
else
  PYTHON_DIRECTORIES = ${prefix}/lib/python*/*-packages/*uhal* ${exec_prefix}/lib/python*/*-packages/*uhal*
endif
CONFIG_DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))/config
include ${CONFIG_DIR}/mfInstallVariables.mk

.PHONY: uninstall
uninstall:
	rm -rf $(bindir)/uhal
	rm -rf $(libdir)/libcactus_uhal*
	rm -rf $(includedir)/uhal
	rm -rf $(sysconfdir)/uhal
	rm -rf $(PYTHON_DIRECTORIES)


# Version check target
include ${CONFIG_DIR}/mfCommonDefs.mk
VERSION_HPP_MAJOR = $(shell sed -n -e "s/\#define UHAL_VERSION_MAJOR //p" uhal/include/uhal/version.hpp)
VERSION_HPP_MINOR = $(shell sed -n -e "s/\#define UHAL_VERSION_MINOR //p" uhal/include/uhal/version.hpp)
VERSION_HPP_PATCH = $(shell sed -n -e "s/\#define UHAL_VERSION_PATCH //p" uhal/include/uhal/version.hpp)

.PHONY: check-version
check-version:
	@echo Checking version.hpp
ifneq (${VERSION_HPP_MAJOR},${PACKAGE_VER_MAJOR})
	$(error "version.hpp: UHAL_VERSION_MAJOR has incorrect value")
endif
ifneq (${VERSION_HPP_MINOR},${PACKAGE_VER_MINOR})
	$(error "version.hpp: UHAL_VERSION_MINOR has incorrect value")
endif
ifneq (${VERSION_HPP_PATCH},${PACKAGE_VER_PATCH})
	$(error "version.hpp: UHAL_VERSION_PATCH has incorrect value")
endif
ifdef CI_COMMIT_TAG
	@echo Detected GitLab CI env. Checking that tag name matches makefile variable values
	@if ! [[ ${CI_COMMIT_TAG} =~ v${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}.* ]]; then echo "ERROR: Value of CI_COMMIT_TAG, \"${CI_COMMIT_TAG}\", inconsistent with PACKAGE_VER_ variables"; exit 1; fi
endif