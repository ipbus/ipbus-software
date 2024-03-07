#########################################################################################
#
#  ControlHub Makefile; there are four top-level options for you:
#     - Do "make" to do a standard release build.
#     - Do "make dev" if you're just developing the code (completely local build + test).
#     - Do "make rpm" to build the release RPM (do a normal "make" first!).
#     - Do "make clean" to clean.
#
#  Credit to:
#     - Richard Carlsson for his erlang-factory presentation that gave inspiration
#       on how to implement automated testing.
#     - Erik Rigtorp for inspiration on how to automatically generate some of the
#       necessary release files.
#
#
#   Robert Frazier
#   March 2012
#
#########################################################################################


BUILD_HOME = $(shell pwd)/..

include $(BUILD_HOME)/config/Makefile.macros

Project = 
Package = controlhub
PackagePath = $(CACTUS_RPM_ROOT)/${Package}
PackageName = cactuscore-controlhub

Packager = Tom Williams

PACKAGE_VER_MAJOR = 2
PACKAGE_VER_MINOR = 8
PACKAGE_VER_PATCH = 13

# This is the version number for the RPM packaging.
PACKAGE_RELEASE = 1${PACKAGE_RELEASE_SUFFIX}

RPMBUILD_DIR = ${PackagePath}/rpm/RPMBUILD
RPMBUILD_SOURCES_DIR = ${RPMBUILD_DIR}/SOURCES

####################  Define path to erlang runtime and compiler  ####################
# Default : User erl, erlc & escript found in $PATH
ERL=erl
ERLC=erlc
ESCRIPT=escript
# If we're rebuilding Erlang using tarball from repo, then use that copy
ifeq ($(BUILD_ERLANG), 1)
    ERL=$(EXTERN_ERLANG_BIN_PREFIX)/erl
    ERLC=$(EXTERN_ERLANG_BIN_PREFIX)/erlc
    ESCRIPT=$(shell pwd)/erlang/bin/escript
endif
######################################################################################

.PHONY: all _all clean  _cleanall build _buildall cleanrpm rpm _rpmall uninstall install _erlang check-version

default: build

all: _all
build: _all
buildall: _all

_all:
	${ESCRIPT} ${REBAR} get-deps compile
	cd rel && ${ESCRIPT} ${REBAR} generate

ifeq ($(BUILD_ERLANG), 1)
    _all: _erlang
endif


ERLANG_VERSION_MAJOR:=$(shell erl -noshell -eval 'io:fwrite("~s\n", [erlang:system_info(otp_release)]).' -s erlang halt)
# Extract major number in case formatted as RxxByy-z (e.g. R16B03-1)
ERLANG_VERSION_MAJOR:=$(shell echo ${ERLANG_VERSION_MAJOR} | sed 's@^[^0-9]*\([0-9]\+\).*@\1@')

# Use ./rebar for Erlang versions >= 23.0
ifeq ($(shell [ ${ERLANG_VERSION_MAJOR} -gt 22 ] && echo true),true)
    REBAR=$(shell pwd)/rebar
else
	REBAR=$(shell pwd)/legacy/rebar
endif

ifeq (${CACTUS_OS}, el7)
    RPMSPEC_SNIPPET_OS_TAG=rhel7
else
    RPMSPEC_SNIPPET_OS_TAG=rhelLeq6
endif


RPM_DIST = $(patsubst %.cern,%,$(shell rpm --eval "%{dist}"))

rpm: _rpmall
_rpmall: build
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES/{lib,bin},SRPMS}
	cp -r rel/controlhub ${RPMBUILD_SOURCES_DIR}/lib/
	cp scripts/controlhub_* ${RPMBUILD_SOURCES_DIR}/bin
	cp pkg/rsyslog.d.conf ${RPMBUILD_SOURCES_DIR}/ 
	cp pkg/logrotate.d.conf ${RPMBUILD_SOURCES_DIR}/
ifneq ($(CACTUS_OS),el7)
	cp pkg/init.d ${RPMBUILD_SOURCES_DIR}/controlhub
else
	cp pkg/systemd/controlhub.service ${RPMBUILD_SOURCES_DIR}/
	cp pkg/systemd/controlhub.env ${RPMBUILD_SOURCES_DIR}/
endif	
	cp pkg/cactuscore-controlhub.spec.template ${PackageName}.spec
	sed -i -e '/___INSTALL_SCRIPTLET_FINALE___/ {' -e 'r pkg/cactuscore-controlhub.spec.$(RPMSPEC_SNIPPET_OS_TAG).install-scriptlet-finale' -e 'd' -e '}' ${PackageName}.spec
	sed -i -e '/___POST_PREUN_SCRIPTLETS___/ {' -e 'r pkg/cactuscore-controlhub.spec.$(RPMSPEC_SNIPPET_OS_TAG).post-preun-scriptlets' -e 'd' -e '}' ${PackageName}.spec
	sed -i -e '/___FILELIST_FINALE___/ {' -e 'r pkg/cactuscore-controlhub.spec.$(RPMSPEC_SNIPPET_OS_TAG).filelist-finale' -e 'd' -e '}' ${PackageName}.spec
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD \
		--define  "_topdir ${RPMBUILD_DIR}" \
		--define "_prefix ${CACTUS_ROOT}" \
		--define "sources_dir ${RPMBUILD_SOURCES_DIR}" \
		--define "name ${PackageName}" \
		--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}" \
		--define "release ${PACKAGE_RELEASE}${RPM_DIST}" \
		--define "packager ${Packager}" \
		${PackageName}.spec
	find  ${RPMBUILD_DIR} -name "*.rpm" -exec mv {} $(PackagePath)/rpm \;

cleanrpm:
	rm -rf ${PackageName}.spec rpm


clean: _cleanall
_cleanall:
	${ESCRIPT} ${REBAR} clean
	rm -rf $(RPMBUILD_DIR) .rebar rpm deps ebin/*.beam erlang rel/controlhub ${PackageName}.spec


ERL_TARBALL_FILE=otp_src_R16B01.tar.gz
ERL_TARBALL_DIR=otp_src_R16B01
ERL_TARGET_DIR=$(shell pwd)/erlang

_erlang: erlang/.build_done
erlang/.build_done:
	mkdir -p erlang
	$(info Extracting: ${ERL_TARBALL_FILE})
	source $(BUILD_HOME)/config/progress.sh 'tar -tf ${ERL_TARBALL_FILE} | wc -l' 'tar -xvzf ${ERL_TARBALL_FILE}';
	cd ${ERL_TARBALL_DIR};                    \
	CFLAGS="-DOPENSSL_NO_EC=1" ./configure    \
             --prefix=${ERL_TARGET_DIR}           \
             --exec-prefix=${ERL_TARGET_DIR}      \
             --with-ssl                           \
             --without-javac;                     \
        make;                                     \
        make install;                             \
        rm -rf ${ERL_TARBALL_DIR};                \
        date > ${ERL_TARGET_DIR}/.build_done;



# Installation directories for all types of files (following GNU naming convention)
prefix = /opt/cactus
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib
includedir = $(prefix)/include
sysconfdir = $(prefix)/etc

install:
	cd scripts; find . -name "controlhub_*" -exec install -D -m 755 {} $(bindir)/{} \;
	mkdir -p $(libdir) && cp -r rel/controlhub $(libdir)/
	install -D -m 644 pkg/rsyslog.d.conf /etc/rsyslog.d/controlhub.conf
	install -D -m 644 pkg/logrotate.d.conf /etc/logrotate.d/controlhub.conf

	mkdir -p /etc/controlhub/default
	mv $(libdir)/controlhub/sys.config /etc/controlhub/default/sys.config
	sed -i "s|\".*core.config\"|\"$(libdir)/controlhub/core.config\"|" /etc/controlhub/default/sys.config
	sed -i "s|CONTROLHUB_CONFIG_FILE_DEFAULT=.*|CONTROLHUB_CONFIG_FILE_DEFAULT=/etc/controlhub/default/sys.config|" $(libdir)/controlhub/bin/controlhub
	sed -i "s|CONTROLHUB_CONFIG_FILE_OPTIONAL=.*|CONTROLHUB_CONFIG_FILE_OPTIONAL=/etc/controlhub/sys.config|" $(libdir)/controlhub/bin/controlhub

	sed -i "s|CONTROLHUB_BIN_DIR=.*|CONTROLHUB_BIN_DIR=$(libdir)/controlhub/bin|" $(bindir)/controlhub_*

	mkdir -p /var/log/controlhub
	touch /var/log/controlhub/controlhub.log
	chmod 644 /var/log/controlhub/controlhub.log
ifeq (,$(shell /bin/bash -c "command -v service"))
	@echo " WARNING : 'service' command not detected! After the installation has finished, you should restart rsyslog to ensure that the ControlHub syslog config is loaded"
else
	-service rsyslog restart
endif

uninstall:
	rm -rf $(bindir)/controlhub*
	rm -rf $(libdir)/controlhub
	rm -rf /etc/rsyslog.d/controlhub.conf /etc/logrotate.d/controlhub.conf /etc/controlhub/default


# Version check target
RELTOOL_CONFIG_PATH = rel/reltool.config
RELTOOL_CONFIG_VERSION_STRING = $(shell sed -e 's/.*rel, "controlhub", "\(.*\)",/\1/;t;d' ${RELTOOL_CONFIG_PATH})

CONTROLHUB_APP_PATH = ebin/controlhub.app
CONTROLHUB_APP_VERSION_STRING = $(shell sed -e 's/.*vsn, "\(.*\)".,/\1/;t;d' ${CONTROLHUB_APP_PATH})

check-version:
	$(info Checking ${RELTOOL_CONFIG_PATH})
ifneq (${RELTOOL_CONFIG_VERSION_STRING},${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH})
	$(error "${RELTOOL_CONFIG_PATH}: Value of version string "${RELTOOL_CONFIG_VERSION_STRING}" is inconsistent with PACKAGE_VER_ Makefile variables")
endif
	$(info Checking ${CONTROLHUB_APP_PATH})
ifneq (${CONTROLHUB_APP_VERSION_STRING},${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH})
	$(error "${CONTROLHUB_APP_PATH}: Value of version string "${CONTROLHUB_APP_VERSION_STRING}" is inconsistent with PACKAGE_VER_ Makefile variables")
endif
ifdef CI_COMMIT_TAG
	@echo Detected GitLab CI env. Checking that tag name matches makefile variable values
	@if ! [[ ${CI_COMMIT_TAG} =~ v${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}.* ]]; then echo "ERROR: Value of CI_COMMIT_TAG, \"${CI_COMMIT_TAG}\", inconsistent with PACKAGE_VER_ variables"; exit 1; fi
endif