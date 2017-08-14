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
PACKAGE_VER_MINOR = 5
PACKAGE_VER_PATCH = 2

# This is the version number for the RPM packaging.
PACKAGE_RELEASE = 1

RPMBUILD_DIR = ${PackagePath}/RPMBUILD
RPMBUILD_SOURCES_DIR = ${RPMBUILD_DIR}/SOURCES

####################  Define path to erlang runtime and compiler  ####################
UNAME := $(shell uname)
# Allows me to still develop on Mac OS X with a standard Erlang install
ifeq ($(UNAME), Darwin)
ERL=erl
ERLC=erlc
ESCRIPT=escript
else
#If we're on Linux, use the cactuscore-extern-erlang RPM sources or RPM installation
ESCRIPT=$(shell pwd)/erlang/bin/escript
ifeq ($(shell test -d ${EXTERN_ERLANG_BIN_PREFIX} && echo OK),OK)
ERL=$(EXTERN_ERLANG_BIN_PREFIX)/erl
ERLC=$(EXTERN_ERLANG_BIN_PREFIX)/erlc
else
ERL=$(CACTUS_ROOT)/bin/erl
ERLC=$(CACTUS_ROOT)/bin/erlc
endif
endif
######################################################################################

.PHONY: all _all clean  _cleanall build _buildall rpm _rpmall _erlang

default: build

all: _all
build: _all
buildall: _all

_all: _erlang
	${ESCRIPT} ./rebar get-deps compile
	cd rel && ${ESCRIPT} ../rebar generate


ifeq (${CACTUS_OS}, centos7)
RPMSPEC_SNIPPET_OS_TAG=rhel7
else
RPMSPEC_SNIPPET_OS_TAG=rhelLeq6
endif

rpm: _rpmall
_rpmall: build
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES/{lib,bin},SRPMS}
	cp -r rel/controlhub ${RPMBUILD_SOURCES_DIR}/lib/
	cp scripts/controlhub_* ${RPMBUILD_SOURCES_DIR}/bin
	cp pkg/rsyslog.d.conf ${RPMBUILD_SOURCES_DIR}/ 
	cp pkg/logrotate.d.conf ${RPMBUILD_SOURCES_DIR}/
ifneq ($(CACTUS_OS),centos7)
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
		--define "release ${PACKAGE_RELEASE}.${CACTUS_OS}" \
		--define "packager ${Packager}" \
		${PackageName}.spec



clean: _cleanall
_cleanall:
	${ESCRIPT} ./rebar clean
	rm -rf $(RPMBUILD_DIR)


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

