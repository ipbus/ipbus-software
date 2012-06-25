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


CACTUS_ROOT = $(shell pwd)/..

include $(CACTUS_ROOT)/config/Makefile.macros

Package = controlhub
PackagePath = $(CACTUS_ROOT)/${Package}
PackageName = cactus-controlhub

Packager = Robert Frazier

PACKAGE_VER_MAJOR = 1
PACKAGE_VER_MINOR = 1
PACKAGE_VER_PATCH = 0

# This is the version number for the RPM packaging.
PACKAGE_RELEASE = 1

# A tarball is generated as part of the Erlang release-building mechanism
TARBALL_FILE = controlhub.tar.gz

RPMBUILD_DIR = ${PackagePath}/RPMBUILD


####################  Define path to erlang runtime and compiler  ####################
UNAME := $(shell uname)
# Allows me to still develop on Mac OS X with a standard Erlang install
ifeq ($(UNAME), Darwin)
ERL=erl
ERLC=erlc
else
#If we're on Linux, use the cactus-extern-erlang RPM sources or RPM installation
ifeq ($(shell test -d ${EXTERN_ERLANG_BIN_PREFIX} && echo OK),OK)
ERL=$(EXTERN_ERLANG_BIN_PREFIX)/erl
ERLC=$(EXTERN_ERLANG_BIN_PREFIX)/erlc
else
ERL=$(INSTALL_PREFIX)/bin/erl
ERLC=$(INSTALL_PREFIX)/bin/erlc
endif
endif
######################################################################################

ERLC_FLAGS=

INCLUDE_DIR = include
INCLUDES = $(wildcard $(INCLUDE_DIR)/*.hrl)

SOURCES = $(wildcard src/*.erl)
TEST_SOURCES = $(wildcard src/unittest/*.erl)

OBJECTS = $(SOURCES:src/%.erl=ebin/%.beam)
TEST_OBJECTS = $(TEST_SOURCES:src/unittest/%.erl=ebin/unittest/%.beam)

CONTROLHUB_RELEASE_FILES = controlhub.rel controlhub.boot $(TARBALL_FILE)


.PHONY: all _all clean  _cleanall build _buildall install _installall rpm _rpmall test _testall spec_update

.PHONY: dev _check_test_build_dir

default: build

all: _all
build: _all
buildall: _all

_all: $(OBJECTS) $(CONTROLHUB_RELEASE_FILES)

dev: $(OBJECTS) _check_test_build_dir $(TEST_OBJECTS) test

rpm: _rpmall
_rpmall:
	mkdir -p ${RPMBUILD_DIR}/{RPMS/{i386,i586,i686,x86_64},SPECS,BUILD,SOURCES,SRPMS}
	tar -xvzf ${TARBALL_FILE} -C ${RPMBUILD_DIR}/SOURCES | sed 's/^/extracted to RPMBUILD\/SOURCES: /'
	rpmbuild -bb -bl --buildroot=${RPMBUILD_DIR}/BUILD \
		--define  "_topdir ${RPMBUILD_DIR}" \
		--define "_prefix ${INSTALL_PREFIX}" \
		--define "tarball_file ${TARBALL_FILE}" \
		--define "sources_dir ${RPMBUILD_DIR}/SOURCES" \
		--define "name ${PackageName}" \
		--define "version ${PACKAGE_VER_MAJOR}.${PACKAGE_VER_MINOR}.${PACKAGE_VER_PATCH}" \
		--define "release ${PACKAGE_RELEASE}" \
		--define "packager ${Packager}" \
		${PackageName}.spec


clean: _cleanall
_cleanall:
	rm -rf $(RPMBUILD_DIR)
	rm -f ebin/*.beam
	rm -rf ebin/unittest
	rm -f controlhub.rel
	rm -f controlhub.boot
	rm -f controlhub.script
	rm -f controlhub.tar.gz


############## Nitty gritty ######################

_rpmdirs:


ebin/unittest/%.beam : src/unittest/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_FLAGS) -I $(INCLUDE_DIR) -o ebin/unittest/ $<

ebin/%.beam : src/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_FLAGS) -I $(INCLUDE_DIR) -o ebin/ $<


_check_test_build_dir:
	test -d ebin/unittest || mkdir ebin/unittest

test:
	$(ERL) -noshell -pa ebin ebin/unittest -eval 'eunit:test("ebin",[verbose])' -s init stop



$(TARBALL_FILE): controlhub.boot controlhub.rel Makefile
	$(ERL) -pa ebin -noshell +B -eval \
	 'case systools:make_tar("controlhub", []) of ok -> halt(0); _ -> halt(1) end.'

# Auto-generate the release boot and script files
controlhub.boot: controlhub.rel Makefile
	$(ERL) -pa ebin -noshell +B -eval \
	 'case systools:make_script("controlhub",[local]) of ok -> halt(0); _ -> halt(1) end.'

# Auto-generate the control.rel file.
controlhub.rel: ebin/controlhub.app Makefile
	$(ERL) -pa ebin -noshell +B -eval \
	 "ok = application:load(controlhub), \
	 {ok, ControlHubVsn} = application:get_key(controlhub, vsn), \
	 {ok, Apps} = application:get_key(controlhub, applications), \
	 {ok, F} = file:open("$@", [write]), \
	 io:format(F, \"~p.~n\", [{release, {\"controlhub\", ControlHubVsn}, \
	 {erts, erlang:system_info(version)}, \
	 lists:map(fun (App) -> application:load(App), \
	 {ok, Vsn} = application:get_key(App, vsn), \
         {App, Vsn} end, Apps ++ [controlhub])}]), \
	 file:close(F), halt(0)."
