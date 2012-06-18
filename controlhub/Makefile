###################################################################################
#
# Simple Erlang build file with testing built-in.
# Inspiration taken from an erlang-factory.com presentation by Richard Carlsson 
#
#   Robert Frazier, March 2012
#
###################################################################################

BUILD_HOME = $(shell pwd)/..

include $(BUILD_HOME)/config/Makefile.macros

ERL=$(EXTERN_ERLANG_BIN_PREFIX)/erl

ERLC=$(EXTERN_ERLANG_BIN_PREFIX)/erlc

ERLC_FLAGS=

INCLUDE_DIR=include
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)

SOURCES=$(wildcard src/*.erl)
TEST_SOURCES=$(wildcard src/unittest/*.erl)

OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)
TEST_OBJECTS=$(TEST_SOURCES:src/unittest/%.erl=ebin/unittest/%.beam)

.PHONY: all _all build_controlhub build_and_run_tests check_test_build_dir clean  _cleanall test install _installall rpm _rpmall

default: _all
build: _all
buildall: _all
all: _all
_all: build_controlhub build_and_run_tests

build_controlhub: $(OBJECTS)

build_and_run_tests: check_test_build_dir $(TEST_OBJECTS) test

ebin/unittest/%.beam : src/unittest/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_FLAGS) -I $(INCLUDE_DIR) -o ebin/unittest/ $<

ebin/%.beam : src/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_FLAGS) -I $(INCLUDE_DIR) -o ebin/ $<

clean: _cleanall
_cleanall:
	rm -f ebin/*.beam
	rm -rf ebin/unittest

check_test_build_dir:
	test -d ebin/unittest || mkdir ebin/unittest

test:
	$(ERL) -noshell -pa ebin ebin/unittest -eval 'eunit:test("ebin",[verbose])' -s init stop

release: clean build_controlhub
