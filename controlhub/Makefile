###################################################################################
#
# Simple Erlang build file with testing built-in.
# Inspiration taken from an erlang-factory.com presentation by Richard Carlsson 
#
#   Robert Frazier, March 2012
#
###################################################################################


ERLC_FLAGS=

INCLUDE_DIR=include
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)

SOURCES=$(wildcard src/*.erl)
TEST_SOURCES=$(wildcard src/unittest/*.erl)

OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)
TEST_OBJECTS=$(TEST_SOURCES:src/unittest/%.erl=ebin/unittest/%.beam)

all: controlhub build_and_run_tests

controlhub: $(OBJECTS)

build_and_run_tests: check_test_build_dir $(TEST_OBJECTS) test

ebin/unittest/%.beam : src/unittest/%.erl $(INCLUDES)
	erlc $(ERLC_FLAGS) -I $(INCLUDE_DIR) -o ebin/unittest/ $<

ebin/%.beam : src/%.erl $(INCLUDES)
	erlc $(ERLC_FLAGS) -I $(INCLUDE_DIR) -o ebin/ $<

clean: 
	rm -f ebin/*.beam
	rm -rf ebin/unittest

check_test_build_dir:
	test -d ebin/unittest || mkdir ebin/unittest

test:
	erl -noshell -pa ebin ebin/unittest -eval 'eunit:test("ebin",[verbose])' -s init stop

release: clean controlhub
