%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since April 2012
%%%
%%% @doc Test-code for the API of the device_client_registry module.
%%% @end
%%% ===========================================================================

-module(device_client_registry_tests).

-include("ch_global.hrl").

%% API exports
-export([]).


%%% ===========================================================================
%%% Test Fixtures
%%% 
%%% The test fixtures, including the lists of tests to be run, and the
%%% setup and teardown functions used by the fixtures.
%%% ===========================================================================

%% Test fixture 
device_client_registry_test_() ->
    { setup,
      fun setup/0,
      fun teardown/1,
      { inparallel,  % run the tests below concurrently, JUST BECAUSE I CAN! (and it's prob a bit quicker)
        [ fun test_get_index/0
        ]
      }
    }.

%% Setup function for test fixture - starts up the ch_stats server
%% @spec setup() -> ok
setup() ->
    device_client_registry:start_link(),
    ok.

%% Teardown function for test fixture
teardown(_Ignore) -> device_client_registry:stop().



%%% ===========================================================================
%%% Individual Test Functions.
%%% ===========================================================================

test_get_index() ->
    _Tid = device_client_registry:get_index().

%%% ==========================================================================
%%% Test Helper Functions
%%% ==========================================================================


