%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since May 2012
%%%
%%% @doc Test-code for the API of the ch_tcp_listener module.
%%% @end
%%% ===========================================================================

-module(ch_tcp_listener_tests).

-include("ch_global.hrl").
-include("ch_tcp_server_params.hrl").

%% API exports
-export([]).


%%% ===========================================================================
%%% Test Fixtures
%%% 
%%% The test fixtures that contain lists of tests to be run, and the
%%% setup and teardown functions used by the fixtures.
%%% ===========================================================================

%% Test fixture 
ch_tcp_listener_test_() ->
    { setup,
      fun setup/0,
      fun teardown/1,
      [ fun test_tcp_connect/0
      ]
    }.

%% Setup function for test fixture
%% @spec setup() -> ok
setup() ->
    ch_stats:start_link(), % This will help us test
    ch_tcp_listener:start_link(),
    ok.

%% Teardown function for test fixture
teardown(_Ignore) ->
    ch_stats:stop(),
    ch_tcp_listener:stop(),
    % This sucks, but it's just to prevent the standard IO going down before
    % any io:format messages that report shutdowns might have completed. This
    % can cause misleading error logger reports during unit-testing.
    timer:sleep(10).  



%%% ===========================================================================
%%% Individual Test Functions.
%%% ===========================================================================

test_tcp_connect() ->    
    % Need to improve this... currently relies on the stats server to prove it's doing the right thing.
    % Create a whole bunch of dummy client connections with a small timeout
    Timeout_ms = 50, % 50 millisecond timeout
    {ok, TestSocket1} = gen_tcp:connect("localhost", ?CONTROL_HUB_TCP_LISTEN_PORT, [binary, {packet, 4}], Timeout_ms),
    {ok, TestSocket2} = gen_tcp:connect("localhost", ?CONTROL_HUB_TCP_LISTEN_PORT, [binary, {packet, 4}], Timeout_ms),
    {ok, TestSocket3} = gen_tcp:connect("localhost", ?CONTROL_HUB_TCP_LISTEN_PORT, [binary, {packet, 4}], Timeout_ms),
    {ok, TestSocket4} = gen_tcp:connect("localhost", ?CONTROL_HUB_TCP_LISTEN_PORT, [binary, {packet, 4}], Timeout_ms),
    timer:sleep(10), % sleep briefly to make sure the stats server becomes up to date.
    ?assertEqual(4, ch_stats:get_active_clients()),
    gen_tcp:close(TestSocket4),
    gen_tcp:close(TestSocket3),
    gen_tcp:close(TestSocket2),
    gen_tcp:close(TestSocket1),
    timer:sleep(10), % sleep briefly to make sure the stats server becomes up to date.
    ?assertEqual(4, ch_stats:get_max_active_clients()),
    ?assertEqual(0, ch_stats:get_active_clients()).

    
        
    