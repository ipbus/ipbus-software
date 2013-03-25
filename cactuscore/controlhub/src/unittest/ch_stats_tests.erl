%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since April 2012
%%%
%%% @doc Test-code for the API of the ch_stats module.
%%% @end
%%% ===========================================================================

-module(ch_stats_tests).

-include("ch_global.hrl").

%% API exports
-export([]).


%%% ===========================================================================
%%% Test Fixtures
%%% 
%%% The test fixtures that contain lists of tests to be run, and the
%%% setup and teardown functions used by the fixtures.
%%% ===========================================================================

%% Test fixture 
ch_stats_test_() ->
    { setup,
      fun setup/0,
      fun teardown/1,
      { inorder,
        [ fun test_client_connections/0,
          fun test_client_request_in/0,
          fun test_client_request_malformed/0,
          fun test_client_response_sent/0,
          fun test_udp_in/0,
          fun test_udp_malformed/0,
          fun test_udp_out/0,
          fun test_udp_response_timeout/0,
          fun test_report_to_string/0
        ]
      }
    }.

%% Setup function for test fixture - starts up the ch_stats server
%% @spec setup() -> ok
setup() ->
    ch_stats:start_link(),
    ok.

%% Teardown function for test fixture
teardown(_Ignore) -> ch_stats:stop().



%%% ===========================================================================
%%% Individual Test Functions.
%%% ===========================================================================

test_client_connections() ->
    ?assertEqual(0, ch_stats:get_active_clients()),
    ?assertEqual(0, ch_stats:get_max_active_clients()),
    ?assertEqual(0, ch_stats:get_cumulative_client_count()),
    n_call(7, fun ch_stats:client_connected/0),
    ?assertEqual(7, ch_stats:get_active_clients()),
    ?assertEqual(7, ch_stats:get_max_active_clients()),
    ?assertEqual(7, ch_stats:get_cumulative_client_count()),
    n_call(5, fun ch_stats:client_disconnected/0),
    ?assertEqual(2, ch_stats:get_active_clients()),
    ?assertEqual(7, ch_stats:get_max_active_clients()),
    ?assertEqual(7, ch_stats:get_cumulative_client_count()),
    n_call(10, fun ch_stats:client_connected/0),
    ?assertEqual(12, ch_stats:get_active_clients()),
    ?assertEqual(12, ch_stats:get_max_active_clients()),
    ?assertEqual(17, ch_stats:get_cumulative_client_count()).

test_client_request_in() ->
    ?assertEqual(0, ch_stats:get_total_client_requests()),
    n_call(20, fun ch_stats:client_request_in/0),
    ?assertEqual(20, ch_stats:get_total_client_requests()).

test_client_request_malformed() ->
    ?assertEqual(0, ch_stats:get_total_client_malformed_requests()),
    n_call(13, fun ch_stats:client_request_malformed/0),
    ?assertEqual(13, ch_stats:get_total_client_malformed_requests()).

test_client_response_sent() ->
    ?assertEqual(0, ch_stats:get_total_client_responses()),
    n_call(7, fun ch_stats:client_response_sent/0),
    ?assertEqual(7, ch_stats:get_total_client_responses()).

test_udp_in() ->
    ?assertEqual(0, ch_stats:get_udp_in()),
    n_call(19, fun ch_stats:udp_in/0),
    ?assertEqual(19, ch_stats:get_udp_in()).

test_udp_malformed() ->
    ?assertEqual(0, ch_stats:get_udp_malformed()),
    n_call(3, fun ch_stats:udp_malformed/0),
    ?assertEqual(3, ch_stats:get_udp_malformed()).

test_udp_out() ->
    ?assertEqual(0, ch_stats:get_udp_out()),
    n_call(29, fun ch_stats:udp_out/0),
    ?assertEqual(29, ch_stats:get_udp_out()).

test_udp_response_timeout() ->
    ?assertEqual(0, ch_stats:get_udp_response_timeouts(normal)),
    ?assertEqual(0, ch_stats:get_udp_response_timeouts(recovered)),
    ?assertEqual(0, ch_stats:get_udp_response_timeouts(status)),
    ?assertEqual(0, ch_stats:get_udp_response_timeouts(resend)),

    n_call(31, fun () -> ch_stats:udp_response_timeout(normal) end),
    n_call(25, fun () -> ch_stats:udp_response_timeout(recovered) end),
    n_call(5,  fun () -> ch_stats:udp_response_timeout(status) end),
    n_call(3,  fun () -> ch_stats:udp_response_timeout(resend) end),

    ?assertEqual(31, ch_stats:get_udp_response_timeouts(normal)),
    ?assertEqual(25, ch_stats:get_udp_response_timeouts(recovered)),
    ?assertEqual(5,  ch_stats:get_udp_response_timeouts(status)),
    ?assertEqual(3,  ch_stats:get_udp_response_timeouts(resend)).

test_report_to_string() ->
    StatsReportString = ch_stats:report_to_string(),
    ExpectedResult = "Control Hub Stats Report\n"
                     "------------------------\n\n"
                     "CLIENT  All-time connections: 17\n"  
                     "          Active connections: 12 (peak: 12)\n"
                     "           Requests received: 20 (of which 13 were malformed)\n"
                     "              Responses sent: 7\n\n"
                     "UDP              Packets Out: 29\n"
                     "                  Packets In: 19 (of which 3 were malformed)\n"
                     "                    Timeouts: 31 in normal operation (of which 25 were recovered)\n"
                     "              Other timeouts: 5 (status), 3 (in resends)\n",
    ?assertEqual(ExpectedResult, StatsReportString).

%%% ==========================================================================
%%% Test Helper Functions
%%% ==========================================================================

%% Call some function N times.
n_call(N, Fun) -> n_call(0, N, Fun).

%% Implements n_call/2
n_call(_N, _N, _Fun) -> ok;

n_call(X, N, Fun) ->
    Fun(),
    n_call(X+1, N, Fun).
