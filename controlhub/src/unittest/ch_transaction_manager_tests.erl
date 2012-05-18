%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since May 2012
%%%
%%% @doc Test-code for the API of the ch_transaction_manager module.
%%% @end
%%% ===========================================================================

-module(ch_transaction_manager_tests).

-include("ch_global.hrl").
-include("ch_tcp_server_params.hrl").

%% API exports
-export([]).

-define(DUMMY_HW_PORTS_LIST, [60011, 60012, 60013]).
-define(LOCALHOST, 16#7f000001).

%%% ===========================================================================
%%% Test Fixtures
%%% 
%%% The test fixtures that contain lists of tests to be run, and the
%%% setup and teardown functions used by the fixtures.
%%% ===========================================================================

%% Test fixture 
ch_transaction_manager_test_() ->
    { setup,
      fun setup/0,
      fun teardown/1,
      { inparallel,
        [ fun test_normal_operation_single_req_single_target/0
        ]
      }
    }.

%% Setup function for test fixture
%% @spec setup() -> [DummyHw1Pid::pid(), DummyHw2Pid::pid()]
setup() ->
    ch_device_client_registry:start_link(), % needs to be available to spawn the device clients
    % Spawn the dummy hardware
    DummyHwPidList = lists:map(fun ch_unittest_common:spawn_udp_echo_server/1, ?DUMMY_HW_PORTS_LIST),
    DummyHwPidList.

%% Teardown function for test fixture
teardown(DummyHwPidList) ->
    ch_device_client_registry:stop(),
    lists:foreach(fun(Pid) -> Pid ! shutdown end, DummyHwPidList). % Shutdown all dummy hw.


%%% ===========================================================================
%%% Individual Test Functions.
%%% ===========================================================================

%% Tests the sending ofs a single request to a single target device
test_normal_operation_single_req_single_target() ->
    ok.
    %TestRequest = <<16#
    %TestBin = <<16#deadbeef:32, 16#cafebabe:32, 16#a5a5a5a5:32, 16#69696969:32>>,
    %ch_device_client:enqueue_requests(?LOCALHOST, ?DUMMY_HW_PORT, TestBin),
    %receive
    %    {device_client_response, ?LOCALHOST, ?DUMMY_HW_PORT, ErrorCode, Response} ->
    %        ?assertEqual(0, ErrorCode),
    %        ?assertEqual(TestBin, Response)
    %end.
