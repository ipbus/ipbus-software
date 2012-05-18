%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since May 2012
%%%
%%% @doc Test-code for the API of the ch_transaction_manager module.
%%% @end
%%% ===========================================================================

-module(ch_transaction_manager_tests).

%% Includes
-include("ch_global.hrl").
-include("ch_tcp_server_params.hrl").
-include("ch_error_codes.hrl").

%% API exports
-export([]).

-define(DUMMY_HW_PORTS_LIST, [60011, 60012, 60013]).
-define(LOCALHOST, 16#7f000001).

% A record to hold stuff we need to teardown at the end or stuff
% needed by the tests themselves.
-record(test_baggage, {listen_socket, dummy_hw_pid_list}).

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
      { with,
        [ fun test_normal_operation_single_req_single_target/1
        ]
      }
    }.

%% Setup function for test fixture
%% @spec setup() -> {test_inits, {dummy_hw, [DummyHw1Pid::pid(), ...]}, {
setup() ->
    ch_device_client_registry:start_link(), % needs to be available to spawn the device clients.
    ch_stats:start_link(), % Needed to prove the control hub is ignoring malformed requests.
    % Spawn the dummy hardware
    DummyHwPidList = lists:map(fun ch_unittest_common:spawn_udp_echo_server/1, ?DUMMY_HW_PORTS_LIST),
    {ok, TcpListenSocket} = gen_tcp:listen(?CONTROL_HUB_TCP_LISTEN_PORT, ?TCP_SOCKET_OPTIONS),
    TestBaggage = #test_baggage{listen_socket=TcpListenSocket, dummy_hw_pid_list=DummyHwPidList},
    TestBaggage.

%% Teardown function for test fixture
teardown(TestBaggage) ->
    ch_device_client_registry:stop(),
    ch_stats:stop(),
    lists:foreach(fun(Pid) -> Pid ! shutdown end, TestBaggage#test_baggage.dummy_hw_pid_list), % Shutdown all dummy hw.
    gen_tcp:close(TestBaggage#test_baggage.listen_socket).


%%% ===========================================================================
%%% Individual Test Functions.
%%% ===========================================================================

%% Tests the sending ofs a single request to a single target device
test_normal_operation_single_req_single_target(TestBaggage) ->
    % Dummy IPbus request data only going to echo server
    DummyIPbusRequest = << 16#deadbeef:32,
                           16#cafebabe:32,
                           16#a5a5a5a5:32,
                           16#69696969:32 >>,
    % Take the above, but prepend with target address, etc, as would be sent by uHAL.
    TestRequest = << ?LOCALHOST:32,
                     (lists:nth(1, ?DUMMY_HW_PORTS_LIST)):16, (size(DummyIPbusRequest) div 4):16,
                     DummyIPbusRequest/binary >>,
    % The response we expect back

    ExpectedResponse = << ?LOCALHOST:32,
                          (lists:nth(1, ?DUMMY_HW_PORTS_LIST)):16, ?ERRCODE_SUCCESS:16,
                          DummyIPbusRequest/binary >>,
    % Create the transaction manager process we're going to test:
    
    ch_transaction_manager:start_link(TestBaggage#test_baggage.listen_socket),
    % Connect to the transaction manager
    {ok, ClientSocket} = gen_tcp:connect("localhost", ?CONTROL_HUB_TCP_LISTEN_PORT, [binary, {packet, 4}]),
    ok = gen_tcp:send(ClientSocket, TestRequest),
    ReceivedBin = receive
                      {tcp, ClientSocket, Bin} -> Bin
                  end,
    ?assertEqual(ExpectedResponse, ReceivedBin).
    
    