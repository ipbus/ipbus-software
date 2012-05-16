%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since May 2012
%%%
%%% @doc Test-code for the API of the ch_device_client module.
%%% @end
%%% ===========================================================================

-module(ch_device_client_tests).

-include("ch_global.hrl").
-include("ch_timeouts.hrl").

%% API exports
-export([]).

-define(DUMMY_HW_PORT, 60011).
-define(LOCALHOST, 16#7f000001).

%%% ===========================================================================
%%% Test Fixtures
%%% 
%%% The test fixtures that contain lists of tests to be run, and the
%%% setup and teardown functions used by the fixtures.
%%% ===========================================================================

%% Test fixture 
ch_device_client_test_() ->
    { setup,
      fun setup/0,
      fun teardown/1,
      [ fun test_normal_operation/0,
        fun test_unresponse_target/0,
        fun test_multiple_client_processes/0
      ]
    }.

%% Setup function for test fixture
%% @spec setup() -> ok
setup() ->
    ch_device_client_registry:start_link(), % needs to be available to spawn the device clients
    DummyHwPid = spawn(fun() -> ch_unittest_common:udp_echo_server(?DUMMY_HW_PORT) end),
    DummyHwPid.

%% Teardown function for test fixture
teardown(DummyHwPid) ->
    ch_device_client_registry:stop(),
    DummyHwPid ! die.


%%% ===========================================================================
%%% Individual Test Functions.
%%% ===========================================================================

%% Queue some "requests", and check we get back what we send.  We rely on the
%% fact that the device client currently doesn't need to look at the actual 
%% IPbus payload, so the dummy hardware can be a simple bounceback server.
test_normal_operation() ->
    TestBin = <<16#deadbeef:32, 16#cafebabe:32, 16#a5a5a5a5:32, 16#69696969:32>>,
    ch_device_client:enqueue_requests(?LOCALHOST, ?DUMMY_HW_PORT, TestBin),
    receive
        {device_client_response, ?LOCALHOST, ?DUMMY_HW_PORT, ErrorCode, Response} ->
            ?assertEqual(0, ErrorCode),
            ?assertEqual(TestBin, Response)
    end.

%% Send some data to a device client that has an unresponsive target. Check
%% to see if the timeout reply mechanism works as expected.
test_unresponse_target() ->
    TestBin = <<16#deadbabe:32, 16#beefcafe:32, 16#01234567:32, 16#89abcdef:32>>,
    BadPort = 59314,  % Hopefully nothing at this port!
    ch_device_client:enqueue_requests(?LOCALHOST, BadPort, TestBin), 
    receive
        {device_client_response, ?LOCALHOST, BadPort, ErrorCode, Response} ->
            ?assertEqual(1, ErrorCode),
            ?assertEqual(<<>>, Response)
    end.    


%% Test multiple processes chucking loads of data at a single device client (and thus single dummy hw)
%% More of a stress test than unit test...
test_multiple_client_processes() ->
    process_flag(trap_exit, true),
    TotalTestClients = 20,
    IterationsPerTestClient = 20,
    MaxRequestSizePerTestClient = 350,
    spawn_test_clients(TotalTestClients, IterationsPerTestClient, MaxRequestSizePerTestClient, ?LOCALHOST, ?DUMMY_HW_PORT),
    ?assertEqual(test_ok, await_test_client_exits(TotalTestClients)).


%%% ==========================================================================
%%% Test Helper Functions
%%% ==========================================================================

%% Spawns request generators to test the device client process. Specify the total number you
%% want spawning, the total number of request loops each generator should perform, the maximum
%% number of 32-bit words allowed in the randomly generated packets, and the target hardware's
%% IP address and port.
spawn_test_clients(RemainingToSpawn, TotalIterations, MaxRequestLength, TargetIPaddrU32, TargetPort) when RemainingToSpawn > 0 ->
    spawn_link(fun() -> test_client(TotalIterations, MaxRequestLength, TargetIPaddrU32, TargetPort) end),
    spawn_test_clients(RemainingToSpawn - 1, TotalIterations, MaxRequestLength, TargetIPaddrU32, TargetPort);

spawn_test_clients(0, _TotalIterations, _MaxRequestLength, _TargetIPaddrU32, _TargetPort) -> ok.

%% Does a bunch of request/receive loops
test_client(RemainingIterations, MaxRequestLength, TargetIPaddrU32, TargetPort) when RemainingIterations > 0 ->
    FakeIPbusRequests = request_data_generator(MaxRequestLength),
    ch_device_client:enqueue_requests(TargetIPaddrU32, TargetPort, FakeIPbusRequests),
    receive
        { device_client_response, TargetIPaddrU32, TargetPort, 0, FakeIPbusRequests } ->
            test_client(RemainingIterations-1, MaxRequestLength, TargetIPaddrU32, TargetPort);
        _Other -> exit(unexpected_response_received)
    after ?RESPONSE_FROM_DEVICE_CLIENT_TIMEOUT ->
        exit(timeout_reached)
    end;

test_client(0, _MaxRequestLength, _TargetIPaddrU32, _TargetPort) -> ok.


request_data_generator(MaxRequestLength) ->
    RequestLengthInBytes = random:uniform(MaxRequestLength) * 4,
    % slightly hacky way to produce random request data.
    list_to_binary([random:uniform(256)-1 || _X <- lists:seq(1, RequestLengthInBytes)]).

% Waits for the given number of test clients to exit, finally returning
await_test_client_exits(Remaining) ->
  await_test_client_exits(Remaining, test_ok).

await_test_client_exits(Remaining, TestStatus) when Remaining > 0 ->
    receive
        {'EXIT', _Pid, normal} ->
            if
                TestStatus == test_ok -> await_test_client_exits(Remaining-1, test_ok);
                true -> await_test_client_exits(Remaining-1, test_fail)
            end;
        {'EXIT', _Pid, _NonNormal} -> await_test_client_exits(Remaining-1, test_fail)
    end;

await_test_client_exits(0, TestStatus) -> TestStatus.

