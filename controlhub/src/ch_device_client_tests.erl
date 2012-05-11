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
        fun test_unresponse_target/0
      ]
    }.

%% Setup function for test fixture
%% @spec setup() -> ok
setup() ->
    ch_device_client_registry:start_link(), % needs to be available to spawn the device clients
    DummyHwPid = spawn(fun() -> udp_echo_server(?DUMMY_HW_PORT) end),
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


%% Test multiple processes chucking loads of data at a single device client
%% More of a stress test than unit test...
test_multiple_client_processes() ->
    process_flag(trap_exit, true),
    TotalGenerators = 20,
    spawn_request_generators(TotalGenerators, 100, 100, ?LOCALHOST, ?DUMMY_HW_PORT),
    await_generator_exits(TotalGenerators).


%%% ==========================================================================
%%% Test Helper Functions
%%% ==========================================================================

%% Very simple dummy hardware - just returns whatever it gets sent.
udp_echo_server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    udp_echo_server_loop(Socket).

%% The receive loop for the echo server.
udp_echo_server_loop(Socket) ->
    receive
        {udp, Socket, IP, Port, Packet} ->
            gen_udp:send(Socket, IP, Port, Packet),
            udp_echo_server_loop(Socket);
        die -> ok % For a receiving a clean/normal exit message.
    end.

%% Spawns request generators to test the device client process. Specify the total number you
%% want spawning, the total number of request loops each generator should perform, the maximum
%% number of 32-bit words allowed in the randomly generated packets, and the target hardware's
%% IP address and port.
spawn_request_generators(RemainingToSpawn, TotalIterations, MaxRequestLength, TargetIPaddrU32, TargetPort) ->
    spawn_link(fun() -> request_generator(TotalIterations, MaxRequestLength, TargetIPaddrU32, TargetPort) end),
    spawn_request_generators(RemainingToSpawn - 1, TotalIterations, MaxRequestLength, TargetIPaddrU32, TargetPort);

spawn_request_generators(RemainingToSpawn, TotalIterations, MaxRequestLength, TargetIPaddrU32, TargetPort) ->
    blah.

request_generator(RemainingIterations, MaxRequestLength, TargetIPaddrU32, TargetPort) ->
    blah.

await_generator_exits(Remaining) ->
    receive
        {exit, _Pid, _Why} ->
            await_generator_exits(Remaining-1)
    end;

await_generator_exits(0) -> ok.