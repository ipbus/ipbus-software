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
    DummyHwPid = spawn(fun() -> bounceback_simple_dummy_hw(?DUMMY_HW_PORT) end),
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



%%% ==========================================================================
%%% Test Helper Functions
%%% ==========================================================================

%% Very simple dummy hardware that simply sends back whatever it receives
%% over UDP.  Simply spawn a process that starts with this function and
%% specify the port for it to listen on.
bounceback_simple_dummy_hw(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    bounceback_recv_send_loop(Socket).

bounceback_recv_send_loop(Socket) ->
    receive
        {udp, Socket, IP, Port, Packet} ->
            gen_udp:send(Socket, IP, Port, Packet),
            bounceback_recv_send_loop(Socket);
        die -> ok % For a receiving a clean/normal exit message.
    end.
