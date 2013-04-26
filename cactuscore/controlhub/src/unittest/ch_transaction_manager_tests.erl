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
        [ fun test_normal_operation_single_req_single_target/1,
          fun test_normal_operation_multi_req_single_target/1,
          fun test_normal_operation_same_req_multi_targets/1,
          fun test_normal_operation_multi_req_multi_targets/1,
          fun ignore_bad_data_non_integer_number_of_words/1
        ]
      }
    }.


%% @doc Setup function for test fixture
%% @spec setup() -> Baggage::test_baggage()
%% @end

setup() ->
    ch_device_client_registry:start_link(), % needs to be available to spawn the device clients.
    ch_stats:start_link(), % Needed to prove the control hub is ignoring malformed requests.
    % Spawn the dummy hardware
    DummyHwPidList = lists:map(fun({A,B}) -> ch_unittest_common:spawn_device_emulator(A, B) end, [{{1,3},X} || X <- ?DUMMY_HW_PORTS_LIST]),
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

%% Tests the sending of a single request to a single target device
test_normal_operation_single_req_single_target(TestBaggage) ->
    % Dummy IPbus request data - only going to echo server, so no need for real IPbus content.
    DummyIPbusRequest = << 16#deadbeef:32,
                           16#cafebabe:32,
                           16#a5a5a5a5:32,
                           16#69696969:32 >>,
    % Take the above, but prepend with target address, etc, as would be sent by uHAL.
    TestRequest = << ?LOCALHOST:32,
                     (lists:nth(1, ?DUMMY_HW_PORTS_LIST)):16, (size(DummyIPbusRequest) div 4):16,
                     DummyIPbusRequest/binary >>,
    % The response we expect back
    ExpectedResponse = << (size(DummyIPbusRequest) +8):32,
                          ?LOCALHOST:32,
                          (lists:nth(1, ?DUMMY_HW_PORTS_LIST)):16, ?ERRCODE_SUCCESS:16,
                          DummyIPbusRequest/binary >>,
    % Run the test
    ReceivedResponse = create_send_receive(TestBaggage#test_baggage.listen_socket, TestRequest),
    ?assertEqual(ExpectedResponse, ReceivedResponse).
    

%% Tests the sending of multiple requests to a single target device
test_normal_operation_multi_req_single_target(TestBaggage) ->
    % The dummy content of our three requests - only going to echo server, so no need for real IPbus content.
    DummyIPbusRequest1 = << 16#01010101:32 >>,
    DummyIPbusRequest2 = << 16#02020202:32, 16#03030303:32 >>,
    DummyIPbusRequest3 = << 16#04040404:32, 16#05050505:32, 16#06060606:32 >>,    
    % Build the test request packets
    TestRequests = [<< ?LOCALHOST:32,
                      (lists:nth(1, ?DUMMY_HW_PORTS_LIST)):16, (size(DummyIPbusRequest1) div 4):16,
                      DummyIPbusRequest1/binary >>,
                    << ?LOCALHOST:32,
                      (lists:nth(1, ?DUMMY_HW_PORTS_LIST)):16, (size(DummyIPbusRequest2) div 4):16,
                      DummyIPbusRequest2/binary >>,
                    << ?LOCALHOST:32,
                       (lists:nth(1, ?DUMMY_HW_PORTS_LIST)):16, (size(DummyIPbusRequest3) div 4):16,
                       DummyIPbusRequest3/binary >>],
    % The response we expect back
    ExpectedResponses = [<< (size(DummyIPbusRequest1) +8):32,
                            ?LOCALHOST:32,
                            (lists:nth(1, ?DUMMY_HW_PORTS_LIST)):16, ?ERRCODE_SUCCESS:16,
                            DummyIPbusRequest1/binary >>,
                         << (size(DummyIPbusRequest2) +8):32,
                            ?LOCALHOST:32,
                            (lists:nth(1, ?DUMMY_HW_PORTS_LIST)):16, ?ERRCODE_SUCCESS:16,
                            DummyIPbusRequest2/binary >>,
                         << (size(DummyIPbusRequest3) +8):32,
                            ?LOCALHOST:32,
                            (lists:nth(1, ?DUMMY_HW_PORTS_LIST)):16, ?ERRCODE_SUCCESS:16,
                            DummyIPbusRequest3/binary >>],
    % Run the test
    ReceivedResponses = create_send_receive(TestBaggage#test_baggage.listen_socket, TestRequests),
    ?assertEqual(ExpectedResponses, ReceivedResponses).


%% Tests the sending of the same request to multiple target devices
test_normal_operation_same_req_multi_targets(TestBaggage) ->
    % Dummy IPbus request data - only going to echo server, so no need for real IPbus content.
    DummyIPbusRequest = << 16#deadbeef:32,
                           16#cafebabe:32,
                           16#a5a5a5a5:32,
                           16#69696969:32 >>,
    % Build the test requests
    TestRequests = [<< ?LOCALHOST:32,
                       (lists:nth(1, ?DUMMY_HW_PORTS_LIST)):16, (size(DummyIPbusRequest) div 4):16,
                       DummyIPbusRequest/binary >>,
                    << ?LOCALHOST:32,
                       (lists:nth(2, ?DUMMY_HW_PORTS_LIST)):16, (size(DummyIPbusRequest) div 4):16,
                       DummyIPbusRequest/binary >>,
                    << ?LOCALHOST:32,
                       (lists:nth(3, ?DUMMY_HW_PORTS_LIST)):16, (size(DummyIPbusRequest) div 4):16,
                       DummyIPbusRequest/binary >>],
    % The responses we expect back
    ExpectedResponses = [<< (size(DummyIPbusRequest) +8):32,
                            ?LOCALHOST:32,
                            (lists:nth(1, ?DUMMY_HW_PORTS_LIST)):16, ?ERRCODE_SUCCESS:16,
                            DummyIPbusRequest/binary >>,
                         << (size(DummyIPbusRequest) +8):32,
                            ?LOCALHOST:32,
                            (lists:nth(2, ?DUMMY_HW_PORTS_LIST)):16, ?ERRCODE_SUCCESS:16,
                            DummyIPbusRequest/binary >>,
                         << (size(DummyIPbusRequest) +8):32,
                            ?LOCALHOST:32,
                            (lists:nth(3, ?DUMMY_HW_PORTS_LIST)):16, ?ERRCODE_SUCCESS:16,
                            DummyIPbusRequest/binary >>],
    % Run the test
    ReceivedResponses = create_send_receive(TestBaggage#test_baggage.listen_socket, TestRequests),
    ?assertEqual(ExpectedResponses, ReceivedResponses).


%% Tests the sending multiple different requests to multiple target devices (from one TCP client)
test_normal_operation_multi_req_multi_targets(TestBaggage) ->
    HwPort1 = lists:nth(1, ?DUMMY_HW_PORTS_LIST),
    HwPort2 = lists:nth(2, ?DUMMY_HW_PORTS_LIST),
    HwPort3 = lists:nth(3, ?DUMMY_HW_PORTS_LIST),
    % The dummy content of our requests - only going to echo server, so no need for real IPbus content.
    HwPortsRequestsList = [{HwPort1, << 16#01010001:32 >>},
                           {HwPort2, << 16#02010001:32 >>},
                           {HwPort3, << 16#03010001:32 >>},
                           {HwPort1, << 16#01020001:32, 16#01020002:32 >>},
                           {HwPort2, << 16#02020001:32, 16#02020002:32 >>},
                           {HwPort3, << 16#03020001:32, 16#03020002:32 >>},
                           {HwPort1, << 16#01030001:32, 16#01030002:32, 16#01030003:32 >>},
                           {HwPort2, << 16#02030001:32, 16#02030002:32, 16#02030003:32 >>},
                           {HwPort3, << 16#03030001:32, 16#03030002:32, 16#03030003:32 >>}],
    % Build the test request
    TestRequests = [ << ?LOCALHOST:32,
                        HwPort:16, (size(Request) div 4):16,
                        Request/binary >> || {HwPort, Request} <- HwPortsRequestsList ],
    % The response we expect back
    ExpectedResponses = [ << (size(IPbusRequest) +8):32,
                             ?LOCALHOST:32,
                             HwPort:16, ?ERRCODE_SUCCESS:16,
                             IPbusRequest/binary >> || {HwPort, IPbusRequest} <- HwPortsRequestsList ],
    % Run the test
    ReceivedResponses = create_send_receive(TestBaggage#test_baggage.listen_socket, TestRequests),
    ?assertEqual(ExpectedResponses, ReceivedResponses).


% Tests to see if the transaction manager ignores requests
% that are a non-integer number of 32-bit words.
ignore_bad_data_non_integer_number_of_words(TestBaggage) ->
    Common = <<?LOCALHOST:32,
               (lists:nth(1, ?DUMMY_HW_PORTS_LIST)):16, 2:16,  % claiming there are 2 IPbus words to follow   
               16#01020304:32 >>,
    Request_104bits = << Common/binary, 16#05:8>>,
    Request_112bits = << Common/binary, 16#0506:16>>,
    Request_120bits = << Common/binary, 16#050607:24>>,
    ?assertEqual(no_response, create_send_receive(TestBaggage#test_baggage.listen_socket, Request_104bits)),
    ?assertEqual(no_response, create_send_receive(TestBaggage#test_baggage.listen_socket, Request_112bits)),
    ?assertEqual(no_response, create_send_receive(TestBaggage#test_baggage.listen_socket, Request_120bits)).
    

%%% ==========================================================================
%%% Test Helper Functions
%%% ==========================================================================

% Creates the transaction manager under test, sends one or more test requests to it, receives
% and returns the response binary(s).  If there is no response before the timeout is
% reached, it returns the atom no_response
% @spec create_send_receive(TcpListenSocket::socket(), TestRequests) -> Responses
%         TestRequests = binary() | [binary()]
%         Responses    = Response | [Response]
%         Response     = binary() | no_response
create_send_receive(TcpListenSocket, TestRequest) when is_binary(TestRequest) ->
    lists:nth(1, create_send_receive(TcpListenSocket, [TestRequest]) );
create_send_receive(TcpListenSocket, TestRequests) when is_list(TestRequests) ->
    ch_transaction_manager:start_link(TcpListenSocket),
    % Connect to the transaction manager
    {ok, ClientSocket} = gen_tcp:connect("localhost", ?CONTROL_HUB_TCP_LISTEN_PORT, [binary, {packet, 4}]),
    ReceivedResponses = [send_receive(ClientSocket, Request) || Request <- TestRequests],
    gen_tcp:close(ClientSocket),
    % Wait for the transaction manager to shut down normally. Can cause problems in debug
    % trace mode otherwise (process gets killed whilst debug stdout is occurring). Need
    % to find a better way around this problem...
    timer:sleep(10),
    ReceivedResponses.

% Sends a test request to ControlHub, then waits for and returns the response binary.
% @spec send_receive(Socket::socket(), RequestPacket::binary()) -> Response::binary() | no_response
send_receive(Socket, RequestPacket) ->
    ok = gen_tcp:send(Socket, RequestPacket),
    receive
        {tcp, Socket, Bin} -> Bin
    after 10 ->
        no_response
    end.

