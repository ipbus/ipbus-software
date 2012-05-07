%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since April 2012
%%%
%%% @doc Test-code for the API of the ch_device_client_registry module.
%%% @end
%%% ===========================================================================

-module(ch_device_client_registry_tests).

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
ch_device_client_registry_test_() ->
    { setup,
      fun setup/0,
      fun teardown/1,
      { inorder,  % run the tests below in order
        [ fun test_get_index/0,
          fun test_total_device_clients/0,
          fun test_get_pid/0,
          fun test_device_client_death_updates_registry/0
        ]
      }
    }.

%% Setup function for test fixture
%% @spec setup() -> ok
setup() ->
    ch_device_client_registry:start_link(),
    ok.

%% Teardown function for test fixture
teardown(_Ignore) -> ch_device_client_registry:stop().



%%% ===========================================================================
%%% Individual Test Functions.
%%% ===========================================================================

% Simple test to get the index table ID and see if it's valid.
test_get_index() ->
    IndexTid = ch_device_client_registry:get_index(),
    % Do somthing with the Tid to make sure valid Tid - test table has correct name
    [Name] = [ X || {name, X} <- ets:info(IndexTid) ],
    ?assertEqual(device_client_index, Name).


% Test getting the total number of registered device clients
test_total_device_clients() ->
    Index = ch_device_client_registry:get_index(),
    ?assertEqual(0, ch_device_client_registry:total_device_clients(Index)),
    ch_device_client_registry:get_pid(Index, 16#00000001, 50001),
    ch_device_client_registry:get_pid(Index, 16#00000002, 50001),
    ch_device_client_registry:get_pid(Index, 16#00000003, 50001),
    ch_device_client_registry:get_pid(Index, 16#00000004, 50001),
    ?assertEqual(4, ch_device_client_registry:total_device_clients(Index)).


% Ask for a pid for a certain target twice - first time a device client should
% get created, second time we should just receive the same device client process.
test_get_pid() ->
    Index = ch_device_client_registry:get_index(),
    NumDeviceClients1 = ch_device_client_registry:total_device_clients(Index),
    TestIPaddrU32 = 16#c0a800ff,
    TestPort = 50001,
    {ok, Pid1} = ch_device_client_registry:get_pid(Index, TestIPaddrU32, TestPort), % New target - should create a new device client
    NumDeviceClients2 = ch_device_client_registry:total_device_clients(Index),
    ?assertEqual(1, NumDeviceClients2 - NumDeviceClients1),
    {ok, Pid2} = ch_device_client_registry:get_pid(Index, TestIPaddrU32, TestPort), % Now an existing target
    ?assertEqual(Pid1, Pid2),
    ?assertEqual(0, ch_device_client_registry:total_device_clients(Index) - NumDeviceClients2).


% Ask for a pid for a certain target, then do a manual kill of the process, and see
% if the device_client_registry successfully traps the exit and updates the index.
test_device_client_death_updates_registry() ->
    Index = ch_device_client_registry:get_index(),
    TestIPaddrU32 = 16#0000dead,
    TestPort = 12345,
    InitialNumClients = ch_device_client_registry:total_device_clients(Index),
    {ok, Pid1} = ch_device_client_registry:get_pid(Index, TestIPaddrU32, TestPort),
    ?assertEqual(InitialNumClients + 1, ch_device_client_registry:total_device_clients(Index)),
    exit(Pid1, kill),
    timer:sleep(1), % sleep briefly to let the magic happen.
    ?assertEqual(InitialNumClients, ch_device_client_registry:total_device_clients(Index)),
    {ok, Pid2} = ch_device_client_registry:get_pid(Index, TestIPaddrU32, TestPort), % ask for device with same addr & port
    ?assert(Pid1 /= Pid2).
    

%%% ==========================================================================
%%% Test Helper Functions
%%% ==========================================================================


