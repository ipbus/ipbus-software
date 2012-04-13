%% Author: rob
%% Created: Dec 10, 2010
%% Description: TODO: Add description to control_hub_host
-module(startup).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([cmd_line_start/1, start/1]).

%%
%% API Functions
%%

%% @spec start(DeviceAddrFile) -> {ok, started} | {error, What}

cmd_line_start(DeviceAddrFile) when is_list(DeviceAddrFile) ->
    start(DeviceAddrFile),
    % Now we just block indefinitely, otherwise the process will
    % finish and terminate all the Control Hub subprocesses
    receive indefinite_wait -> true end.


start(DeviceAddrFile) when is_list(DeviceAddrFile) ->   
    
    io:format("Control Hub v1.0.0 starting...~n"),
    
    % Start packet statistics server - this just keeps track of incoming + outgoing TCP/UDP
    % packets and also counts any malformed packets that are otherwised ignored.
    ch_stats:start_link(),
    
    % Start the Device Client Index - this does two things as follows:  Firstly, as per the Board Address File,
    % it will create a Device Client process for each declared board in the system. Each Device Client will
    % then negotiate all future communication to and from the board it is linked to.  Secondly, the PID
    % server will keep track of the PIDs for each Device Client, and when given a Device ID number (as per the
    % Device Address File), it will return a PID that can be used to access the appropriate Device Client.
    device_client_index:start(DeviceAddrFile),
    
    % Start TCP front-end transaction managers.
    transaction_manager:start_transaction_manager_tcp_listen_pool(),
    
    io:format("...Control Hub started successfully.~n").



%%
%% Local Functions
%%

    
    