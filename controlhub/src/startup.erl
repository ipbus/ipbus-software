%% Author: rob
%% Created: Dec 10, 2010
%% Description: TODO: Add description to control_hub_host
-module(startup).



%% Exported Functions
-export([cmd_line_start/0, start/0]).


%%% ====================================================================
%%% API functions (public interface)
%%% ====================================================================

%% @spec start(DeviceAddrFile) -> {ok, started} | {error, What}

cmd_line_start() ->
    start(),
    % Now we just block indefinitely, otherwise the process will
    % finish and terminate all the Control Hub subprocesses
    timer:sleep(infinity).


start() ->   
    
    io:format("Control Hub v2.0.0 starting...~n"),
    
    % Start the Control Hub Stats server
    ch_stats:start_link(),
    
    % Start the Device Client Registry
    device_client_registry:start_link(),
    
    % Start TCP front-end transaction managers.
    transaction_manager:start_transaction_manager_tcp_listen_pool(),
    
    io:format("...Control Hub started successfully.~n").



%%
%% Local Functions
%%

    
    