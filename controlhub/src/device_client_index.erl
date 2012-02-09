%%% -------------------------------------------------------------------
%%% Author  : rob
%%% Description :
%%%
%%% Created : Jan 6, 2011
%%% -------------------------------------------------------------------
-module(device_client_index).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/1, get_pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

% Firstly, this reads in the file pointed to by the DeviceAddrFile string
%% @spec start(DeviceAddrFile::string()) -> {ok,Pid} | {error,Error}
start(DeviceAddrFile) when is_list(DeviceAddrFile)->
    Devices = device_address_file_reader:read(DeviceAddrFile), 
    gen_server:start({local, ?MODULE}, ?MODULE, [Devices], []).

% Given a Device ID, this will return the Process ID for the relevant Device Client, or it
% will return the atom "undefined" if that device is not registered as available.
%% @spec get_pid(DeviceID::integer()) -> Pid | undefined
get_pid(DeviceID) when is_integer(DeviceID) ->
    gen_server:call(?MODULE, {getDeviceClientPid, DeviceID}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Devices]) ->
    create_device_clients(Devices),
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({getDeviceClientPid, DeviceID}, _From, State) ->
    Reply = get(DeviceID),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

create_device_clients([{DeviceID, IPaddress, Port} | Tail]) ->
    case device_client:start(DeviceID, IPaddress, Port) of
        {ok, Pid} -> 
            % Using process dictionary as the DeviceID to Pid mappings are effectively
            % process constants and will never change.
            % TODO: Report if a DeviceID has already been taking (i.e. conflicting registration)
            put(DeviceID, Pid),
            io:format("Successfully created client process (PID = ~p) for DeviceID = ~p~n", [Pid, DeviceID]);
        {error, What} -> 
            io:format("Warning: could not start device client with DeviceID=~p, "
                      "IPaddress=~p, Port=~p! Reason: ~p~n", [DeviceID, IPaddress, Port, What])
    end,
    create_device_clients(Tail);

create_device_clients([]) -> ok.
