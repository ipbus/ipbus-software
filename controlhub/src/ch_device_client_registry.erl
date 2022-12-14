%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since April 2012
%%%
%%% @doc Module for getting the process ID of the Device Client that deals
%%%      with the IPbus communication to a particular hardware target. If
%%%      such a process doesn't already exist it gets created and registered
%%%      transparently.
%%% @end
%%% ===========================================================================
-module(ch_device_client_registry).

-behaviour(gen_server).

-include("ch_global.hrl").

%% API  exports
-export([start_link/0, stop/0, get_pid/2, total_device_clients/0 ]).

%% Behavioural exports - gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server state record definition
-record(state, {dc_index = ets:new(device_client_index, [named_table, protected, {read_concurrency, true}]),
                dc_reverse_index = ets:new(device_client_reverse_index, [private]),
                max_in_flight = 16
                }).


%%% ====================================================================
%%% API functions (public interface)
%%% ====================================================================

%% ---------------------------------------------------------------------
%% @doc Starts the Device Client Registry server.
%%
%% @spec start_link() -> {ok, Pid} | {error, {already_started, Pid}}
%% where
%%   Pid = pid()
%% @end
%% ---------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ---------------------------------------------------------------------
%% @doc Stops the Device Client Registry server.
%%
%% @spec stop() -> ok
%% @end
%% ---------------------------------------------------------------------
stop() -> gen_server:cast(?MODULE, stop).


%% ---------------------------------------------------------------------
%% @doc Returns the Process ID of the Device Client that handles the
%%      IPbus transactions for a given hardware target - as defined by
%%      the target's IP address and port number.  If no such Device
%%      Client yet exists for the given hardware target, a Device Client
%%      will be created, registered, and its PID returned.
%%
%% @spec get_pid(IPaddr::integer(), Port::integer()) -> {ok, pid()} | blocked | {error, any()}
%% @end
%% ---------------------------------------------------------------------
get_pid(IPaddr, Port) ->
    case ets:lookup(device_client_index, {IPaddr, Port}) of
        [ { {IPaddr, Port}, Pid } ] ->
            {ok, Pid};
        [ ] ->
            AllowlistMode = ch_config:get(device_allowlist_mode),
            {IP1, IP2, IP3, IP4} = ch_utils:ipv4_u32_addr_to_tuple(IPaddr),
            case {AllowlistMode, ch_config:get_device_access_status({IP1, IP2, IP3, IP4}, Port)} of
                {_, allow} ->
                    gen_server:call(?MODULE, {register, {IPaddr, Port}});
                {permissive, deny} ->
                    ch_utils:log(warning, "Packets are being sent to device at  ~w.~w.~w.~w:~w, which is not in the allowlist (running in permissive mode though, so still sending the packet). ", [IP1, IP2, IP3, IP4, Port]),
                    gen_server:call(?MODULE, {register, {IPaddr, Port}});
                {enforcing, deny} ->
                    ch_utils:log(error, "Blocked attempt to send packet to device at ~w.~w.~w.~w:~w.", [IP1, IP2, IP3, IP4, Port]),
                    blocked
            end;
        Error ->
            {error, Error}
    end.


%% ---------------------------------------------------------------------
%% @doc Returns the number of Device Clients currently registered.
%%
%% @spec total_device_clients() -> integer()
%% @end
%% ---------------------------------------------------------------------
total_device_clients() ->
    [NumDeviceClients] = [ X || {size, X} <- ets:info(device_client_index) ],
    NumDeviceClients.


%%% ====================================================================
%%% gen_server callbacks
%%% ====================================================================


%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    % Note that our ETS table for holding the index of device clients
    % gets created as part of the standard server state record.
    ch_utils:log(notice, "Initialising the device client registry."),
    % Grab max_in_flight value from configuration file
    MaxInFlight = ch_config:get(max_udp_in_flight),
    % Trap exits - if any device clients spawned by the registry end
    % up dying, we'll need to catch the message and remove the device
    % client from the index.
    process_flag(trap_exit, true),
    {ok, #state{max_in_flight=MaxInFlight}}.

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
handle_call({register, {IPaddr, Port}}, _From, State) ->
    % To avoid possible race conditions now that we're within the
    % safe sequential territory of our single device client registry
    % server process, we must first check that the specified device
    % hasn't already been created+registered, and proceed from there.
    Reply = case ets:lookup(State#state.dc_index, {IPaddr, Port}) of
                [ { {IPaddr, Port}, Pid } ] -> {ok, Pid};  % Already got created
                [] -> create_and_register_device_client(IPaddr, Port, State#state.max_in_flight, State#state.dc_index, State#state.dc_reverse_index)
            end,
    {reply, Reply, State};

handle_call(get_index, _From, State) ->
    {reply, State#state.dc_index, State};

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
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%%
handle_info({'EXIT', Pid, _Reason}, State = #state{dc_index = Index, dc_reverse_index = RIndex}) ->
    ?CH_LOG_DEBUG("Observed process ~p shutting down with reason: ~p", [Pid, _Reason]),
    % If the dead process is registered, delete the entries associated with it.
    case ets:lookup(RIndex, Pid) of
        [{Pid, {IPaddr, Port}}] ->
            ets:delete(Index, {IPaddr, Port}),
            ets:delete(RIndex, Pid);
        [] -> ok
    end,     
    {noreply, State};

%% Default handle info
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ch_utils:log(notice, "Device client registry shutting down."),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%% --------------------------------------------------------------------
%%% Internal functions (private)
%%% --------------------------------------------------------------------

create_and_register_device_client(IPaddr, Port, MaxInFlight, Index, ReverseIndex) ->
    case ch_device_client:start_link(IPaddr, Port, MaxInFlight) of
        {ok, Pid} -> 
            ets:insert(Index, {{IPaddr, Port}, Pid}),
            % For reverse lookups.
            % If we catch a process dying, we can check the dead process Pid
            % to be certain its a device client started by the registry.
            ets:insert(ReverseIndex, {Pid, {IPaddr, Port}}),  
            {ok, Pid};
        Error -> {error, Error}
    end.


