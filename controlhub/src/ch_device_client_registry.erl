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
-export([start_link/0, stop/0, get_index/0, get_pid/3, total_device_clients/1 ]).

%% Behavioural exports - gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server state record definition
-record(state, {dc_index = ets:new(device_client_index, [protected, {read_concurrency, true}]),
                dc_reverse_index = ets:new(device_client_reverse_index, [private])}).


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
%% @doc Returns the Device Client Index ETS table ID the device clients
%%      are registered into. This table is read only to any processes
%%      except the registry itself.
%%
%% @spec get_index() -> DCIndex::tid()
%% @end
%% ---------------------------------------------------------------------
get_index() ->
    gen_server:call(?MODULE, get_index).


%% ---------------------------------------------------------------------
%% @doc Returns the Process ID of the Device Client that handles the
%%      IPbus transactions for a given hardware target - as defined by
%%      the target's IP address and port number - by looking it up in the
%%      given Device Client Index ETS table (see get_index() if you don't
%%      have the table ID already).  If no such Device Client yet exists
%%      for the given hardware target, a Device Client will be created,
%%      registered, and its PID returned.
%%
%% @spec get_pid(DCIndex::tid(), IPaddr::integer(), Port::integer()) -> {ok, pid()} | {error, any()}
%% @end
%% ---------------------------------------------------------------------
get_pid(DCIndex, IPaddr, Port) ->
    case ets:lookup(DCIndex, {IPaddr, Port}) of
        [ { {IPaddr, Port}, Pid } ] -> {ok, Pid};
        [ ] -> gen_server:call(?MODULE, {register, {IPaddr, Port}});
        Error -> {error, Error}
    end.


%% ---------------------------------------------------------------------
%% @doc Returns the number of registered Device Clients currently in the
%%      Device Client Index.
%%
%% @spec total_device_clients(DCIndex::tid()) -> integer()
%% ---------------------------------------------------------------------
total_device_clients(DCIndex) ->
    [NumDeviceClients] = [ X || {size, X} <- ets:info(DCIndex) ],
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
    ?DEBUG_TRACE("Initialising the device client registry."),
    % Trap exits - if any device clients spawned by the registry end
    % up dying, we'll need to catch the message and remove the device
    % client from the index.
    process_flag(trap_exit, true),
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
handle_call({register, {IPaddr, Port}}, _From, State) ->
    % To avoid possible race conditions now that we're within the
    % safe sequential territory of our single device client registry
    % server process, we must first check that the specified device
    % hasn't already been created+registered, and proceed from there.
    Reply = case ets:lookup(State#state.dc_index, {IPaddr, Port}) of
                [ { {IPaddr, Port}, Pid } ] -> {ok, Pid};  % Already got created
                [] -> create_and_register_device_client(IPaddr, Port, State#state.dc_index, State#state.dc_reverse_index)
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
    ?DEBUG_TRACE("Observed process ~p shutting down with reason: ~p", [Pid, _Reason]),
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

create_and_register_device_client(IPaddr, Port, Index, ReverseIndex) ->
    case ch_device_client:start_link(IPaddr, Port) of
        {ok, Pid} -> 
            ets:insert(Index, {{IPaddr, Port}, Pid}),
            % For reverse lookups.
            % If we catch a process dying, we can check the dead process Pid
            % to be certain its a device client started by the registry.
            ets:insert(ReverseIndex, {Pid, {IPaddr, Port}}),  
            {ok, Pid};
        Error -> {error, Error}
    end.


