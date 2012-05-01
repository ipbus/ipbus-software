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
-module(device_client_registry).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, stop/0, get_pid/3, get_index/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server state record definition
-record(state, {device_client_index = ets:new(device_client_index, [{read_concurrency, true}])}).


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
%% @doc Returns the Device Client Index ETS table ID the device clients
%%      are registered into. This table is read only to any processes
%%      except the registry itself.
%%
%% @spec get_index() -> DCIndex::tid()
%% @end
%% ---------------------------------------------------------------------
get_index() ->
    gen_server:call(?MODULE, get_index).



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
    % Create the device client register, and we're done.
    %ets:new(ets_device_client_register, [named_table, {read_concurrency, true}]).
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
    Reply = case ets:lookup(State#state.device_client_index, {IPaddr, Port}) of
                [ { {IPaddr, Port}, Pid } ] -> {ok, Pid};  % Already got created
                [ ] -> create_and_register_device_client(IPaddr, Port, State#state.device_client_index)
            end,
    {reply, Reply, State};

handle_call(get_index, _From, State) ->
    {reply, State#state.device_client_index, State};

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



%%% --------------------------------------------------------------------
%%% Internal functions (private)
%%% --------------------------------------------------------------------

create_and_register_device_client(IPaddr, Port, Index) ->
    case device_client:start_link(IPaddr, Port) of
        {ok, Pid} -> 
            ets:insert(Index, {{IPaddr, Port}, Pid}),
            {ok, Pid};
        Error -> {error, Error}
    end.

%% create_device_clients([{DeviceID, IPaddress, Port} | Tail]) ->
%%     case device_client:start(DeviceID, IPaddress, Port) of
%%         {ok, Pid} -> 
%%             % Using process dictionary as the DeviceID to Pid mappings are effectively
%%             % process constants and will never change.
%%             % TODO: Report if a DeviceID has already been taking (i.e. conflicting registration)
%%             put(DeviceID, Pid),
%%             io:format("Successfully created client process (PID = ~p) for DeviceID = ~p~n", [Pid, DeviceID]);
%%         {error, What} -> 
%%             io:format("Warning: could not start device client with DeviceID=~p, "
%%                       "IPaddress=~p, Port=~p! Reason: ~p~n", [DeviceID, IPaddress, Port, What])
%%     end,
%%     create_device_clients(Tail);
%% 
%% create_device_clients([]) -> ok.
