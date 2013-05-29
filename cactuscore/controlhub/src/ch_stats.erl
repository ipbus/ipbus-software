%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since April 2012
%%%
%%% @doc Keeps track of various Control Hub stats such as request/packet
%%%      counts, etc.
%%% @end
%%% ===========================================================================
-module(ch_stats).

-behaviour(gen_server).

-include("ch_global.hrl").

%% API exports
-export([start_link/0,
         stop/0,
         client_connected/0,
         client_disconnected/0,
         client_request_in/0,
         client_request_malformed/0,
         client_response_sent/0,
         new_device_client_table/2,
         udp_sent/1,
         udp_rcvd/1,
         udp_timeout/2,
         udp_lost/2,
         get_active_clients/0,
         get_max_active_clients/0,
         get_cumulative_client_count/0,
         get_total_client_requests/0,
         get_total_client_malformed_requests/0,
         get_total_client_responses/0,
         report_to_console/0,
         report_to_string/0]).

%% Behavioural exports - the gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server state record definition - all of type integer().
-record(state, {active_clients = 0 :: integer(),
                max_active_clients = 0 :: integer(),
                cumulative_client_count = 0 :: integer(),
                request_count = 0 :: integer(),
                malformed_request_count = 0 :: integer(),
                response_count = 0 :: integer(),
                previous_udp_stats_table,
                current_udp_stats_tables = []
                }).



%%% ====================================================================
%%% API functions (public interface)
%%% ====================================================================

    
%% ---------------------------------------------------------------------
%% @doc Starts the (singleton) stats server.
%%
%% @spec start_link() -> {ok, Pid} | {error, {already_started, Pid}}
%% where
%%   Pid = pid()
%% @end
%% ---------------------------------------------------------------------
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ---------------------------------------------------------------------
%% @doc Stops the stats server.
%%
%% @spec stop() -> ok
%% @end
%% ---------------------------------------------------------------------
stop() -> gen_server:cast(?MODULE, stop).


%% ---------------------------------------------------------------------
%% @doc Inform the stats server that a user-client has connected.
%%
%% @spec client_connected() -> ok
%% @end
%% ---------------------------------------------------------------------
client_connected() -> gen_server:cast(?MODULE, client_connected).


%% ---------------------------------------------------------------------
%% @doc Inform the stats server that a user-client has disconnected.
%%
%% @spec client_disconnected() -> ok
%% @end
%% ---------------------------------------------------------------------
client_disconnected() -> gen_server:cast(?MODULE, client_disconnected).


%% ---------------------------------------------------------------------
%% @doc Inform the stats server that a user-client request has arrived.
%%      Note: this is for registering that a user-client request of
%%      some type - valid or malformed - has arrived at the Control Hub.
%%
%% @spec client_request_in() -> ok
%% @end
%% ---------------------------------------------------------------------
client_request_in() -> gen_server:cast(?MODULE, client_request_in).

%% ---------------------------------------------------------------------
%% @doc Inform the stats server that a user-client request was determined
%%      to be malformed.
%%
%% @spec client_request_malformed() -> ok
%% @end
%% ---------------------------------------------------------------------
client_request_malformed() -> gen_server:cast(?MODULE, client_request_malformed).


%% ---------------------------------------------------------------------
%% @doc Inform the stats server that a response has been sent to a
%%      user-client by the Control Hub.
%%
%% @spec client_response_sent() -> ok
%% @end
%% ---------------------------------------------------------------------
client_response_sent() -> gen_server:cast(?MODULE, client_response_sent).


%% ------------------------------------------------------------------------------------
%% @doc Returns a fresh ETS table for collating device_client stats, with all counters
%%      already inserted, and set to 0
%%
%% @spec new_device_client_table(TargetIPTuple, TargetPort) -> NewStatsTable
%% @end
%% ------------------------------------------------------------------------------------

new_device_client_table(TargetIPTuple, TargetPort) ->
    T = new_device_client_table(),
    ets:insert(T, {start, os:timestamp()}),
    ets:insert(T, {target_ip, TargetIPTuple}),
    ets:insert(T, {target_port, TargetPort}),
    ets:setopts(T, [{heir, whereis(ch_stats), no_data}]),
    ch_stats ! {new_device_client_table, self(), T},
    T.


%% ---------------------------------------------------------------------
%% @doc Increment "udp_sent" counter in device_client stats table
%%
%% @spec udp_sent(StatsTable) -> NewValue
%% @end
%% ---------------------------------------------------------------------
udp_sent(T) ->
    ets:update_counter(T, udp_sent, 1).


%% ---------------------------------------------------------------------
%% @doc Increment "udp_rcvd" counter in device_client stats table
%%
%% @spec udp_rcvd(StatsTable) -> NewValue
%% @end
%% ---------------------------------------------------------------------

udp_rcvd(T) ->
    ets:update_counter(T, udp_rcvd, 1).


%% ---------------------------------------------------------------------
%% @doc Increment one of the "udp_timeout" counters in device_client stats table
%%
%% @spec udp_timeout(StatsTable, Type) -> NewValue
%% where
%%     Type = normal | recovered | resend | status
%% @end
%% ---------------------------------------------------------------------

udp_timeout(T, Type) when Type=:=normal; Type=:=recovered; Type=:=resend; Type=:=status ->
    ets:update_counter(T, {udp_timeout, Type}, 1).


%% ---------------------------------------------------------------------
%% @doc Increment one of the "udp_lost" counters in device_client stats table
%%
%% @spec udp_timeout(StatsTable, Type) -> NewValue
%% where 
%%     Type = request | response
%% @end
%% ---------------------------------------------------------------------

udp_lost(T, Type) when Type=:=request; Type=:=response ->
    ets:update_counter(T, {udp_lost, Type}, 1).


%% ---------------------------------------------------------------------
%% @doc Returns the current number of connected user-clients.
%%
%% @spec get_active_clients() -> integer()
%% @end
%% ---------------------------------------------------------------------
get_active_clients() -> gen_server:call(?MODULE, get_active_clients).


%% ---------------------------------------------------------------------
%% @doc Returns the highest ever value of connected user-clients.
%%
%% @spec get_max_active_clients() -> integer()
%% @end
%% ---------------------------------------------------------------------
get_max_active_clients() -> gen_server:call(?MODULE, get_max_active_clients).

%% ---------------------------------------------------------------------
%% @doc Returns the total number of clients that have ever connected
%%      within the lifetime of the software instance.
%%
%% @spec get_cumulative_client_count() -> integer()
%% @end
%% ---------------------------------------------------------------------
get_cumulative_client_count() -> gen_server:call(?MODULE, get_cumulative_client_count).


%% ---------------------------------------------------------------------
%% @doc Returns the total number of client requests that have been
%%      received. Note that this total does not differentiate between
%%      valid/malformed requests, it's just the total number of requests
%%      that have arrived at the Control Hub.
%%
%% @spec get_total_client_requests() -> integer()
%% @end
%% ---------------------------------------------------------------------
get_total_client_requests() -> gen_server:call(?MODULE, get_total_client_requests).


%% ----------------------------------------------------------------------------
%% @doc Returns the total number of client requests that have arrived but have
%%      been determined to be malformed.
%%
%% @spec get_total_client_malformed_requests() -> integer()
%% @end
%% ----------------------------------------------------------------------------
get_total_client_malformed_requests() -> gen_server:call(?MODULE, get_total_client_malformed_requests).


%% ----------------------------------------------------------------------------
%% @doc Returns the total number of responses that have been sent to user
%%      clients by the Control Hub.
%%
%% @spec get_total_client_responses() -> integer()
%% @end
%% ----------------------------------------------------------------------------
get_total_client_responses() -> gen_server:call(?MODULE, get_total_client_responses).


%% ----------------------------------------------------------------------------
%% @doc Prints a report of all the current stats to the console.
%%
%% @spec report_to_console() -> ok
%% @end
%% ----------------------------------------------------------------------------
report_to_console() -> gen_server:cast(?MODULE, report_to_console).


%% ----------------------------------------------------------------------------
%% @doc Returns a current stats report in string form (well, a list really...).
%%
%% @spec report_to_string() -> string()
%% @end
%% ----------------------------------------------------------------------------
report_to_string() -> gen_server:call(?MODULE, report_to_string).



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
    ?CH_LOG_DEBUG("Initialising the stats server."),
    State = #state{previous_udp_stats_table=new_device_client_table()},
    {ok, State}.

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
handle_call(get_active_clients, _From,  State) ->
    {reply, State#state.active_clients, State};

handle_call(get_max_active_clients, _From,  State) ->
    {reply, State#state.max_active_clients, State};

handle_call(get_cumulative_client_count, _From,  State) ->
    {reply, State#state.cumulative_client_count, State};

handle_call(get_total_client_requests, _From,  State) ->
    {reply, State#state.request_count, State};

handle_call(get_total_client_malformed_requests, _From,  State) ->
    {reply, State#state.malformed_request_count, State};

handle_call(report_to_string, _From, State) ->
    {reply, report_to_string(State), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(client_connected, State = #state{active_clients = ActiveClients,
                                             max_active_clients = MaxActiveClients,
                                             cumulative_client_count = AllTimeClients}) ->
    NewActiveClients = ActiveClients+1,
    NewAllTimeClients = AllTimeClients+1,
    NewMaxActiveClients = if
                              MaxActiveClients < NewActiveClients -> NewActiveClients;
                              true -> MaxActiveClients
                          end,
    NewState = State#state{active_clients = NewActiveClients,
                           max_active_clients = NewMaxActiveClients,
                           cumulative_client_count = NewAllTimeClients},
    {noreply, NewState};

handle_cast(client_disconnected, State = #state{active_clients = Clients}) ->
    NewState = State#state{active_clients = Clients-1},
    {noreply, NewState};

handle_cast(client_request_in, State = #state{request_count = Requests}) ->
    NewState = State#state{request_count = Requests+1},
    {noreply, NewState};

handle_cast(client_request_malformed, State = #state{malformed_request_count = BadRequests}) ->
    NewState = State#state{malformed_request_count = BadRequests+1},
    {noreply, NewState};

handle_cast(client_response_sent, State = #state{response_count = Responses}) ->
    NewState = State#state{response_count = Responses+1},
    {noreply, NewState};

handle_cast(report_to_console, State) ->
    io:format("~n~s~n", [report_to_string(State)]),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({new_device_client_table, DeviceClientPid, TableId}, S) ->
    NewTablesList = [ {DeviceClientPid,TableId} | S#state.current_udp_stats_tables ],
    {noreply, S#state{current_udp_stats_tables=NewTablesList}};

handle_info({'ETS-TRANSFER', T1, Pid, no_data}, S) ->
    T0 = S#state.previous_udp_stats_table,
    lists:foreach(fun(X) -> ets:update_counter(T0, X, ets:lookup_element(T1, X, 2)) end,
                  [udp_sent,
                   udp_rcvd,
                   {udp_timeout,normal},
                   {udp_lost,request},
                   {udp_lost,response},
                   {udp_timeout,recovered},
                   {udp_timeout,resend},
                   {udp_timeout,status}
                   ]),
    NewTablesList = lists:keydelete(Pid, 1, S#state.current_udp_stats_tables),
    {noreply, S#state{current_udp_stats_tables=NewTablesList}};

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

%% Returns a stats report in string form
report_to_string(State) ->
    lists:flatten([io_lib:format("Control Hub Stats Report~n"
                                "------------------------~n~n"
                                "CLIENT  All-time connections: ~p~n"  
                                "          Active connections: ~p (peak: ~p)~n"
                                "           Requests received: ~p (of which ~p were malformed)~n"
                                "              Responses sent: ~p~n",
                                [State#state.cumulative_client_count,
                                 State#state.active_clients,
                                 State#state.max_active_clients,
                                 State#state.request_count,
                                 State#state.malformed_request_count,
                                 State#state.response_count
                                ]),
                   lists:flatten([stats_table_to_string(T,"") || {_, T} <- State#state.current_udp_stats_tables]),
                   stats_table_to_string(State#state.previous_udp_stats_table, "Other UDP")
                   ]).


%% ------------------------------------------------------------------------------------
%% @doc Returns a fresh ETS table for collating device_client stats, with all counters
%%      already inserted, and set to 0
%%
%% @spec new_device_client_table() -> NewStatsTable
%% @end
%% ------------------------------------------------------------------------------------

new_device_client_table() ->
    T = ets:new(device_client_stats, [ordered_set,protected]),
    ets:insert(T, {udp_sent, 0}),
    ets:insert(T, {udp_rcvd, 0}),
    ets:insert(T, {{udp_timeout,normal}, 0}),
    ets:insert(T, {{udp_lost,request},0}),
    ets:insert(T, {{udp_lost,response},0}),
    ets:insert(T, {{udp_timeout,recovered}, 0}),
    ets:insert(T, {{udp_timeout,resend}, 0}),
    ets:insert(T, {{udp_timeout,status}, 0}),
    T.


%% Returns stats table results in string form
stats_table_to_string(T, DefaultText) ->
    io_lib:format("~n~s~n"
                  "              Packets Out: ~p~n"
                  "               Packets In: ~p~n"
                  "                 Timeouts: ~p in normal operation (of which ~p were recovered)~n"
                  "                             in ~p of these instances the request got lost, ~p times response got lost~n"
                  "           Other timeouts: ~p (status), ~p (in resends)~n",
                  [case ets:member(T, target_ip) of
                       true -> 
                           {IP1,IP2,IP3,IP4} = ets:lookup_element(T, target_ip, 2),
                           TS = {_,_,Micro} = ets:lookup_element(T, start, 2),
                           {{Yr,Month,Day}, {Hr,Min,Sec}} = calendar:now_to_universal_time(TS),
                           io_lib:format("UDP to/from ~w.~w.~w.~w:~w"
                                         "  since ~2w:~2..0w:~2..0w.~3..0w, ~2..0w-~2..0w-~4w UTC",
                                         [IP1,IP2,IP3,IP4,ets:lookup_element(T,target_port,2),
                                          Hr,Min,Sec,Micro div 1000, Day,Month,Yr]);
                       false ->
                           DefaultText
                   end,
                   ets:lookup_element(T, udp_sent, 2),
                   ets:lookup_element(T, udp_rcvd, 2),
                   ets:lookup_element(T, {udp_timeout,normal}, 2),
                   ets:lookup_element(T, {udp_timeout,recovered}, 2),
                   ets:lookup_element(T, {udp_lost, request}, 2),
                   ets:lookup_element(T, {udp_lost, response}, 2),
                   ets:lookup_element(T, {udp_timeout,status}, 2),
                   ets:lookup_element(T, {udp_timeout,resend}, 2)
                  ]).
