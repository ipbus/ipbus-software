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
         client_requests_in/2,
         client_request_malformed/1,
         client_responses_sent/2,
         new_transaction_manager_table/2,
         new_device_client_table/2,
         udp_sent/1,
         udp_rcvd/1,
         udp_timeout/2,
         udp_lost/2,
         get_active_clients/0,
         get_max_active_clients/0,
         get_cumulative_client_count/0,
         report_to_console/0,
         report_to_string/0,
         get_app_info/0]).

%% Behavioural exports - the gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server state record definition - all of type integer().
-record(state, {active_clients = 0 :: integer(),
                max_active_clients = 0 :: integer(),
                cumulative_client_count = 0 :: integer(),
                previous_client_stats_table,
                current_client_stats_tables = [],
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
%% @doc Increment requests_in counter in transaction_manager stats table
%%
%% @spec client_request_in(T, N :: non_neg_integer()) -> ok
%% @end
%% ---------------------------------------------------------------------
client_requests_in(T, N) ->
    ets:update_counter(T, requests_in, N).

%% ---------------------------------------------------------------------
%% @doc Increment request_malformed counter in transaction_manager stats table
%%
%% @spec client_request_malformed(T) -> ok
%% @end
%% ---------------------------------------------------------------------
client_request_malformed(T) -> 
    ets:update_counter(T, requests_malformed, 1).


%% ---------------------------------------------------------------------
%% @doc Increment responses_sent counter in transaction_manager stats table
%%
%% @spec client_responses_sent(T, N :: non_neg_integer()) -> ok
%% @end
%% ---------------------------------------------------------------------
client_responses_sent(T, N) -> 
    ets:update_counter(T, responses_sent, N).


%% ------------------------------------------------------------------------------------
%% @doc Returns a fresh ETS table for collating transaction_manager stats, with all counters
%%      already inserted, and set to 0
%%
%% @spec new_transaction_manager_table(ClientIPTuple, ClientPort) -> NewStatsTable
%% @end
%% ------------------------------------------------------------------------------------

new_transaction_manager_table(ClientIPTuple, ClientPort) ->
    T = new_transaction_manager_table(),
    ets:insert(T, {start, os:timestamp()}),
    ets:insert(T, {client_ip, ClientIPTuple}),
    ets:insert(T, {client_port, ClientPort}),
    ets:setopts(T, [{heir, whereis(ch_stats), no_data}]),
    ch_stats ! {new_transaction_manager_table, self(), T},
    T.


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



%% ----------------------------------------------------------------------------
%% @doc Returns lists of tuples containing ControlHub configuration parameters 
%%
%% @end
%% ----------------------------------------------------------------------------
-spec get_app_info() -> [{controlhub_start_timestamp, erlang:timestamp()}
                         | {controlhub_vsn, string()}
                         | {config_file, string()}
                         | {tcp_listen_port, non_neg_integer()}
                         | {tcp_socket_opts, [any()]}
                         | {max_udp_in_flight, non_neg_integer()}
                         | {device_response_timeout, non_neg_integer()}
                         | {device_client_shutdown_after, non_neg_integer()}].
get_app_info() ->
    Vsn = case application:get_key(controlhub,vsn) of
              {ok, Version} -> Version;
              Else -> io_lib:format("ERROR retrieving version~w", [Else])
          end,
    [{controlhub_start_timestamp, ch_config:get_start_time()},
     {controlhub_vsn, Vsn},
     {config_file, ch_config:get(config_file)},
     {tcp_listen_port, ch_config:get(tcp_listen_port)},
     {tcp_socket_opts, ch_config:get(tcp_socket_opts)},
     {max_udp_in_flight, ch_config:get(max_udp_in_flight)},
     {device_response_timeout, ch_config:get(device_response_timeout)},
     {device_client_shutdown_after, ch_config:get(device_client_shutdown_after)},
     {device_allowlist_mode, ch_config:get(device_allowlist_mode)},
     {device_allowlist_file, ch_config:get(device_allowlist_file)}
    ].



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
    ch_utils:log(notice, "Initialising the stats server."),
    State = #state{previous_udp_stats_table=new_device_client_table(),
                   previous_client_stats_table=new_transaction_manager_table()
                  },
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

handle_info({new_transaction_manager_table, Pid, TableId}, S) ->
    NewTablesList = [ {Pid,TableId} | S#state.current_client_stats_tables ],
    {noreply, S#state{current_client_stats_tables=NewTablesList}};

handle_info({'ETS-TRANSFER', T1, Pid, no_data}, S) ->
    NewS = case ets:info(T1, name) of
               device_client_stats ->
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
                   ets:delete(T1),
                   S#state{current_udp_stats_tables=NewTablesList};
               transaction_manager_stats ->
                   T0 = S#state.previous_client_stats_table,
                   lists:foreach(fun(X) -> ets:update_counter(T0, X, ets:lookup_element(T1, X, 2)) end,
                                 [requests_in,
                                  requests_malformed,
                                  responses_sent
                                 ]),
                   NewTablesList = lists:keydelete(Pid, 1, S#state.current_client_stats_tables),
                   ets:delete(T1),
                   S#state{current_client_stats_tables=NewTablesList}
            end,
    {noreply, NewS};

handle_info(_Info, State) ->
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ch_utils:log(notice, "Stats server shutting down."),
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
                                "          Active connections: ~p (peak: ~p)~n",
                                [State#state.cumulative_client_count,
                                 State#state.active_clients,
                                 State#state.max_active_clients
                                ]),
                   lists:flatten([stats_table_to_string(T,"") || {_, T} <- State#state.current_client_stats_tables]),
                   stats_table_to_string(State#state.previous_client_stats_table, "Old clients"),
                   lists:flatten([stats_table_to_string(T,"") || {_, T} <- State#state.current_udp_stats_tables]),
                   stats_table_to_string(State#state.previous_udp_stats_table, "Old UDP")
                   ]).


%% ------------------------------------------------------------------------------------
%% @doc Returns a fresh ETS table for collating transaction_manager stats, with all 
%%      counters already inserted, and set to 0
%%
%%
%% @spec new_transaction_manager_table() -> NewStatsTable
%% @end
%% ------------------------------------------------------------------------------------

new_transaction_manager_table() ->
    T = ets:new(transaction_manager_stats, [ordered_set,protected]),
    ets:insert(T, {requests_in, 0}),
    ets:insert(T, {requests_malformed, 0}),
    ets:insert(T, {responses_sent, 0}),
    T.


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
    case ets:info(T, name) of
        transaction_manager_stats ->
            io_lib:format("  ~s~n"
                          "           Requests received: ~p (of which ~p were malformed)~n"
                          "              Responses sent: ~p~n",
                          [case ets:member(T, client_ip) of 
                               true ->
                                   ClientIP = ets:lookup_element(T, client_ip, 2),
                                   ClientPort = ets:lookup_element(T, client_port, 2),
                                   io_lib:format("@ ~s", [ch_utils:ip_port_string(ClientIP, ClientPort)]);
                               false ->
                                   DefaultText
                           end,
                           ets:lookup_element(T, requests_in, 2),
                           ets:lookup_element(T, requests_malformed, 2),
                           ets:lookup_element(T, responses_sent, 2)
                          ]);
        device_client_stats ->
            io_lib:format("~n~s~n"
                          "              Packets Out: ~p~n"
                          "               Packets In: ~p~n"
                          "                 Timeouts: ~p in normal operation (of which ~p were recovered)~n"
                          "                             (~p request packets got lost, ~p reply packet)~n"
                          "           Other timeouts: ~p (status), ~p (in resends)~n",
                          [case ets:member(T, target_ip) of
                               true -> 
                                   {IP1,IP2,IP3,IP4} = ets:lookup_element(T, target_ip, 2),
                                   TS = {_,_,Micro} = ets:lookup_element(T, start, 2),
                                   {{Yr,Month,Day}, {Hr,Min,Sec}} = calendar:now_to_universal_time(TS),
                                   io_lib:format("Board @ ~w.~w.~w.~w:~w"
                                                 ", since ~2w:~2..0w:~2..0w.~3..0w, ~2..0w-~2..0w-~4w UTC",
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
                           ])
    end.
