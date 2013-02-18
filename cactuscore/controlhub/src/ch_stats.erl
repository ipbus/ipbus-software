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
         udp_in/0,
         udp_malformed/0,
         udp_out/0,
         udp_response_timeout/0,
         get_active_clients/0,
         get_max_active_clients/0,
         get_cumulative_client_count/0,
         get_total_client_requests/0,
         get_total_client_malformed_requests/0,
         get_total_client_responses/0,
         get_udp_in/0,
         get_udp_malformed/0,
         get_udp_out/0,
         get_udp_response_timeouts/0,
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
                udp_in = 0 :: integer(),
                udp_malformed = 0 :: integer(),
                udp_out = 0 :: integer(),
                udp_response_timeouts = 0 :: integer()}).



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


%% ---------------------------------------------------------------------
%% @doc Inform the stats server that a UDP packet arrived.
%%
%% @spec udp_in() -> ok
%% @end
%% ---------------------------------------------------------------------
udp_in() -> gen_server:cast(?MODULE, udp_in).


%% ---------------------------------------------------------------------
%% @doc Inform the stats server that a UDP packet was determined
%%      to be malformed.
%%
%% @spec udp_malformed() -> ok
%% @end
%% ---------------------------------------------------------------------
udp_malformed() -> gen_server:cast(?MODULE, udp_malformed).


%% ---------------------------------------------------------------------
%% @doc Inform the stats server that a UDP packet was sent.
%%
%% @spec udp_out() -> ok
%% @end
%% ---------------------------------------------------------------------
udp_out() -> gen_server:cast(?MODULE, udp_out).


%% ---------------------------------------------------------------------
%% @doc Inform the stats server that a UDP response timeout occurred.
%%
%% @spec udp_response_timeout() -> ok
%% @end
%% ---------------------------------------------------------------------
udp_response_timeout() -> gen_server:cast(?MODULE, udp_response_timeout).


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
%% @doc Returns the current total of incoming UDP packets that have been
%%      received (malformed or valid).
%%
%% @spec get_udp_in() -> integer()
%% @end
%% ----------------------------------------------------------------------------
get_udp_in() -> gen_server:call(?MODULE, get_udp_in).


%% ----------------------------------------------------------------------------
%% @doc Returns the current total of incoming UDP packets that have been
%%      determined to be malformed.
%%
%% @spec get_udp_malformed() -> integer()
%% @end
%% ----------------------------------------------------------------------------
get_udp_malformed() -> gen_server:call(?MODULE, get_udp_malformed).


%% ----------------------------------------------------------------------------
%% @doc Returns the current total of UDP packets that have been sent.
%%
%% @spec get_udp_out() -> integer()
%% @end
%% ----------------------------------------------------------------------------
get_udp_out() -> gen_server:call(?MODULE, get_udp_out).


%% ----------------------------------------------------------------------------
%% @doc Returns the current total of UDP reponse timeouts that have occurred.
%%
%% @spec get_udp_response_timeouts() -> integer()
%% @end
%% ----------------------------------------------------------------------------
get_udp_response_timeouts() -> gen_server:call(?MODULE, get_udp_response_timeouts).


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
    ?DEBUG_TRACE("Initialising the stats server."),
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

handle_call(get_total_client_responses, _From,  State) ->
    {reply, State#state.response_count, State};

handle_call(get_udp_in, _From,  State) ->
    {reply, State#state.udp_in, State};

handle_call(get_udp_malformed, _From,  State) ->
    {reply, State#state.udp_malformed, State};

handle_call(get_udp_out, _From,  State) ->
    {reply, State#state.udp_out, State};

handle_call(get_udp_response_timeouts, _From,  State) ->
    {reply, State#state.udp_response_timeouts, State};

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

handle_cast(udp_in, State = #state{udp_in = Packets}) ->
    NewState = State#state{udp_in = Packets+1},
    {noreply, NewState};

handle_cast(udp_malformed, State = #state{udp_malformed = Packets}) ->
    NewState = State#state{udp_malformed = Packets+1},
    {noreply, NewState};

handle_cast(udp_out, State = #state{udp_out = Packets}) ->
    NewState = State#state{udp_out = Packets+1},
    {noreply, NewState};

handle_cast(udp_response_timeout, State = #state{udp_response_timeouts = Value}) ->
    NewState = State#state{udp_response_timeouts = Value+1},
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
    lists:flatten(io_lib:format("Control Hub Stats Report~n"
                                "------------------------~n~n"
                                "CLIENT  All-time connections: ~p~n"  
                                "          Active connections: ~p (peak: ~p)~n"
                                "           Requests received: ~p (of which ~p were malformed)~n"
                                "              Responses sent: ~p~n~n"
                                "UDP              Packets Out: ~p~n"
                                "                  Packets In: ~p (of which ~p were malformed)~n"
                                "                    Timeouts: ~p",
                                [State#state.cumulative_client_count,
                                 State#state.active_clients,
                                 State#state.max_active_clients,
                                 State#state.request_count,
                                 State#state.malformed_request_count,
                                 State#state.response_count,
                                 State#state.udp_out,
                                 State#state.udp_in,
                                 State#state.udp_malformed,
                                 State#state.udp_response_timeouts])).
