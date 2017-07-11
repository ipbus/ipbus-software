%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since April 2012
%%%
%%% @doc Owns the TCP socket that is being listened on for incoming TCP client
%%%      connections (i.e. microHAL clients), spawning new ch_transaction_manager
%%%      processes in a loop that wait to accept a connection and handle the
%%%      client for the lifetime of the client connection.
%%% @end
%%% ===========================================================================
-module(ch_tcp_listener).

-behaviour(gen_server).

-include("ch_global.hrl").


%% API exports
-export([start_link/0, stop/0, connection_accept_completed/0]).

%% Behavioural exports - the gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% Server state - just a socket.
-record(state, {socket}).  % store the TCP socket that we're listening on


%%% ====================================================================
%%% API functions (public interface)
%%% ====================================================================

%% ---------------------------------------------------------------------
%% @doc Starts the (singleton) TCP listener that owns the TCP socket.
%%
%% @spec start_link() -> {ok, Pid} | {error, {already_started, Pid}}
%% where
%%   Pid = pid()
%% @end
%% ---------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ---------------------------------------------------------------------
%% @doc Stops the TCP listener
%%
%% @spec stop() -> ok
%% @end
%% ---------------------------------------------------------------------
stop() ->
    gen_server:cast(?MODULE, stop).


%% ---------------------------------------------------------------------
%% @doc Allows processes that have been spawned to accept the TCP client
%%      connections to report back (asynchronously) to the TCP listener
%%      process that the TCP accept completed. This TCP accept completion
%%      can either be successful or otherwise; it doen't matter which,
%%      just that the TCP accept call stopped blocking and returned.
%% @spec connection_accept_completed() -> ok
%% @end
%% ---------------------------------------------------------------------
connection_accept_completed() ->
    gen_server:cast(?MODULE, connection_accept_completed).



%%% ====================================================================
%%% Behavioural externals (the gen_server callbacks)
%%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% @doc Opens a TCP port to listen on, and spawns an acceptor to wait
%%      for the first connection
%% @end
%% --------------------------------------------------------------------
init([]) ->
    Port = ch_config:get(tcp_listen_port),
    ch_utils:log(notice, "Initialising the TCP listener (port ~p).", [Port]),
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, lists:keyreplace(active, 1, ch_config:get(tcp_socket_opts), {active,false}) ) of
        {ok, TcpListenSocket} ->
            {ok, spawn_acceptor(#state{socket = TcpListenSocket})};
        {error, eaddrinuse} ->
            ErrorString = io_lib:format("The ControlHub TCP listen port (~p) is already in use", [Port]),
            ch_utils:log(critical, "~s; ControlHub cannot start correctly!", [ErrorString]),
            {stop, ErrorString};
        {error, What} ->
            ch_utils:log(critical,"Unexpected error listening to TCP port ~p -- error message: ~p", [Port,What]),
            {stop, {"Error starting the Control Hub TCP listener", What}}
    end.    


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

%% Default call handler - on unknown call request it does nothing.
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

%% Spawn a new acceptor as soon as a transaction manager reports that a conection accept completed.
handle_cast(connection_accept_completed, State) ->
    {noreply, spawn_acceptor(State)};

%% Shutdown
handle_cast(stop, State) ->
    {stop, normal, State};

%% Default cast handler - on unknown cast message it does nothing
handle_cast(_Msg, State) ->
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

% Observe any exit signals that come our way but do nothing with them other than a trace message
handle_info({'EXIT', _Pid, _Reason}, State) when _Reason =/= normal ->
    ch_utils:log(warning, "Observed transaction manager process ~p shutting down with reason: ~p", [_Pid, _Reason]),
    {noreply, State};

%% Default info handler - on unknown info message it does nothing
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, State) ->
    ch_utils:log(notice, "TCP listener shutting down."),
    gen_tcp:close(State#state.socket),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% --------------------------------------------------------------------
%%% Internal functions
%%% --------------------------------------------------------------------

%% spawns a ch_transaction_manager process to accept and deal with each new client
%% @spec spawn_acceptor(State) -> State
spawn_acceptor(State) ->
    ch_transaction_manager:start_link(State#state.socket),
    State.
