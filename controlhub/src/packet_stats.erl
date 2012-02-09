%%% -------------------------------------------------------------------
%%% Author  : Robert Frazier
%%% Description : Keeps track of incoming/outgoing packet statistics
%%%
%%% Created : Dec 17, 2010
%%% -------------------------------------------------------------------
-module(packet_stats).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/0,
         tcp_in/0,
         tcp_malformed_in/0,
         tcp_out/0,
         udp_in/0,
         udp_malformed_in/0,
         udp_out/0,
         udp_response_timeout/0,
         report/0,
         get_tcp_in/0,
         get_tcp_malformed_in/0,
         get_tcp_out/0,
         get_udp_in/0,
         get_udp_malformed_in/0,
         get_udp_out/0,
         get_udp_response_timeouts/0,
         get_all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tcp_in = 0,
                tcp_malformed_in = 0,
                tcp_out = 0,
                udp_in = 0,
                udp_malformed_in = 0,
                udp_out = 0,
                udp_response_timeouts = 0}).

%% ====================================================================
%% External functions
%% ====================================================================

%% Start the packet statistics server
start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% Call this to add one to the incoming TCP packet counter (for all TCP packets, malformed or otherwise).
tcp_in() -> gen_server:cast(?MODULE, tcp_in).

%% Call this to add one to the incoming but malformed TCP packet counter.
tcp_malformed_in() -> gen_server:cast(?MODULE, tcp_malformed_in).

%% Call this to add one to the outgoing TCP packet counter.
tcp_out() -> gen_server:cast(?MODULE, tcp_out).

%% Call this to add one to the incoming UDP packet counter (for all UDP packets, malformed or otherwise).
udp_in() -> gen_server:cast(?MODULE, udp_in).

%% Call this to add one to the incoming but malformed UDP packet counter.
udp_malformed_in() -> gen_server:cast(?MODULE, udp_malformed_in).

%% Call this to add one to the outgoing TCP packet counter.
udp_out() -> gen_server:cast(?MODULE, udp_out).

%% Call this to add one to the UDP response timeout counter.
udp_response_timeout() -> gen_server:cast(?MODULE, udp_response_timeout).

%% Returns the current number of incoming TCP packets that have been received (good or malformed).
%% @spec get_tcp_in() -> integer
get_tcp_in() -> gen_server:call(?MODULE, get_tcp_in).

%% Returns the current number of incoming TCP packets that have been malformed.
%% @spec get_tcp_malformed_in() -> integer
get_tcp_malformed_in() -> gen_server:call(?MODULE, get_tcp_malformed_in).

%% Returns the current number of TCP packets that have been sent.
%% @spec get_tcp_out() -> integer
get_tcp_out() -> gen_server:call(?MODULE, get_tcp_out).

%% Returns the current number of incoming UDP packet counts (malformed or otherwise).
%% @spec get_udp_in() -> integer
get_udp_in() -> gen_server:call(?MODULE, get_udp_in).

%% Returns the current number of incoming UDP packets that have been malformed.
%% @spec get_udp_malformed_in() -> integer
get_udp_malformed_in() -> gen_server:call(?MODULE, get_udp_malformed_in).

%% Returns the current number of UDP packets that have been sent.
%% @spec get_udp_out() -> integer
get_udp_out() -> gen_server:call(?MODULE, get_udp_out).

%% Returns the current number of UDP reponse timeouts that have occurred.
%% @spec get_udp_response_timeouts() -> integer
get_udp_response_timeouts() -> gen_server:call(?MODULE, get_udp_response_timeouts).

%% Returns all the packet stats as a record/tuple
%% @spec get_all() -> tuple
get_all() -> gen_server:call(?MODULE, get_all).

%% Prints a report of the current packet stats to the console.
report() -> gen_server:cast(?MODULE, report).

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
init([]) ->
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
handle_call(get_tcp_in, _From,  State = #state{tcp_in = Packets}) ->
    Reply = Packets,
    {reply, Reply, State};

handle_call(get_tcp_malformed_in, _From,  State = #state{tcp_malformed_in = Packets}) ->
    Reply = Packets,
    {reply, Reply, State};

handle_call(get_tcp_out, _From,  State = #state{tcp_out = Packets}) ->
    Reply = Packets,
    {reply, Reply, State};

handle_call(get_udp_in, _From,  State = #state{udp_in = Packets}) ->
    Reply = Packets,
    {reply, Reply, State};

handle_call(get_udp_malformed_in, _From,  State = #state{udp_malformed_in = Packets}) ->
    Reply = Packets,
    {reply, Reply, State};

handle_call(get_udp_out, _From,  State = #state{udp_out = Packets}) ->
    Reply = Packets,
    {reply, Reply, State};

handle_call(get_udp_response_timeouts, _From,  State = #state{udp_response_timeouts = Value}) ->
    Reply = Value,
    {reply, Reply, State};

handle_call(get_all, _From, State) ->
    Reply = State,
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
handle_cast(tcp_in, State = #state{tcp_in = Packets}) ->
    NewState = State#state{tcp_in = Packets+1},
    {noreply, NewState};

handle_cast(tcp_malformed_in, State = #state{tcp_malformed_in = Packets}) ->
    NewState = State#state{tcp_malformed_in = Packets+1},
    {noreply, NewState};

handle_cast(tcp_out, State = #state{tcp_out = Packets}) ->
    NewState = State#state{tcp_out = Packets+1},
    {noreply, NewState};

handle_cast(udp_in, State = #state{udp_in = Packets}) ->
    NewState = State#state{udp_in = Packets+1},
    {noreply, NewState};

handle_cast(udp_malformed_in, State = #state{udp_malformed_in = Packets}) ->
    NewState = State#state{udp_malformed_in = Packets+1},
    {noreply, NewState};

handle_cast(udp_out, State = #state{udp_out = Packets}) ->
    NewState = State#state{udp_out = Packets+1},
    {noreply, NewState};

handle_cast(udp_response_timeout, State = #state{udp_response_timeouts = Value}) ->
    NewState = State#state{udp_response_timeouts = Value+1},
    {noreply, NewState};

handle_cast(report, State = #state{tcp_in = TcpIn, tcp_malformed_in = TcpMalIn, tcp_out = TcpOut,
                                   udp_in = UdpIn, udp_malformed_in = UdpMalIn, udp_out = UdpOut,
                                   udp_response_timeouts = UdpResponseTimeouts}) ->
    io:format("Packet Status Report~n"
              "--------------------~n~n"
              "TCP Packets:       In:  ~p (of which ~p were malformed/ignored)~n"
              "                  Out:  ~p~n"
              "UDP Packets:       In:  ~p (of which ~p were malformed/ignored)~n"
              "                  Out:  ~p~n"
              "             Timeouts:  ~p~n", [TcpIn, TcpMalIn, TcpOut, UdpIn, UdpMalIn, UdpOut, UdpResponseTimeouts]),
    {noreply, State};

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

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

