%%% -------------------------------------------------------------------
%%% Author  : rob
%%% Description :
%%%
%%% Created : Dec 20, 2010
%%% -------------------------------------------------------------------
-module(ch_device_client).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("ch_global.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, enqueue_requests/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket = null}). % Network socket to target device

% Possible error codes for responses back to transaction managers
-define(SUCCESS_CODE, 0).
-define(TIMEOUT_CODE, 1).

%%% ====================================================================
%%% API functions (public interface)
%%% ====================================================================


%% ---------------------------------------------------------------------
%% @doc Start up a Device Client for a given target IP address (given
%%      as a 32-bit uint) and port number (16-bit uint).
%%
%% @spec start_link(IPaddrU32::integer(), PortU16::integer()) -> {ok, Pid} | {error, Error}
%% @end
%% ---------------------------------------------------------------------
start_link(IPaddrU32, PortU16) when is_integer(IPaddrU32), is_integer(PortU16) ->
    gen_server:start_link(?MODULE, [IPaddrU32, PortU16], []).


%% ---------------------------------------------------------------------
%% @doc Add some IPbus requests to the queue of the device client  
%%      dealing with the target hardware at the given IPaddr and Port.
%%      Note that the IP address is given as a raw unsigned 32-bit
%%      integer (no "192.168.0.1" strings, etc). Once the device client
%%      has dispatched the requests, the received responses will be
%%      forwarded to the caller of this function using the form:
%%        { device_client_response,
%%          TargetIPaddrU32::integer(),
%%          TargetPortU16::integer(),
%%          ErrorCodeU16::integer(),
%%          ResponseBin::binary() }
%%
%% @spec enqueue_requests(IPaddrU32::integer(),
%%                        PortU16::integer(),
%%                        IPbusRequests::binary()) -> ok
%% @end
%% ---------------------------------------------------------------------
enqueue_requests(IPaddrU32, PortU16, IPbusRequests) when is_binary(IPbusRequests) ->
    Pid = device_client_registry:get_pid(IPaddr, Port),
    gen_server:cast(Pid, {send, IPbusRequests, self()}),
    ok.
    
    

%%% ====================================================================
%%% Behavioural functions: gen_server callbacks
%%% ====================================================================


%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([IPaddrU32, PortU16]) ->
    % Put process constants in process dict.
    put(targetIP, IPaddrU32),
    put(targetPort, PortU16),
    % Tuple summarising the target address - useful for error/trace messages, etc.
    put(targetSummary, {device_client_target, {ipaddr,ch_utils:ipv4_addr_to_tuple(IPaddr)}, {port, Port}}),
    
    % Try opening ephemeral port and we want data delivered as a binary.
    case gen_udp:open(0, [binary]) of   
        {ok, Socket} -> {ok, #state{socket = Socket}};
        {error, Reason} when is_atom(Reason) ->
            ErrorMessage = {"Device client couldn't open UDP port to target",
                            get(targetSummary),
                            {errorCode, Reason}
                           },
            {stop, ErrorMessage};
        _ ->
            ErrorMessage = {"Device client couldn't open UDP port to target",
                            get(targetSummary),
                            {errorCode, unknown}
                           },
            {stop, ErrorMessage}
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
handle_cast({send, IPbusRequests, ClientPid}, State = #state{socket = Socket}) ->
    ?DEBUG_TRACE("IPbus requests received from Transaction Manager with PID = ~p.  Forwarding to ~p...", [ClientPid, get(targetSummary)]),
    ch_stats:udp_out(),
    ok = gen_udp:send(Socket, get(targetIP), get(targetPort), IPbusRequests),
    Reply = receive
                {udp, Socket, _, _, HardwareReplyBin} -> 
                    ?DEBUG_TRACE("Received response from ~p. Passing it to originating Transaction Manager...", [get(targetSummary)]),
                    ch_stats:udp_in(),
                    { device_client_response, get(targetIP), get(targetPort), ?SUCCESS_CODE, HardwareReplyBin} }
            after ?DEVICE_CLIENT_UDP_TIMEOUT ->
                ?DEBUG_TRACE("TIMEOUT REACHED! No response from ~p. Generating and sending a timeout "
                             "response to originating Transaction Manager...", [get(targetSummary)]),
                ch_stats:udp_response_timeout(),
                { device_client_response, get(targetIP), get(targetPort), ?TIMEOUT_CODE, <<>> }
            end,
    ClientPid ! Reply,
    {noreply, State};


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

