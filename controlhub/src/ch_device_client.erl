%%% ===========================================================================
%%% @author Robert Frazier
%%% @author Tom Williams
%%%
%%% @since May 2012
%%%
%%% @doc A device client forms a single point of contact with a particular
%%%      hardware target, and deals with all the UDP communication to that
%%%      particular target.  The interface simply allows you to queue
%%%      transactions for a particular target, with the relevant device client
%%%      being found (or spawned) behind the scenes.  Replies from the target
%%%      device are sent back as a message to the originating process.
%%% @end
%%% ===========================================================================
-module(ch_device_client).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("ch_global.hrl").
-include("ch_timeouts.hrl").
-include("ch_error_codes.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/3, enqueue_requests/3, increment_pkt_id/1, parse_ipbus_packet/1, state_as_string/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type mode() :: 'setup' | 'normal' | 'recover_lost_pkt' | 'timeout'.

-type ipbus_version() :: {1, 3} | {2, 0} | 'unknown'.

-type timestamp() :: { non_neg_integer() , non_neg_integer() , non_neg_integer() }.

-type in_flight_info() :: {<<_:32>>, timestamp(), binary(), non_neg_integer(), pid(), <<_:32>>} | {pid(), binary()}. %{ModHdr, now(), ModRequest, 0, ClientPid, OrigHdr},

-type queue() :: any().

%-type endness() :: big | little.

-record(state, {mode = setup                 :: mode(),
                socket,                      % Holds network socket to target device 
                local_port                   :: non_neg_integer(), 
                ip_tuple                     :: tuple(),
                ip_u32                       :: non_neg_integer(),
                port                         :: non_neg_integer(),
                udp_pid                      :: pid(),
                timeout                      :: non_neg_integer(),
                shutdown_after               :: non_neg_integer(),
                ipbus_v                      :: ipbus_version(), 
                next_id                      :: non_neg_integer(),
                in_flight = {0, queue:new()} :: {non_neg_integer(), queue()},
                queue = queue:new()          :: queue(),
                max_in_flight                :: non_neg_integer(),
                last_timeout,
                stats 
                }
        ).


-ifdef(old_erlang_time_api).

-define(GET_MONOTONIC_TIME, erlang:now() ).
-define(CALC_MONOTONIC_TIME_DIFF(T2,T1), timer:now_diff(T2,T1) ).

-else.

-define(GET_MONOTONIC_TIME, erlang:monotonic_time(micro_seconds) ).
-define(CALC_MONOTONIC_TIME_DIFF(T2,T1), (T2 - T1) ).

-endif.


-ifdef(orig_udp_port_command_format).
-define(GEN_UDP_PORT_COMMAND_PREFIX, []).
-else.
-define(GEN_UDP_PORT_COMMAND_PREFIX, [1]).
-endif.


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
start_link(IPaddrU32, PortU16, ChMaxInFlight) when is_integer(IPaddrU32), is_integer(PortU16), is_integer(ChMaxInFlight) ->
    gen_server:start_link(?MODULE, [IPaddrU32, PortU16, ChMaxInFlight], []).


%% ---------------------------------------------------------------------
%% @doc Add some IPbus requests to the queue of the device client  
%%      dealing with the target hardware at the given IPaddr and Port.
%%      Note that the IP address is given as a raw unsigned 32-bit
%%      integer (no "192.168.0.1" strings, etc). Once the device client
%%      has dispatched the requests, the received responses will be
%%      forwarded to the caller of this function using the form:
%%
%%        { device_client_response,
%%          TargetIPaddrU32::integer(),
%%          TargetPortU16::integer(),
%%          ErrorCodeU16::integer(),
%%          TargetResponse::binary() }
%%
%%      Currently ErrorCodeU16 will either be:
%%          0  :  Success, no error.
%%          1  :  Target response timeout reached.
%%
%%      If the the error code is not 0, then the TargetResponse will be
%%      an empty binary. 
%%
%% @spec enqueue_requests(IPaddrU32::integer(),
%%                        PortU16::integer(),
%%                        IPbusRequests::binary()) -> ok
%% @end
%% ---------------------------------------------------------------------
enqueue_requests(IPaddrU32, PortU16, IPbusRequest) when is_binary(IPbusRequest) ->
    {ok, Pid} = ch_device_client_registry:get_pid(IPaddrU32, PortU16),
    gen_server:cast(Pid, {send, IPbusRequest, self()}),
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
init([IPaddrU32, PortU16, ChMaxInFlight]) ->
    % Put process constants in process dict.
    put(target_ip_u32, IPaddrU32),
    IPTuple = ch_utils:ipv4_u32_addr_to_tuple(IPaddrU32),
    put(target_ip_tuple, IPTuple),   
    put(target_port, PortU16),
    ch_utils:log(debug, "Initialising device client for target at ~w.~w.~w.~w:~w. Absolute max nr packets in flight is ~w.",
                 [element(1, IPTuple), element(2, IPTuple), element(3, IPTuple), element(4, IPTuple), PortU16, ChMaxInFlight]),

    % Try opening ephemeral port and we want data delivered as a binary.
    case gen_udp:open(0, [binary, {active, true}, {buffer, 100000}, {recbuf, 100000}, {read_packets, 50}]) of   
        {ok, Socket} ->
            put(socket, Socket),
            put(abs_max_in_flight, ChMaxInFlight),
            StatsTable = ch_stats:new_device_client_table(IPTuple,PortU16),
            put(stats, StatsTable),
            UdpPid = spawn_link(fun udp_proc_init/0),
            put(udp_pid, UdpPid),
            gen_udp:controlling_process(Socket, UdpPid),
            UdpPid ! {start, Socket, IPTuple, PortU16, self()},
            LocalPort = case inet:port(Socket) of
                {ok, PortNr} -> PortNr;
                Other -> Other
            end,
            {ok, #state{socket=Socket, local_port=LocalPort, ip_tuple=IPTuple, ip_u32=IPaddrU32, port=PortU16, udp_pid=UdpPid, timeout=ch_config:get(device_response_timeout), shutdown_after=ch_config:get(device_client_shutdown_after), last_timeout=none, stats=StatsTable} };
        {error, Reason} when is_atom(Reason) ->
            ErrorMessage = {"Device client couldn't open UDP port to target",
                            target_ip_port(),
                            {errorCode, Reason}
                           },
            {stop, ErrorMessage};
        _ ->
            ErrorMessage = {"Device client couldn't open UDP port to target",
                            target_ip_port(),
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
    ch_utils:log(warning, "Unexpected call received : ~p~n~s", [_Request, state_as_string(State)]),
    Reply = ok,
    {reply, Reply, State, State#state.shutdown_after}.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

% handle_cast callback for enqueue_requests API call.
handle_cast({send, RequestPacket, ClientPid}, S) ->
    ?CH_LOG_DEBUG("IPbus request packet received from Transaction Manager with PID = ~w.", [ClientPid]),
    {ReqIPbusVer, _Type, _End} = parse_ipbus_packet(RequestPacket),
    case get_device_status(ReqIPbusVer, S#state.timeout) of
        {ok, {1,3}, {}, _} ->
            ch_utils:log({info,log_prefix(S)}, "Device client intialised, IPbus1.3 target"),
            NewS = S#state{mode = normal,
                           ipbus_v = {1,3},
                           max_in_flight = 1,
                           next_id = void
                          },
            device_client_loop(recv, send_request(RequestPacket, ClientPid, NewS) );
        {ok, {2,0}, {_MTU, TargetNrBuffers, NextExpdId}, _} ->
            ch_utils:log({info,log_prefix(S)}, "Device client initialised, IPbus2.0 target (MTU=~w bytes, NrBuffers=~w, NextExpdId=~w)", [_MTU, TargetNrBuffers, NextExpdId]),
            NewS = S#state{mode = normal,
                           ipbus_v = {2,0},
                           max_in_flight = min(TargetNrBuffers, get(abs_max_in_flight)),
                           next_id = NextExpdId
                          },
            device_client_loop(send, send_request(RequestPacket, ClientPid, NewS) );
        {error, _, MsgForTransManager, _} ->
            ch_utils:log({info,log_prefix(S)}, "Target didn't respond correctly to status request in ch_device_client:init/1."),
            ClientPid ! MsgForTransManager,
            reply_to_all_pending_requests_in_msg_inbox(MsgForTransManager),
            {stop, normal, S}
    end;

%% Default handle cast
handle_cast(_Msg, State) -> 
    ch_utils:log(warning, "Unexpected cast received : ~p~n~s", [_Msg, state_as_string(State)]),
    {nostop, wrong_clause, State}.


% Send/receive clause
%   Takes messages from front
device_client_loop(send, S = #state{ socket=Socket, ip_tuple=IP, port=Port, in_flight={NrInFlight, _} }) when NrInFlight < S#state.max_in_flight ->
    Timeout = if
                NrInFlight =/= 0 -> S#state.timeout;
                true -> S#state.shutdown_after
              end,
    receive
        {'$gen_cast', {send, Pkt, Pid}} ->
            NewS = send_request(Pkt, Pid, S),
            device_client_loop(recv, NewS);
        {udp, Socket, IP, Port, Pkt} ->
            NewS = forward_reply(Pkt, S),
            device_client_loop(send, NewS);
        Other ->
            ch_utils:log(warning, "Unexpected message received : ~p ~n~s", [Other, state_as_string(S)]),
            device_client_loop(send, S)
    after Timeout ->
        if
          NrInFlight =/= 0 ->
            device_client_loop(timeout, S);
          true ->
            ch_utils:log(info, "No communication has passed through this device client in ~pms ; shutting down now~s~n", [S#state.shutdown_after, state_as_string(S)]),
            {stop, normal, S}
        end
    end;

device_client_loop(send, S) ->
    device_client_loop(recv, S);

% Non-blocking receive clause
%   Looks through msg queue
device_client_loop(recv, S = #state{ socket=Socket, ip_tuple=IP, port=Port, in_flight={NrInFlight,_} }) when NrInFlight < S#state.max_in_flight ->
    receive
        {udp, Socket, IP, Port, Pkt} ->
            NewS = forward_reply(Pkt, S),
            device_client_loop(recv, NewS)
    after 0 ->
        device_client_loop(send, S)
    end;

% Blocking receive clause
%   Looks through msg queue
device_client_loop(recv, S = #state{ socket=Socket, ip_tuple=IP, port=Port }) ->
    receive
        {udp, Socket, IP, Port, Pkt} ->
            NewS  = forward_reply(Pkt, S),
            device_client_loop(send, NewS)
    after S#state.timeout ->
        device_client_loop(timeout, S)
    end;

% Response timeout clause
device_client_loop(timeout, S) ->
    ch_utils:log({debug,log_prefix(S)}, "Timeout when waiting for control packet response. Starting packet-loss recovery ..."),
    case recover_from_timeout(0, S) of
        {ok, NewState} ->
            ch_utils:log({debug,log_prefix(S)}, "Timeout recovered!"),
            device_client_loop(send, NewState);
        {error, NewState, MsgForTransactionManager} ->
            S#state.udp_pid ! {print_incident_packets_then_exit},
            Now = ?GET_MONOTONIC_TIME,
            {LostPktId, NrInFlight, RecoveryInfoList} = NewState#state.last_timeout,
            ch_utils:log({error,log_prefix(S)}, "Irrecoverable timeout for control packet ID ~w, with ~w in flight. ~w recovery attempts, timeout ~wms~s",
                         [LostPktId, NrInFlight, length(RecoveryInfoList), S#state.timeout, 
                         lists:flatten([io_lib:format("; ~.1fms ago, ~w lost", [?CALC_MONOTONIC_TIME_DIFF(Now,Timestamp) / 1000, Type]) || {Timestamp, Type, _} <- RecoveryInfoList])]),
            ReqPkt = case S#state.ipbus_v of
                          {1, 3} ->
                              element(2, element(2, S#state.in_flight));
                          {2, 0} ->
                              {value, {_, _, ReqHdr, ReqBody}} = queue:peek(element(2, S#state.in_flight)),
                              <<ReqHdr/binary, ReqBody/binary>>
                      end,
            ch_utils:log({error,log_prefix(S)}, "Irrecoverable timeout for control packet ID ~w - request packet was ~s", [LostPktId, convert_binary_to_string(ReqPkt, "  0x")]),
            case RecoveryInfoList of
                [{_, _, StatusReplyPkt} | _] ->
                    ch_utils:log({error,log_prefix(S)}, "Irrecoverable timeout for control packet ID ~w - last status packet was ~s", [LostPktId, convert_binary_to_string(StatusReplyPkt, "  0x")]);
                _ -> void 
            end,
            Pids = case S#state.ipbus_v of
                       {1, 3} ->
                           [element(1, element(2, S#state.in_flight))];
                       {2, 0} ->
                           [Pid || {Pid, _, _, _} <- queue:to_list(element(2,S#state.in_flight))]
                   end,
            lists:foreach(fun(Pid) -> Pid ! MsgForTransactionManager end, Pids),
            reply_to_all_pending_requests_in_msg_inbox(MsgForTransactionManager),
            {stop, normal, NewState}            
    end.


send_request(ReqPkt, Pid, S = #state{next_id=NextId, in_flight={NrInFlight,InFlightQ}}) when S#state.ipbus_v == {2,0} ->
    case parse_ipbus_control_pkt(ReqPkt) of
        {{2,0}, Endness} ->
            ?CH_LOG_DEBUG("IPbus 2.0 request packet from PID ~w is being forwarded to board at ~s.", [Pid, ch_utils:ip_port_string(S#state.ip_tuple, S#state.port)]),
            <<OrigHdr:4/binary, Body/binary>> = ReqPkt,
            NewHdr = ipbus2_pkt_header(NextId, Endness),
            S#state.udp_pid ! {send, [NewHdr, Body]},
            ch_stats:udp_sent(S#state.stats),
            S#state{ next_id=increment_pkt_id(NextId), in_flight={NrInFlight+1, queue:in({Pid, OrigHdr, NewHdr, Body}, InFlightQ)} };
        Other ->
            ch_utils:log(warning, "Request packet from PID ~w is invalid for an IPbus 2.0 target, and is being ignored. parse_ipbus_control_pkt returned ~w", [Pid, Other]), 
            Pid ! {device_client_response, S#state.ip_u32, S#state.port, ?ERRCODE_WRONG_PROTOCOL_VERSION, <<>>},
            S
    end;
send_request(ReqPkt, Pid, S) when S#state.ipbus_v == {1,3} ->
    case parse_ipbus_control_pkt(ReqPkt) of
        {{1,3}, _} ->
            ?CH_LOG_DEBUG("IPbus 1.3 request packet from PID ~w is being forwarded to board at ~s.", [Pid, ch_utils:ip_port_string(S#state.ip_tuple, S#state.port)]),
            S#state.udp_pid ! {send, ReqPkt},
            ch_stats:udp_sent(S#state.stats),
            S#state{in_flight={1, {Pid, ReqPkt}}};
        Other ->
            ch_utils:log(warning, "Request packet from PID ~w is invalid for an IPbus 1.3 target, and is being ignored. parse_ipbus_control_pkt returned ~w", [Pid, Other]),
            Pid ! {device_client_response, S#state.ip_u32, S#state.port, ?ERRCODE_WRONG_PROTOCOL_VERSION, <<>>},
            S
    end.


forward_reply(Pkt, S = #state{ in_flight={NrInFlight,InFlightQ} }) when S#state.ipbus_v == {2,0}, NrInFlight>0 ->
    {{value, SentPktInfo = {Pid,OrigHdr,SentHdr,_}}, NewQ} = queue:out(InFlightQ),
    case Pkt of 
        <<SentHdr:4/binary, ReplyBody/binary>> ->
            ?CH_LOG_DEBUG("IPbus 2.0 reply from ~s is being forwarded to PID ~w", [ch_utils:ip_port_string(S#state.ip_tuple,S#state.port), Pid]),
            Pid ! {device_client_response, S#state.ip_u32, S#state.port, ?ERRCODE_SUCCESS, [OrigHdr, ReplyBody]},
            ch_stats:udp_rcvd(S#state.stats),
            S#state{ in_flight={NrInFlight-1,NewQ} };
        <<RcvdHdr:4/binary, _/binary>> ->
            ?CH_LOG_DEBUG("Ignoring received packet with incorrect header (expecting header ~w, could just be genuine out-of-order reply): ~w", [SentHdr, RcvdHdr]),
            S#state{ in_flight={NrInFlight,queue:in_r(SentPktInfo, NewQ)} }
    end;
forward_reply(Pkt, S = #state{ in_flight={1,{TransManagerPid,_}} }) when S#state.ipbus_v == {1,3} ->
    ?CH_LOG_DEBUG("IPbus reply from ~s is being forwarded to PID ~w", [ch_utils:ip_port_string(S#state.ip_tuple,S#state.port), TransManagerPid]),
    TransManagerPid ! {device_client_response, S#state.ip_u32, S#state.port, ?ERRCODE_SUCCESS, Pkt},
    ch_stats:udp_rcvd(S#state.stats),
    S#state{in_flight={0,{void,void}}};
forward_reply(Pkt, S) ->
    LastTimoutSummary = case S#state.last_timeout of
                            none ->
                                "No previous timeouts.";
                            {LostPktId, NrInFlight, [ {Timestamp, Type, _StatusReplyPkt} | AttemptInfoTail]} ->
                                io_lib:format("Last timeout was ~.1fms ago, ~w packet ID ~w lost, ~w in flight, ~w recovery attempts.", 
                                              [?CALC_MONOTONIC_TIME_DIFF(?GET_MONOTONIC_TIME,Timestamp) / 1000, Type, LostPktId, NrInFlight, length(AttemptInfoTail)+1]);
                            {LostPktId, NrInFlight, []} ->
                                io_lib:format("Last timeout was for packet ID ~w, ~w in flight.", [LostPktId, NrInFlight])
                            end,
    ch_utils:log({error,log_prefix(S)}, "Not expecting any response from board, but received UDP packet: ~s. ~s", [convert_binary_to_string(Pkt, 12, "  0x"), LastTimoutSummary]),
    S.


recover_from_timeout(NrFailedAttempts, S = #state{socket=Socket, ip_tuple=IP, port=Port}) when S#state.ipbus_v == {1,3} ->
    ch_utils:log({debug,log_prefix(S)}, "IPbus 1.3 target, so wait an extra ~w ms for reply packet to come.", [S#state.timeout]),
    NewS = case NrFailedAttempts of
                 0 ->
                     S#state{last_timeout={0, 1, [{?GET_MONOTONIC_TIME, unknown, <<>>}]}};
                 _ ->
                     {_, _, AttemptInfoList} = S#state.last_timeout,
                     S#state{last_timeout={0, 1, [{?GET_MONOTONIC_TIME, unknown, <<>>} | AttemptInfoList]}}
           end,
    receive
        {udp, Socket, IP, Port, Pkt} ->
            {ok, forward_reply(Pkt, NewS)}
    after S#state.timeout ->
        if 
          (NrFailedAttempts+1) == 3 ->
            {error, NewS, {device_client_response, S#state.ip_u32, S#state.port, ?ERRCODE_TARGET_CONTROL_TIMEOUT, <<>>}};
          true ->
            recover_from_timeout(NrFailedAttempts + 1, NewS)
        end
    end;

recover_from_timeout(NrFailedAttempts, S = #state{next_id=NextId, in_flight={NrInFlight,InFlightQ}, socket=Socket, ip_tuple=IP, port=Port}) when S#state.ipbus_v == {2,0} ->
    NextIdMinusN = decrement_pkt_id(NextId, NrInFlight),
    RequestsInFlight = [[NewHdr, Body] || {_Pid, _OrigHdr, NewHdr, Body} <- queue:to_list(InFlightQ)],
    ch_stats:udp_timeout(S#state.stats, if NrFailedAttempts==0 -> normal; true -> resend end),
    % Take recovery action
    case get_device_status(S#state.ipbus_v, S#state.timeout) of
        {ok, {2,0}, {_, _, HwNextId}, StatusReplyPkt} ->
            TypeLost = case HwNextId of NextIdMinusN -> request; _-> response end,
            if NrFailedAttempts==0 -> ch_stats:udp_lost(S#state.stats, TypeLost); true -> void end,
            % Re-send request packets, or request that responses are re-sent
            case TypeLost of 
                request ->
                    ch_utils:log({info,log_prefix(S)}, 
                                 "Re-sending ~w lost request packets (~w in flight, next ID: ~w without loss, ~w in device); attempt ~w.", 
                                 [NrInFlight, NrInFlight, NextId, HwNextId, NrFailedAttempts+1]),
                    lists:foreach(fun(RequestIoData) -> S#state.udp_pid ! {send, RequestIoData}, 
                                                        ch_stats:udp_sent(S#state.stats) end, 
                                  RequestsInFlight
                                 );
                response ->
                    ch_utils:log({info,log_prefix(S)}, 
                                 "Requesting re-send of ~w lost response packets (~w in flight, next ID: ~w without loss, ~w in device); attempt ~w", 
                                 [NrInFlight, NrInFlight, NextId, HwNextId, NrFailedAttempts+1]),
                    lists:foreach(fun([ReqHdr, _ReqBody]) -> S#state.udp_pid ! {send, resend_request_pkt(ReqHdr)},
                                                             ch_stats:udp_sent(S#state.stats) end, 
                                   RequestsInFlight
                                 )
            end,
            NewS = case NrFailedAttempts of
                       0 ->
                           S#state{last_timeout={NextIdMinusN, NrInFlight, [{?GET_MONOTONIC_TIME, TypeLost, StatusReplyPkt}]}};
                       _ ->
                           {_, _, AttemptInfoList} = S#state.last_timeout,
                           S#state{last_timeout={NextIdMinusN, NrInFlight, [{?GET_MONOTONIC_TIME, TypeLost, StatusReplyPkt} | AttemptInfoList]}}
                   end,
            % Check whether recovered from timeout or not
            {value, {_, _, ExpdHdr, _}} = queue:peek(InFlightQ),
            receive
                {udp, Socket, IP, Port, Pkt = <<ExpdHdr:4/binary, _/binary>>} ->
                    ch_stats:udp_timeout(S#state.stats, recovered),
                    {ok, forward_reply(Pkt, NewS)}
            after S#state.timeout ->
                if 
                  (NrFailedAttempts+1) == 5 ->
                    ErrorCode = case TypeLost of response -> ?ERRCODE_TARGET_RESEND_TIMEOUT; request -> ?ERRCODE_TARGET_CONTROL_TIMEOUT end,
                    {error, NewS, {device_client_response, S#state.ip_u32, S#state.port, ErrorCode, <<>>}};
                  true ->
                    recover_from_timeout(NrFailedAttempts + 1, NewS)
                end
            end;
        {error, Reason, MsgForTransManager, ReplyStatusPkt} ->
            case Reason of
                timeout ->
                    ch_utils:log({error,log_prefix(S)},
                                 "Status request timed out after control packet timeout (~w in flight, next expected ID should be ~w), recovery attempt ~w.", 
                                 [NrInFlight, NextId, NrInFlight]);
                malformed ->
                    ch_utils:log({error,log_prefix(S)},
                                 "Malformed status packet recieved when trying to recover from control packet timeout (~w in flight, next expected ID should be ~w), recovery attempt ~w. Malformed packet was ~s", 
                                 [NrInFlight, NextId, NrInFlight, convert_binary_to_string(ReplyStatusPkt, byte_size(ReplyStatusPkt), "  0x")])
            end,
            {error, S#state{last_timeout={NextIdMinusN, NrInFlight, []}}, MsgForTransManager}
    end.


reply_to_all_pending_requests_in_msg_inbox(ReplyMsg) ->
    receive
        {'$gen_cast', {send, _Pkt, Pid}} ->
            Pid ! ReplyMsg,
            reply_to_all_pending_requests_in_msg_inbox(ReplyMsg)
    after 0 ->
        void
    end.



%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

% Dummy handle_info callback
handle_info(_Info, State) ->
    ch_utils:log(warning, "Unexpected handle_info message received : ~p~n~s", [_Info, State]),
    {stop, wrong_clause, State}.


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



%% ---------------------------------------------------------------------
%% @doc Resets the first 4 bytes of a binary
%% @spec reset_header(Packet::<<_:32>>, CurrentHdr::<<_:32>>, NewHdr::<<_:32>>) -> binary()
%%
%% @end
%% ---------------------------------------------------------------------

reset_header(Packet, NewHdr) when is_binary(Packet), size(Packet)>3, is_binary(NewHdr), size(NewHdr)>3 ->
    <<_:4/binary, Body/binary>> = Packet,
    [NewHdr, Body].


%% ---------------------------------------------------------------------
%% @doc Returns version and endianness of an IPbus 1.3/2.0 control packet
%% @spec parse_ipbus_control_pkt( Pkt :: binary() ) -> {ipbus_version(), (big || little)} || invalid
%%
%% @end
%% ---------------------------------------------------------------------

parse_ipbus_control_pkt( <<16#20:8, _Id:16/big, 16#f0:8, _/binary>> ) ->
    {{2,0}, big};
parse_ipbus_control_pkt( <<16#f0:8, _Id:16/little, 16#20:8, _/binary>> ) ->
    {{2,0}, little};
parse_ipbus_control_pkt( <<1:4, _:12, 0:8, 16#1f:5, _:1, 0:2, _/binary>> ) ->
    {{1,3}, big};
parse_ipbus_control_pkt( <<16#1f:5, _:1, 0:2, 0:8, _:8, 1:4, _:4, _/binary>> ) ->
    {{1,3}, little};
parse_ipbus_control_pkt( _ ) ->
    invalid.


%% ---------------------------------------------------------------------
%% @doc Returns IPbus 2.0 control packet header as binary
%% @spec ipbus2_pkt_header(Id :: integer(), Endness :: (big || little) ) -> binary()
%%
%% @end
%% ---------------------------------------------------------------------

ipbus2_pkt_header(Id, big) ->
    <<16#20:8, Id:16/big, 16#f0>>;
ipbus2_pkt_header(Id, little) ->
    <<16#f0:8, Id:16/little, 16#20>>.


%% ---------------------------------------------------------------------
%% @doc Decrements packet ID looping round from 1 to 0xffff
%% @spec decrement_pkt_id( Id::integer() ) -> IdMinusOne::integer()
%% @end
%% ---------------------------------------------------------------------

decrement_pkt_id(Id) when is_integer(Id), Id>0 ->
    if
      Id =:= 1 ->
        16#ffff;
      true ->
        Id - 1
    end.


%% ---------------------------------------------------------------------
%% @doc Decrements packet ID N times, looping round from 1 to 0xffff
%% @spec decrement_pt_id( Id :: pos_integer(), N :: pos_integer() ) -> IdPlusN :: pos_integer()
%% @end
%% ---------------------------------------------------------------------

decrement_pkt_id(Id, 0) ->
    Id;
decrement_pkt_id(Id, N) when is_integer(Id), Id>0, is_integer(N), N>0 ->
    decrement_pkt_id( decrement_pkt_id(Id) , N-1 ).


%% ---------------------------------------------------------------------
%% @doc Increments packet ID looping round from 0xffff to 1
%% @spec increment_pkt_id( Id::pos_integer() ) -> IdPlusOne::pos_integer()
%% @end
%% ---------------------------------------------------------------------

increment_pkt_id(Id) when is_integer(Id), Id>0 ->
    if
      Id=:=16#ffff ->
        1;
      true ->
        Id + 1
    end.


%% ---------------------------------------------------------------------
%% @doc Parses an IPbus 1.3 or 2.0 packet; typically only looks at the header.
%% @spec parse_ipbus_packet( binary() ) -> {ipbus_version(), Type, endness()} | malformed
%% where
%%       Type = {control, integer() | void}
%%            | {status, request}
%%            | {status, MTU::pos_integer(), NBuffers::pos_integer(), NextId::pos_integer()}
%%            | {resend, Id :: pos_integer()}
%% @end
%% ---------------------------------------------------------------------

parse_ipbus_packet(PacketBin) when is_binary(PacketBin) ->
    PktSize = size(PacketBin),
    case PacketBin of
        <<16#20:8/big, Id:16/big, 15:4/big, Type:4/big, _/binary>> ->
            if
              Type =:= 0 ->
                {{2,0}, {control, Id}, big};
              (Type =:= 1) and (Id =:= 0) and (PktSize =:= 64) ->
                parse_ipbus_packet({status, PacketBin});
              (Type =:= 2) and (PktSize =:= 4) ->
                {{2,0}, {resend, Id}, big};
              true ->
                malformed
            end;
        <<15:4/big, Type:4/big, Id:16/little, 16#20:8/big, _/binary>> ->
            if
              Type =:= 0 ->
                {{2,0}, {control, Id}, little};
              (Type =:= 1) and (PktSize =:= 0) ->
                {{2,0}, {resend, Id}, little};
              true ->
                malformed
            end;
        <<1:4/big, _:12, 0:8, 16#1f:5, _:1, 0:2, _/binary>> ->
            {{1,3}, {control, void}, big};
        <<16#1f:5/big, _:1, 0:2, 0:8, _:8, 1:4/big, _:4, _/binary>> ->
            {{1,3}, {control, void}, little}
    end;

parse_ipbus_packet({status, Bin}) when is_binary(Bin) ->
    case Bin of
        <<16#200000f1:32/big, 0:(15*32)/big>> ->
            {{2,0}, {status, request}, big};
        <<16#200000f1:32/big, MTU:32/big, NBuffers:32/big,
          16#20:8, NextId:16, 16#f0:8, _:(12*32)/big>> ->
            {{2,0}, {status, MTU, NBuffers, NextId}, big}
    end.

%% ------------------------------------------------------------------------------------
%% @doc Returns re-send request packet with specified packet ID and endian-ness
%% @spec resend_request_pkt(Id :: integer(), End ) -> Pkt::<<_:32>>
%% where
%%       End = big | little
%% @end
%% ------------------------------------------------------------------------------------
resend_request_pkt(<<16#20:8, Id:16/big, 16#f0:8, _/binary>>) ->
    resend_request_pkt(Id);
resend_request_pkt(<<16#f0:8, Id:16/little, 16#20:8, _/binary>>) ->
    resend_request_pkt(Id);
resend_request_pkt(Id) when Id =< 16#ffff, Id >= 0 ->
    Value = (2 bsl 28) + (Id bsl 8) + 16#f2,
    <<Value:32/big>>.


%% ------------------------------------------------------------------------------------
%% @doc Retrieves number of response buffers and next expected packet ID for the
%%       device, assuming it's an IPbus 2.0 device. Returns tuple beginning with
%%       atom error in case there was a timeout or response was malformed.
%% @spec get_device_status( IPbusVer :: ipbus_version() )
%%                             -> {ok, {1,3}, {}, StatusReplyPacket}
%%                              | {ok, {2,0}, {NrResponseBuffers, NextExpdId}, StatusReplyPacket}
%%                              | {error, malformed, MsgForTransManager, StatusReplyPacket}
%%                              | {error, timeout, MsgForTransManager, StatusReplyPacket}
%% @end
%% ------------------------------------------------------------------------------------

get_device_status(IPbusVer, Timeout) ->
    get_device_status(IPbusVer, 5, Timeout).


%% ------------------------------------------------------------------------------------
%% @doc Determines protocol version of device and other "status" information (such as
%%       number of response buffers and next expected packet ID. Returns tuple beginning
%%       with atom error in case there was a timeout or response was malformed.
%% @spc get_device_status( IPbusVer :: ipbus_version(), NrAttemptsLeft :: non_neg_integer() )
%%                                         -> {ok, {1,3}, {}, StatusReplyPacket}
%%                                          | {ok, {2,0}, {MTU, NrResponseBuffers, NextExpdId}, StatusReplyPacket}
%%                                          | {error, malformed, MsgForTransManager, StatusReplyPacket}
%%                                          | {error, timeout, MsgForTransManager, <<>>}
%%
%% @end
%% ------------------------------------------------------------------------------------

get_device_status(IPbusVer, {NrAttemptsLeft, TotNrAttempts}, Timeout) when NrAttemptsLeft =:= 0 ->
    ch_utils:log(info, "No response from target after ~w IPbus ~w status requests, each with timeout of ~wms.",
                  [TotNrAttempts, IPbusVer, Timeout]),
    {error, timeout, {device_client_response, get(target_ip_u32), get(target_port), ?ERRCODE_TARGET_STATUS_TIMEOUT, <<>>}, <<>>};

get_device_status(IPbusVer, NrAttemptsLeft, Timeout) when is_integer(NrAttemptsLeft), NrAttemptsLeft > 0 ->
    get_device_status(IPbusVer, {NrAttemptsLeft, NrAttemptsLeft}, Timeout);

get_device_status(IPbusVer, {NrAttemptsLeft, TotNrAttempts}, Timeout) when is_integer(NrAttemptsLeft), NrAttemptsLeft > 0 ->
    AttemptNr = TotNrAttempts - NrAttemptsLeft + 1,
    Socket = get(socket),
    UdpPid = get(udp_pid),
    {TargetIPTuple, TargetPort} = target_ip_port(),
    StatusReq13 = binary:copy(<<16#100000f8:32/native>>, 10),
    StatusReq20 = <<16#200000f1:32/big, 0:(15*32)>>,
    ?CH_LOG_DEBUG("Sending IPbus status request to target (attempt ~w of ~w).",
                 [AttemptNr, TotNrAttempts]),
    case IPbusVer of
         {1,3} when NrAttemptsLeft=:=TotNrAttempts ->
             UdpPid ! {send, StatusReq13},
%             gen_udp:send(Socket, TargetIPTuple, TargetPort, StatusReq13 ),
             ch_stats:udp_sent(get(stats));
         {1,3} ->
             void;
         {2,0} ->
             UdpPid ! {send, StatusReq20},
%             gen_udp:send(Socket, TargetIPTuple, TargetPort, StatusReq20 ),
             ch_stats:udp_sent(get(stats));
         unknown ->
             UdpPid ! {send, StatusReq20},
%             gen_udp:send(Socket, TargetIPTuple, TargetPort, StatusReq20 ),
             timer:sleep(2),
             UdpPid ! {send, StatusReq13},
%             gen_udp:send(Socket, TargetIPTuple, TargetPort, StatusReq13 ),
             ch_stats:udp_sent(get(stats))
    end,
    receive
        {udp, Socket, TargetIPTuple, TargetPort, <<16#100000fc:32/native, _/binary>> = ReplyBin} ->
            ch_utils:log(debug, "Received an IPbus 1.3 'status response' from target, on attempt ~w of ~w.",
                         [AttemptNr, TotNrAttempts]),
            ch_stats:udp_rcvd(get(stats)),
            {ok, {1,3}, {}, ReplyBin};
        {udp, Socket, TargetIPTuple, TargetPort, <<16#200000f1:32/big, _/binary>> = ReplyBin} ->
            ch_stats:udp_rcvd(get(stats)),
            case parse_ipbus_packet(ReplyBin) of 
                {{2,0}, {status, MTU, NBuffers, NextId}, big} ->
                    ch_utils:log(debug,"Received an IPbus 2.0 'status response' from target, on attempt ~w of ~w. MTU=~w, NBuffers=~w, NextExpdId=~w",
                                 [AttemptNr, TotNrAttempts, MTU, NBuffers, NextId]),
                    {ok, {2,0}, {MTU, NBuffers, NextId}, ReplyBin};
                _Details ->
                    ch_utils:log(warning, "Received a malformed IPbus 2.0 'status response' (correct IPbus packet header, but body wrong format). parse_ipbus_packet returned ~w",
                                 [_Details]),
                    {error, malformed, {device_client_response, TargetIPTuple, TargetPort, ?ERRCODE_MALFORMED_STATUS, <<>>}, ReplyBin}
            end
    after Timeout ->
        ch_utils:log(debug, "TIMEOUT waiting for response in get_device_status! No response from target on attempt ~w of ~w, ipbus version ~w.",
                     [AttemptNr, TotNrAttempts, IPbusVer]),
        ch_stats:udp_timeout(get(stats), status),
        get_device_status(IPbusVer, {NrAttemptsLeft-1, TotNrAttempts}, Timeout)
    end.


%% ------------------------------------------------------------------------------------
%% @doc Returns tuple containing target IP tuple and port retrieved from target_ip_tuple
%%       and target_port entries in process dict
%% @spec target_ip_port() -> {TargetIPTuple, TargetPort}
%% @end
%% ------------------------------------------------------------------------------------
target_ip_port() ->
    {get(target_ip_tuple), get(target_port)}.


%% ------------------------------------------------------------------------------------
%% @doc Returns string summarising state record in nice easy to read (multi-line) format
%%
%% @spec state_as_string( State::string() ) -> string()
%% @end
%% ------------------------------------------------------------------------------------

state_as_string(S) when is_record(S, state) ->
    io_lib:format(" State:  mode = ~w   ||   ipbus version = ~w   ||   next_id = ~w~n"
                  "         target is at ~w port ~w~n"
                  "         number in-flight = ~w   (max = ~w)~n"
                  "         number queued    = ~w~n",
                  [S#state.mode, S#state.ipbus_v, S#state.next_id, 
                   S#state.ip_tuple, S#state.port,
                   element(1, S#state.in_flight), S#state.max_in_flight,
                   queue:len(S#state.queue)]
                  ).


log_prefix(State = #state{local_port=LocalPort, ip_tuple={IP1,IP2,IP3,IP4}, port=TargetPort, next_id=NextPktId}) when is_record(State, state) ->
    io_lib:format("Device(~w-~w.~w.~w.~w:~w, next ID ~w)", [LocalPort, IP1,IP2,IP3,IP4, TargetPort, NextPktId]).



convert_binary_to_string(Bin, WordPrefix) when is_binary(Bin), is_list(WordPrefix) ->
    convert_binary_to_string(Bin, byte_size(Bin), WordPrefix).

convert_binary_to_string(Bin, NrBytes, WordPrefix) when is_binary(Bin), is_integer(NrBytes) ->
    convert_binary_to_string(Bin, {NrBytes, 0}, WordPrefix, [io_lib:format("~w bytes: ", [byte_size(Bin)])]).

convert_binary_to_string(Bin, _, _Prefix, StringAcc) when byte_size(Bin) < 1 ->
    lists:flatten(lists:reverse(StringAcc));
convert_binary_to_string(_Bin, {0, _}, _Prefix, StringAcc) ->
    lists:flatten(lists:reverse([" ... " | StringAcc]));
convert_binary_to_string(<<FirstByte:8, RestOfBin/binary>>, {NrBytesToRead, NrBytesDone}, WordPrefix, StringAcc) ->
    BytePrefix = case (NrBytesDone rem 4) of
                 0 -> WordPrefix;
                 _ -> ""
             end,
    convert_binary_to_string(RestOfBin, {NrBytesToRead-1, NrBytesDone+1}, WordPrefix, [io_lib:format("~2.16.0B", [FirstByte]), io_lib:format(BytePrefix, []) | StringAcc]).



udp_proc_init() ->
    process_flag(trap_exit, true),
    receive
        {start, Socket, IP, Port, ParentPid} ->
            link(ParentPid),
            put(target_ip_tuple, IP),
            put(target_port, Port),
            udp_proc_loop(Socket, IP, Port, ParentPid)
    end.


udp_proc_loop(Socket, IP, Port, ParentPid) ->
    receive
        {udp, Socket, IP, Port, Packet} ->
            ParentPid ! {udp, Socket, IP, Port, Packet}
    after 0 ->
        receive 
            {send, Pkt} ->
                udp_send(Socket, IP, Port, Pkt);
            {inet_reply, Socket, ok} ->
                void;
            {inet_reply, Socket, SendError} ->
                ch_utils:log(error, "Error in UDP async send: ~w", [SendError]);%,
                %throw({udp_send_error, SendError});
            {print_incident_packets_then_exit} ->
                {IP1, IP2, IP3, IP4} = IP,
                ch_utils:log(warning, "UDP proc for ~w.~w.~w.~w:~w entering 'print incident packets then exit' mode", [IP1, IP2, IP3, IP4, Port]),
                receive
                    {udp, Socket, IP, Port, Packet} ->
                        ch_utils:log(warning, "UDP proc received packet ~s", [convert_binary_to_string(Packet, 16, "  0x")])
                    after 10000 -> void
                end,
                ch_utils:log(warning, "UDP proc for ~w.~w.~w.~w:~w exiting 'print incident packets then exit' mode", [IP1, IP2, IP3, IP4, Port]),
                exit(normal);
            {'EXIT', ParentPid, Reason} ->
                Level = case Reason of 
                            normal -> debug;
                            _ -> notice
                        end,
                ch_utils:log(Level, "UDP proc shutting down since parent device client ~w terminated with reason: ~w", [ParentPid, Reason]),
                exit(normal);
            Other ->
                ParentPid ! Other
        end 
    end,
    udp_proc_loop(Socket, IP, Port, ParentPid).


-ifdef(bypass_gen_udp_send).
udp_send(Socket, IP, Port, Pkt) ->
    {IP1, IP2, IP3, IP4} = IP,
    true = erlang:port_command(Socket, [?GEN_UDP_PORT_COMMAND_PREFIX, [((Port) bsr 8) band 16#ff, (Port) band 16#ff], [IP1 band 16#ff, IP2 band 16#ff, IP3 band 16#ff, IP4 band 16#ff], Pkt]).
-else.
udp_send(Socket, IP, Port, Pkt) ->
    ok = gen_udp:send(Socket, IP, Port, Pkt).
-endif.
