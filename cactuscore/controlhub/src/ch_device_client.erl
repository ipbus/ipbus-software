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
-export([start_link/3, enqueue_requests/3, increment_pkt_id/1, reset_packet_id/2, parse_ipbus_packet/1, state_as_string/1]).

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
                ip_tuple                     :: tuple(),
                ip_u32                       :: non_neg_integer(),
                port                         :: non_neg_integer(),
                udp_pid                      :: pid(),
                ipbus_v                      :: ipbus_version(), 
                next_id                      :: non_neg_integer(),
                in_flight = {0, queue:new()} :: {non_neg_integer(), queue()},
                queue = queue:new()          :: queue(),
                max_in_flight                :: non_neg_integer(),
                stats 
                }
        ).

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
    ?CH_LOG_INFO("Setting up device client (in ch_device_client:init/1). Absolute max nr packets in flight is ~w.", 
                 [ChMaxInFlight]),

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
            {ok, #state{socket=Socket, ip_tuple=IPTuple, ip_u32=IPaddrU32, port=PortU16, udp_pid=UdpPid, stats=StatsTable} };
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
    ?CH_LOG_ERROR("Unexpected call received : ~p", [_Request], State),
    Reply = ok,
    {reply, Reply, State, ?DEVICE_CLIENT_SHUTDOWN_AFTER}.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

% handle_cast callback for enqueue_requests API call.
handle_cast({send, RequestPacket, ClientPid}, S = #state{queue=Queue}) ->
    ?CH_LOG_DEBUG("IPbus request packet received from Transaction Manager with PID = ~w.", [ClientPid]),
    ?PACKET_TRACE(RequestPacket, "The following IPbus request have been received from Transaction "
                  "Manager with PID = ~w.", [ClientPid]),
    if
      S#state.mode =:= setup ->
        {ReqIPbusVer, _Type, _End} = parse_ipbus_packet(RequestPacket),
        case get_device_status(ReqIPbusVer) of
            {ok, {1,3}, {}} ->
                ?CH_LOG_INFO("Target speaks IPbus v1.3"),
                NewS = S#state{mode = normal,
                               ipbus_v = {1,3},
                               max_in_flight = 1,
                               next_id = void
                              },
                device_client_loop(recv, send_request(RequestPacket, ClientPid, NewS) );
%                send_requests_to_board({RequestPacket, ClientPid}, S#state{ mode=normal, ipbus_v={1,3}, max_in_flight=1 });
            {ok, {2,0}, {_MTU, TargetNrBuffers, NextExpdId}} ->
                ?CH_LOG_INFO("Target speaks IPbus v2.0 (MTU=~w bytes, NrBuffers=~w, NextExpdId=~w)", [_MTU, TargetNrBuffers, NextExpdId]),
                ?CH_LOG_INFO("Device client is entering new logic."),
                NewS = S#state{mode = normal,
                               ipbus_v = {2,0},
                               max_in_flight = min(TargetNrBuffers, get(abs_max_in_flight)),
                               next_id = NextExpdId
                               },
                device_client_loop(send, send_request(RequestPacket, ClientPid, NewS) );
%                send_requests_to_board({RequestPacket, ClientPid}, S#state{mode = normal,
%                                                                           ipbus_v = {2,0},
%                                                                           max_in_flight = min(TargetNrBuffers, get(abs_max_in_flight)),
%                                                                           next_id = NextExpdId
%                                                                           });
            {error, _, MsgForTransManager} ->
                ?CH_LOG_ERROR("Target didn't respond correctly to status request in ch_device_client:init/1."),
                ClientPid ! MsgForTransManager,
                {stop, "Target didn't respond correctly to status request in device client setup.", S}
        end;
      
      length(S#state.in_flight) < S#state.max_in_flight ->
        send_requests_to_board({RequestPacket, ClientPid}, S);
      true ->
        {_HdrSent, TimeSent, _, RetryCount, _Pid} = lists:nth(1, S#state.in_flight),
        NewTimeout = updated_timeout( (?UDP_RESPONSE_TIMEOUT * (RetryCount+1)), TimeSent),
        ?CH_LOG_DEBUG("Request packet from ~w is being queued (~w packets already in-flight (max nr), new queue length ~w, new timeout ~wms).", [ClientPid, length(S#state.in_flight), queue:len(Queue)+1, NewTimeout]),
        {noreply, S#state{queue=queue:in({RequestPacket, ClientPid}, Queue)}, NewTimeout}
    end;

%% Default handle cast
handle_cast(_Msg, State) -> 
    ?CH_LOG_ERROR("Unexpected cast received : ~p", [_Msg], State),
    {noreply, State, ?DEVICE_CLIENT_SHUTDOWN_AFTER}.


% Send/receive clause
%   Takes messages from front
device_client_loop(send, S = #state{ socket=Socket, ip_tuple=IP, port=Port, in_flight={NrInFlight, _} }) ->
    Timeout = if
                NrInFlight =/= 0 -> ?UDP_RESPONSE_TIMEOUT;
                true -> ?DEVICE_CLIENT_SHUTDOWN_AFTER
              end,
    receive
        {'$gen_cast', {send, Pkt, Pid}} ->
            NewS = send_request(Pkt, Pid, S),
            device_client_loop(recv, NewS);
        {udp, Socket, IP, Port, Pkt} ->
            NewS = forward_reply(Pkt, S),
            device_client_loop(send, NewS);
        Other ->
            ?CH_LOG_WARN("Unexpected message received : ~p", [Other]),
            device_client_loop(send, S)
    after Timeout ->
        if
          NrInFlight =/= 0 ->
            device_client_loop(timeout, S);
          true ->
            ?CH_LOG_INFO("No communication has passed through this device client in ~pms ; shutting down now", [?DEVICE_CLIENT_SHUTDOWN_AFTER], S),
            {stop, normal, S}
        end
    end;

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
    after ?UDP_RESPONSE_TIMEOUT ->
        device_client_loop(timeout, S)
    end;

% Response timeout clause
device_client_loop(timeout, S) ->
    ?CH_LOG_ERROR("TIMEOUT when waiting for response from board. Packet-loss recovery not yet added back in."),
    {stop, udp_timeout, S}.


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
            ?CH_LOG_WARN("Request packet from PID ~w is invalid for an IPbus 2.0 target, and is being ignored. parse_ipbus_control_pkt returned ~w", [Pid, Other]), 
            Pid ! {device_client_response, S#state.ip_u32, S#state.port, 42, <<>>},
            S
    end;
send_request(ReqPkt, Pid, S) when S#state.ipbus_v == {1,3} ->
    case parse_ipbus_control_pkt(ReqPkt) of
        {{1,3}, _} ->
            ?CH_LOG_DEBUG("IPbus 1.3 request packet from PID ~w is being forwarded to board at ~s.", [Pid, ch_utils:ip_port_string(S#state.ip_tuple, S#state.port)]),
            S#state.udp_pid ! {send, ReqPkt},
            ch_stats:udp_sent(S#state.stats),
            S#state{in_flight={1,Pid}};
        Other ->
            ?CH_LOG_WARN("Request packet from PID ~w is invalid for an IPbus 1.3 target, and is being ignored. parse_ipbus_control_pkt returned ~w", [Pid, Other]),
            Pid ! {device_client_response, S#state.ip_u32, S#state.port, 42, <<>>},
            S
    end.


forward_reply(Pkt, S = #state{ in_flight={NrInFlight,InFlightQ} }) when S#state.ipbus_v == {2,0} ->
    {{value, SentPktInfo = {Pid,OrigHdr,SentHdr,_}}, NewQ} = queue:out(InFlightQ),
    case Pkt of 
        <<SentHdr:4/binary, ReplyBody/binary>> ->
            ?CH_LOG_DEBUG("IPbus 2.0 reply from ~s is being forwarded to PID ~w", [ch_utils:ip_port_string(S#state.ip_tuple,S#state.port), Pid]),
            Pid ! {device_client_response, S#state.ip_u32, S#state.port, ?ERRCODE_SUCCESS, [OrigHdr, ReplyBody]},
            ch_stats:udp_rcvd(S#state.stats),
            S#state{ in_flight={NrInFlight-1,NewQ} };
        _ ->
            ?CH_LOG_WARN("Ignoring received packet with incorrect header (expecting header ~w, could just be genuine out-of-order reply): ~w", [SentHdr, Pkt]),
            S#state{ in_flight={NrInFlight,queue:in_r(SentPktInfo, InFlightQ)} }
    end;
forward_reply(Pkt, S) when S#state.ipbus_v == {1,3} ->
    {1, TransManagerPid} = S#state.in_flight,
    ?CH_LOG_DEBUG("IPbus reply from ~s is being forwarded to PID ~w", [ch_utils:ip_port_string(S#state.ip_tuple,S#state.port), TransManagerPid]),
    TransManagerPid ! {device_client_response, S#state.ip_u32, S#state.port, ?ERRCODE_SUCCESS, Pkt},
    ch_stats:udp_rcvd(S#state.stats),
    S.




%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

% handle_info callback for udp packets from device
handle_info({udp, Socket, TargetIPTuple, TargetPort, ReplyBin}, S = #state{socket=Socket, ip_tuple=TargetIPTuple, port=TargetPort}) when length(S#state.in_flight)=/=0 ->
    ?CH_LOG_DEBUG("Received response from target hardware; passing it to originating Transaction Manager..."),
    ch_stats:udp_rcvd(S#state.stats),
    ReplyHdr = binary_part(ReplyBin, 0, 4),
    case {S#state.ipbus_v, lists:keymember(ReplyHdr, 1, S#state.in_flight)} of
        {{2, 0}, true} ->
            forward_replies_to_transaction_manager(ReplyBin, S#state{mode=normal});
        {{2, 0}, _} ->
            ?CH_LOG_WARN("Received UDP response packet with wrong header - either a slightly delayed reply packet that I requested re-send of, or an error!~n"
                         "Received packet is: ~w", [ReplyBin], S),
            [{_HdrSent, TimeSent, _, RetryCount, _ClientPid} | _] = S#state.in_flight,
            {noreply, S, updated_timeout((?UDP_RESPONSE_TIMEOUT * (RetryCount+1)), TimeSent) };
        _ ->
            forward_replies_to_transaction_manager(ReplyBin, S#state{mode=normal})
    end;

% handle_info callback for device response timeout
handle_info(timeout, S = #state{ip_tuple=_TargetIPTuple, port=TargetPort, next_id=NextId}) when length(S#state.in_flight)=/=0 ->
    {HdrSent, TimeSent, IoDataSent, RetryCount, ClientPid} = lists:nth(1, S#state.in_flight),
    ?CH_LOG_DEBUG("TIMEOUT! No response from target hardware at IP addr=~w, port=~w. "
                  "Checking on status of hardware...", [_TargetIPTuple, TargetPort]),
    case RetryCount of
         0 -> ch_stats:udp_timeout(S#state.stats, normal);
         _ -> ch_stats:udp_timeout(S#state.stats, resend)
    end,
    if
      % Packet-loss recovery for IPbus 2.0
      (S#state.ipbus_v=:={2,0}) and (RetryCount<3) ->
        ?CH_LOG_WARN("Timeout when waiting for response from target; starting packet loss recovery (attempt ~w) ... ",[RetryCount+1], S),
        NextIdMinusN = decrement_pkt_id(NextId, length(S#state.in_flight)),
        case get_device_status(S#state.ipbus_v) of 
            % Request packet lost => re-send
            {ok, {2,0}, {_, _, HwNextId}} when HwNextId =:= NextIdMinusN ->
                % Add request packets that would have been dropped, back to front of state's queue
                ?CH_LOG_INFO("Request packet got lost on way to board."),
                InFlightTail = lists:nthtail(1, S#state.in_flight),
                DroppedRequests = [{<<ModHdr/binary, ModBody/binary>>, Pid} || {_, _, [ModHdr, ModBody], _, Pid} <- lists:reverse(InFlightTail)],
                NewQ = queue:join( queue:from_list(DroppedRequests), S#state.queue ),
                NewInFlight = [{HdrSent, TimeSent, IoDataSent, RetryCount+1, ClientPid}],
                % Re-send original packet
                S#state.udp_pid ! {send, IoDataSent},
%                gen_udp:send(Socket, TargetIPTuple, TargetPort, PktSent),
                ch_stats:udp_sent(S#state.stats),
                if RetryCount=:=0 -> ch_stats:udp_lost(S#state.stats, request); true -> void end,
                {noreply, S#state{in_flight=NewInFlight, mode=recover_lost_pkt, next_id=increment_pkt_id(HwNextId), queue=NewQ}, ?UDP_RESPONSE_TIMEOUT};
            % Response packet lost => Ask board to re-send
            {ok, {2,0}, {_, _, HwNextId}} ->
                LostResponseNextIds = [increment_pkt_id(NextIdMinusN, X) || X <- lists:seq(1,length(S#state.in_flight))],
                case lists:member(HwNextId, LostResponseNextIds) of
                    true ->
                        ?CH_LOG_INFO("Reply packet got lost on way back from board."),
                        NewInFlight = lists:keyreplace(HdrSent, 1, S#state.in_flight, {HdrSent, TimeSent, IoDataSent, RetryCount+1, ClientPid}),
                        {{2,0}, {control,Id}, _End} = parse_ipbus_packet(HdrSent),
                        S#state.udp_pid ! {send, resend_request_pkt(Id)},
%                        gen_udp:send(Socket, TargetIPTuple, TargetPort, resend_request_pkt(Id)),
                        ch_stats:udp_sent(S#state.stats),
                        if RetryCount=:=0 -> ch_stats:udp_lost(S#state.stats, response); true -> void end,
                        {noreply, S#state{in_flight=NewInFlight, mode=recover_lost_pkt}, ?UDP_RESPONSE_TIMEOUT};
                    false ->
                        Msg = io_lib:format("Target board's next expected ID has unexpected value of ~w. Another client (ControlHub or uHAL) is probably communicating with this target at the same time. "
                                      "I've experienced timeout, with ~w request packets already in-flight, and I think board's next ID should now lie in the range ~w to ~w (incl.) in case of packet loss.",
                                      [ HwNextId, length(S#state.in_flight), NextIdMinusN, lists:last(LostResponseNextIds) ]),                     
                        ?CH_LOG_ERROR(Msg, [], S),
                        {stop, Msg}
                end;
            % Error in getting device status
            {error, _Type, MsgForTransManager} ->
                ClientPid ! MsgForTransManager,
                {noreply, S#state{in_flight=[], mode=timeout}}
        end;
      % Workaround for softer timeout with non-IPbus 2.0 packets
      RetryCount<3 ->
         ?CH_LOG_WARN("Timeout nr. ~w when waiting for response from target; not IPbus 2.0 and so will just wait for another ~wms ... ", 
                      [RetryCount+1, ?UDP_RESPONSE_TIMEOUT], S),
         NewInFlight = [{HdrSent, TimeSent, IoDataSent, RetryCount+1, ClientPid}],
         {noreply, S#state{in_flight=NewInFlight}, ?UDP_RESPONSE_TIMEOUT};
      % Clause for ultimate irrecoverable timeout
      true ->
        ?CH_LOG_ERROR("TIMEOUT! No response from target. Generating and sending a timeout response to originating Transaction Manager...", [], S),
        ClientPid ! { device_client_response, get(target_ip_u32), TargetPort, ?ERRCODE_TARGET_CONTROL_TIMEOUT, <<>> },
        case queue:is_empty(S#state.queue) of
            true ->
                {noreply, S#state{in_flight=[], mode=timeout}, ?DEVICE_CLIENT_SHUTDOWN_AFTER};
            false ->
                send_requests_to_board(S#state{in_flight=[], mode=timeout})
        end
    end;

% handle_info for when no communication through through this device client in given time
handle_info(timeout, State) -> 
    ?CH_LOG_INFO("No communication has passed through this device client in ~pms ; shutting down now", [?DEVICE_CLIENT_SHUTDOWN_AFTER], State),
    {stop, normal, State};

% Default handle_info callback
handle_info(_Info, State) ->
    ?CH_LOG_ERROR("Unexpected handle_info message received : ~p", [_Info], State),
    {noreply, State, ?DEVICE_CLIENT_SHUTDOWN_AFTER}.


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


%% ------------------------------------------------------------------------------
%% @spec updated_timeout( OrigTimeout :: pos_integer(), TimeSent ) -> NewTimeout
%% @end
%% ------------------------------------------------------------------------------

updated_timeout(OrigTimeout, TimeSent) when is_integer(OrigTimeout), OrigTimeout>0 ->
    TimeSinceSent = ( timer:now_diff(os:timestamp(),TimeSent) div 1000 ),
    max(0, (OrigTimeout - TimeSinceSent)).


%% ------------------------------------------------------------------------------
%% @doc Send control request packet from front of queue to the board, returning
%%      the new return value for the handle_{call,cast,info} function that it's 
%%      called from
%% @spec send_requests_to_board( S :: state() ) -> {noreply, NewState}
%%                                               | {noreply, NewState, Timeout}
%% @end
%% ------------------------------------------------------------------------------

send_requests_to_board( S = #state{queue = Q, in_flight = InFlightList} ) ->
    case queue:is_empty(Q) of
        false when length(InFlightList) < S#state.max_in_flight ->
            send_requests_to_board(queue:get(Q), S#state{queue=queue:drop(Q)} );
        true when length(InFlightList) =:= 0 ->
            {noreply, S, ?DEVICE_CLIENT_SHUTDOWN_AFTER};
        _ ->
            {_, TimeSent, _, RetryCount, _} = lists:nth(1, InFlightList),
            {noreply, S, updated_timeout((?UDP_RESPONSE_TIMEOUT * (RetryCount+1)), TimeSent)}
    end.


%% ------------------------------------------------------------------------------
%% @doc Sends a control request packet to the board, returning the 
%%      new return value for the handle_{call,cast,info} function that 
%%      it's called from
%% @spec send_requests_to_board({Packet::binary(), ClientPid::pid()}, S::state())
%%                         -> {noreply, NewState}
%%                          | {noreply, NewState, Timeout}
%% @end
%% ------------------------------------------------------------------------------

send_requests_to_board({Packet, ClientPid}, S = #state{ipbus_v=IPbusVer}) when is_binary(Packet), is_pid(ClientPid) ->
    ?CH_LOG_DEBUG("Request packet from PID ~w is being forwarded to the board.", [ClientPid]),
    case reset_packet_id(Packet, S#state.next_id) of
        {error, _Type, MsgForClient} ->
            ?CH_LOG_DEBUG("ERROR encountered in resetting packet ID - returning following message to Transaction Manager (PID ~w): ~w", [ClientPid, MsgForClient]),
            ClientPid ! MsgForClient,
            {noreply, S#state{ipbus_v=unknown, next_id=unknown} };
        {IPbusVer, ModPkt, PktId} ->
            S#state.udp_pid ! {send, ModPkt},           
%            gen_udp:send(Socket, TargetIPTuple, TargetPort, ModPkt),
            NewInFlightList = lists:append( S#state.in_flight, [{hd(ModPkt), os:timestamp(), ModPkt, 0, ClientPid}] ),
            ch_stats:udp_sent(S#state.stats),
            NewS = if
                     is_integer(PktId) ->
                       S#state{in_flight=NewInFlightList, next_id = increment_pkt_id(PktId)};
                     true ->
                       S#state{in_flight=NewInFlightList}
                   end,
           ?CH_LOG_DEBUG("Request packet sent. New state  ...", [], NewS),
           send_requests_to_board(NewS)
    end.
    

%% ---------------------------------------------------------------------
%% @doc
%%
%%
%% @spec
%%
%% @end
%% ---------------------------------------------------------------------

forward_replies_to_transaction_manager(ReplyBin, S) when is_binary(ReplyBin), is_record(S,state) ->
    [H | T] = S#state.in_flight,
    ReplyHdr = binary_part(ReplyBin, 0, 4),
    case H of
        {ClientPid, Bin} ->
            ClientPid ! { device_client_response, get(target_ip_u32), get(target_port), ?ERRCODE_SUCCESS, Bin},
            forward_replies_to_transaction_manager(ReplyBin, S#state{in_flight=T});
        _ when S#state.ipbus_v =:= {1,3} ->
            element(5,H) ! { device_client_response, get(target_ip_u32), get(target_port), ?ERRCODE_SUCCESS, ReplyBin},
            send_requests_to_board(S#state{in_flight=[]});
        {ReplyHdr, _TimeSent, _, NrRetries, ClientPid} ->
            ClientPid ! { device_client_response, get(target_ip_u32), get(target_port), ?ERRCODE_SUCCESS, ReplyBin},
            if
              NrRetries>0 ->
                ?CH_LOG_INFO( "Recovered lost packet!", [], S),
                ch_stats:udp_timeout(S#state.stats, recovered);
              true -> void
            end,
            send_requests_to_board( S#state{in_flight=forward_pending_replies(T)} );
        _ ->
            {ReplyHdr, _TimeSent, _, _NrRetries, ClientPid} = lists:keyfind(ReplyHdr, 1, S#state.in_flight),
            send_requests_to_board(S#state{ in_flight = lists:keyreplace(ReplyHdr, 1, S#state.in_flight, {ClientPid,ReplyBin} ) })
    end.


%% ---------------------------------------------------------------------
%% @doc
%%
%% @spec
%% 
%% @end
%% ---------------------------------------------------------------------

forward_pending_replies( [{ClientPid, ReplyBin} | T] ) when is_pid(ClientPid), is_binary(ReplyBin) ->
    ClientPid ! { device_client_response, get(target_ip_u32), get(target_port), ?ERRCODE_SUCCESS, ReplyBin},
    forward_pending_replies(T);
forward_pending_replies( L ) when is_list(L) ->
    L.


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
    <<16#f0:8, Id:16/little, 16#f0>>.


%% ---------------------------------------------------------------------
%% @doc ... TODO ...
%% @spec reset_packet_id(RawRequestBin, Id) -> {ok, IPbusVer, ModIPbusRequest, PacketId}
%%                                           | {error, malformed, MsgForTransManager}
%%                                           | {error, timeout, MsgForTransManager}
%% @end
%% ---------------------------------------------------------------------

reset_packet_id(RawRequest, NewId) ->
    {Ver, _, End} = parse_ipbus_packet(RawRequest),
    case {Ver, NewId} of
        {{2,0}, _} when is_integer(NewId) ->
             case End of
                 big    -> {Ver, reset_header(RawRequest, <<16#20:8, NewId:16/big, 16#f0:8>>), NewId};
                 little -> {Ver, reset_header(RawRequest, <<16#f0:8, NewId:16/little, 16#20:8>>), NewId}
             end;
        {{2,0}, _} ->
            case get_device_status({2,0}) of 
                {error, _Type, _MsgForTransManager} = X ->
                   X;
                {ok, _, IdFromStatus} ->
                   reset_packet_id(RawRequest, IdFromStatus)
             end;
        _ ->
             <<Hdr:4/binary, Body/binary>> = RawRequest,
             {Ver, [Hdr,Body], notset}
    end.


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
%% @doc Increments packet ID N times, loooping round fom 0xffff to 1
%% @spec increment_pkt_id( Id::pos_integer(), N::pos_integer() ) -> IdPlusN::pos_integer()
%% @end
%% ---------------------------------------------------------------------

increment_pkt_id(Id, 0) when is_integer(Id), Id>0 ->
    Id;
increment_pkt_id(Id, N) when is_integer(Id), Id>0, is_integer(N), N>0 ->
    increment_pkt_id( increment_pkt_id(Id), N-1 ).


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
resend_request_pkt(Id) when Id =< 16#ffff, Id >= 0 ->
    Value = (2 bsl 28) + (Id bsl 8) + 16#f2,
    <<Value:32/big>>.


%% ------------------------------------------------------------------------------------
%% @doc Retrieves number of response buffers and next expected packet ID for the
%%       device, assuming it's an IPbus 2.0 device. Returns tuple beginning with
%%       atom error in case there was a timeout or response was malformed.
%% @spec get_device_status( IPbusVer :: ipbus_version() )
%%                             -> {ok, {1,3}, {}}
%%                              | {ok, {2,0}, {NrResponseBuffers, NextExpdId}}
%%                              | {error, malformed, MsgForTransManager}
%%                              | {error, timeout, MsgForTransManager}
%% @end
%% ------------------------------------------------------------------------------------

get_device_status(IPbusVer) ->
    get_device_status(IPbusVer, 4).


%% ------------------------------------------------------------------------------------
%% @doc Determines protocol version of device and other "status" information (such as
%%       number of response buffers and next expected packet ID. Returns tuple beginning
%%       with atom error in case there was a timeout or response was malformed.
%% @spc get_device_status( IPbusVer :: ipbus_version(), NrAttemptsLeft :: non_neg_integer() )
%%                                         -> {ok, {1,3}, {}}
%%                                          | {ok, {2,0}, {MTU, NrResponseBuffers, NextExpdId}}
%%                                          | {error, malformed, MsgForTransManager}
%%                                          | {error, timeout, MsgForTransManager}
%%
%% @end
%% ------------------------------------------------------------------------------------

get_device_status(IPbusVer, {NrAttemptsLeft, TotNrAttempts}) when NrAttemptsLeft =:= 0 ->
    ?CH_LOG_ERROR("MAX NUMBER OF TIMEOUTS reached in get_device_status function for IPbus version ~w."
                  " No response from targets after ~w attempts, each with timeout of ~wms.",
                  [IPbusVer, TotNrAttempts, ?UDP_RESPONSE_TIMEOUT]),
    {error, timeout, {device_client_response, get(target_ip_u32), get(target_port), ?ERRCODE_TARGET_STATUS_TIMEOUT, <<>> } };

get_device_status(IPbusVer, NrAttemptsLeft) when is_integer(NrAttemptsLeft), NrAttemptsLeft > 0 ->
    get_device_status(IPbusVer, {NrAttemptsLeft, NrAttemptsLeft});

get_device_status(IPbusVer, {NrAttemptsLeft, TotNrAttempts}) when is_integer(NrAttemptsLeft), NrAttemptsLeft > 0 ->
    AttemptNr = TotNrAttempts - NrAttemptsLeft + 1,
    Socket = get(socket),
    UdpPid = get(udp_pid),
    {TargetIPTuple, TargetPort} = target_ip_port(),
    StatusReq13 = binary:copy(<<16#100000f8:32/native>>, 10),
    StatusReq20 = <<16#200000f1:32/big, 0:(15*32)>>,
    ?CH_LOG_INFO("Sending IPbus status request to target (attempt ~w of ~w).",
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
        {udp, Socket, TargetIPTuple, TargetPort, <<16#100000fc:32/native, _/binary>>} ->
            ?CH_LOG_INFO("Received an IPbus 1.3 'status response' from target, on attempt ~w of ~w.",
                         [TargetIPTuple, TargetPort, AttemptNr, TotNrAttempts]),
            ch_stats:udp_rcvd(get(stats)),
            {ok, {1,3}, {}};
        {udp, Socket, TargetIPTuple, TargetPort, <<16#200000f1:32/big, _/binary>> = ReplyBin} ->
            ch_stats:udp_rcvd(get(stats)),
            case parse_ipbus_packet(ReplyBin) of 
                {{2,0}, {status, MTU, NBuffers, NextId}, big} ->
                    ?CH_LOG_INFO("Received an IPbus 2.0 'status response' from target, on attempt ~w of ~w. MTU=~w, NBuffers=~w, NextExpdId=~w",
                                 [AttemptNr, TotNrAttempts, MTU, NBuffers, NextId]),
                    {ok, {2,0}, {MTU, NBuffers, NextId}};
                _Details ->
                    ?CH_LOG_ERROR("Received a malformed IPbus 2.0 'status response' (correct IPbus packet header, but body wrong format). parse_ipbus_packet returned ~w",
                                 [_Details]),
                    {error, malformed, {device_client_response, TargetIPTuple, TargetPort, ?ERRCODE_MALFORMED_STATUS, <<>>} }
            end
    after ?UDP_RESPONSE_TIMEOUT ->
        ?CH_LOG_WARN("TIMEOUT waiting for response in get_device_status! No response from target on attempt ~w of ~w, ipbus version ~w.",
                     [AttemptNr, TotNrAttempts, IPbusVer]),
        ch_stats:udp_timeout(get(stats), status),
        get_device_status(IPbusVer, {NrAttemptsLeft-1, TotNrAttempts})
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
                   length(S#state.in_flight), S#state.max_in_flight,
                   queue:len(S#state.queue)]
                  ).


udp_proc_init() ->
    receive
        {start, Socket, IP, Port, ParentPid} ->
            link(ParentPid),
            udp_proc_loop(Socket, IP, Port, ParentPid)
    end.


udp_proc_loop(Socket, IP, Port, ParentPid) ->
    receive
        {udp, Socket, IP, Port, Packet} ->
            ParentPid ! {udp, Socket, IP, Port, Packet}
    after 0 ->
        receive 
            {send, Pkt} ->
                {IP1, IP2, IP3, IP4} = IP,
                true = erlang:port_command(Socket, [[((Port) bsr 8) band 16#ff, (Port) band 16#ff], [IP1 band 16#ff, IP2 band 16#ff, IP3 band 16#ff, IP4 band 16#ff], Pkt]);
            {inet_reply, Socket, ok} ->
                void;
            {inet_reply, Socket, SendError} ->
                ?CH_LOG_ERROR("Error in TCP async send: ~w", [SendError]),
                throw({udp_send_error, SendError});
            Other ->
                ParentPid ! Other
        end 
    end,
    udp_proc_loop(Socket, IP, Port, ParentPid).

