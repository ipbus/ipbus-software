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
-export([start_link/3, enqueue_requests/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
         reset_packet_id/2, parse_ipbus_packet/1, state_as_string/1]).

-type mode() :: 'setup' | 'normal' | 'recover_lost_pkt' | 'timeout'.

-type ipbus_version() :: {1, 3} | {2, 0} | 'unknown'.

-type timestamp() :: { non_neg_integer() , non_neg_integer() , non_neg_integer() }.

-type in_flight_info() :: {<<_:32>>, timestamp(), binary(), non_neg_integer(), pid(), <<_:32>>} | {pid(), binary()}. %{ModHdr, now(), ModRequest, 0, ClientPid, OrigHdr},

%-type endness() :: big | little.

-record(state, {mode = normal       :: mode(), 
                socket,              % Holds network socket to target device 
                target_ip_tuple     :: tuple(),
                target_port         :: non_neg_integer(),
                ipbus_v = unknown   :: ipbus_version(), 
                next_id             :: non_neg_integer(),
                max_in_flight       :: non_neg_integer(),
                in_flight = []      :: [in_flight_info()],   % Details of packet in-flight to board
                queue = queue:new() % Queue of packets waiting to be sent to board
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
    case gen_udp:open(0, [binary]) of   
        {ok, Socket} ->
            put(socket, Socket),
            put(abs_max_in_flight, ChMaxInFlight),
            {ok, #state{mode = setup, socket = Socket, target_ip_tuple=IPTuple, target_port=PortU16}};
%            case get_device_status(unknown) of
%                {ok, {1,3}, {}} ->
%                    ?CH_LOG_INFO("Target speaks IPbus v1.3"),
%                    {ok, BasicState#state{ ipbus_v={1,3} }};
%                {ok, {2,0}, {_MTU, TargetNrBuffers, NextExpdId}} ->
%                    ?CH_LOG_INFO("Target speaks IPbus v2.0 (MTU=~w bytes, NrBuffers=~w, NextExpdId=~w)", [_MTU, TargetNrBuffers, NextExpdId]),
%                    {ok, BasicState#state{ ipbus_v={2,0},
%                                           max_in_flight = min(TargetNrBuffers, ChMaxInFlight),
%                                           next_id = NextExpdId
%                                          }};
%                {error, _, MsgForTransManager} ->
%                    ?CH_LOG_WARN("Target didn't respond correctly to status request in ch_device_client:init/1."),
%                    {ok, BasicState#state{ mode = unresponsive_target }}
%            end;
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
                send_requests_to_board({RequestPacket, ClientPid}, S#state{ mode=normal, ipbus_v={1,3}, max_in_flight=1 });
            {ok, {2,0}, {_MTU, TargetNrBuffers, NextExpdId}} ->
                ?CH_LOG_INFO("Target speaks IPbus v2.0 (MTU=~w bytes, NrBuffers=~w, NextExpdId=~w)", [_MTU, TargetNrBuffers, NextExpdId]),
                send_requests_to_board({RequestPacket, ClientPid}, S#state{mode = normal,
                                                                           ipbus_v = {2,0},
                                                                           max_in_flight = min(TargetNrBuffers, get(abs_max_in_flight)),
                                                                           next_id = NextExpdId
                                                                           });
            {error, _, MsgForTransManager} ->
                ?CH_LOG_ERROR("Target didn't respond correctly to status request in ch_device_client:init/1."),
                ClientPid ! MsgForTransManager,
                {stop, "Target didn't respond correctly to status request in device client setup.", S}
        end;
      
      length(S#state.in_flight) < S#state.max_in_flight ->
        send_requests_to_board({RequestPacket, ClientPid}, S);
      true ->
        {_HdrSent, TimeSent, _, RetryCount, _Pid, _OrigHdr} = lists:nth(1, S#state.in_flight),
        NewTimeout = updated_timeout( (?UDP_RESPONSE_TIMEOUT * (RetryCount+1)), TimeSent),
        ?CH_LOG_DEBUG("Request packet from ~w is being queued (max nr packets already in flight, new queue length ~w, new timeout ~wms).", [ClientPid, queue:len(S#state.queue), NewTimeout]),
        {noreply, S#state{queue=queue:in({RequestPacket, ClientPid}, Queue)}, NewTimeout}
    end;

%% Default handle cast
handle_cast(_Msg, State) -> 
    ?CH_LOG_ERROR("Unexpected cast received : ~p", [_Msg], State),
    {noreply, State, ?DEVICE_CLIENT_SHUTDOWN_AFTER}.


%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

% handle_info callback for udp packets from device
handle_info({udp, Socket, TargetIPTuple, TargetPort, ReplyBin}, S = #state{socket=Socket, target_ip_tuple=TargetIPTuple, target_port=TargetPort}) when length(S#state.in_flight)=/=0 ->
    ?CH_LOG_DEBUG("Received response from target hardware; passing it to originating Transaction Manager..."),
    ch_stats:udp_in(),
    << ReplyHdr:4/binary, _/binary>> = ReplyBin,
    case {S#state.ipbus_v, lists:keymember(ReplyHdr, 1, S#state.in_flight)} of
        {{2, 0}, true} ->
            forward_replies_to_transaction_manager(ReplyBin, S#state{mode=normal});
        {{2, 0}, _} ->
            ?CH_LOG_WARN("Received UDP response packet with wrong header - either a slightly delayed reply packet that I requested re-send of, or an error!~n"
                         "Received packet is: ~w", [ReplyBin], S),
            [{_HdrSent, TimeSent, _, RetryCount, _ClientPid, _OrigHdr} | _] = S#state.in_flight,
            {noreply, S, updated_timeout((?UDP_RESPONSE_TIMEOUT * (RetryCount+1)), TimeSent) };
        _ ->
            forward_replies_to_transaction_manager(ReplyBin, S#state{mode=normal})
    end;

% handle_info callback for device response timeout
handle_info(timeout, S = #state{socket=Socket, target_ip_tuple=TargetIPTuple, target_port=TargetPort, next_id=NextId}) when length(S#state.in_flight)=/=0 ->
    {HdrSent, TimeSent, PktSent, RetryCount, ClientPid, _OrigHdr} = lists:nth(1, S#state.in_flight),
    ?CH_LOG_DEBUG("TIMEOUT! No response from target hardware at IP addr=~w, port=~w. "
                  "Checking on status of hardware...", [TargetIPTuple, TargetPort]),
    case RetryCount of
         0 -> ch_stats:udp_response_timeout(normal);
         _ -> ch_stats:udp_response_timeout(resend)
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
                InFlightTail = lists:nthtail(1, S#state.in_flight),
                DroppedRequests = [{<<OrigHdr:4/binary, ReqBody/binary>>, Pid} || {_, _, <<_:4/binary, ReqBody/binary>>, _, Pid, OrigHdr} <- lists:reverse(InFlightTail)],
                NewQ = queue:join( queue:from_list(DroppedRequests), S#state.queue ),
                NewInFlight = [{HdrSent, TimeSent, PktSent, RetryCount+1, ClientPid, _OrigHdr}],
                % Re-send original packet
                gen_udp:send(Socket, TargetIPTuple, TargetPort, PktSent),
                ch_stats:udp_out(),
                {noreply, S#state{in_flight=NewInFlight, mode=recover_lost_pkt, next_id=increment_pkt_id(HwNextId), queue=NewQ}, ?UDP_RESPONSE_TIMEOUT};
            % Response packet lost => Ask board to re-send
            {ok, {2,0}, {_, _, HwNextId}} when HwNextId =:= NextId ->
                NewInFlight = lists:keyreplace(HdrSent, 1, S#state.in_flight, {HdrSent, TimeSent, PktSent, RetryCount+1, ClientPid, _OrigHdr}),
                {{2,0}, {control,Id}, End} = parse_ipbus_packet(HdrSent),
                gen_udp:send(Socket, TargetIPTuple, TargetPort, resend_request_pkt(Id, End)),
                ch_stats:udp_out(),
                {noreply, S#state{in_flight=NewInFlight, mode=recover_lost_pkt}, ?UDP_RESPONSE_TIMEOUT};
            % Error in getting device status
            {error, _Type, MsgForTransManager} ->
                ClientPid ! MsgForTransManager,
                {noreply, S#state{in_flight=[], mode=timeout}}
        end;
      % Workaround for softer timeout with non-IPbus 2.0 packets
      RetryCount<3 ->
         ?CH_LOG_WARN("Timeout nr. ~w when waiting for response from target; not IPbus 2.0 and so will just wait for another ~wms ... ", 
                      [RetryCount+1, ?UDP_RESPONSE_TIMEOUT], S),
         NewInFlight = [{HdrSent, TimeSent, PktSent, RetryCount+1, ClientPid, _OrigHdr}],
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
    TimeSinceSent = round(timer:now_diff( now(), TimeSent )/1000.0),
    max(0, (OrigTimeout - TimeSinceSent)).


%% ------------------------------------------------------------------------------
%% @doc Send control request packet from front of queue to the board, returning
%%      the new return value for the handle_{call,cast,info} function that it's 
%%      called from
%% @spec send_requests_to_board( S :: state() ) -> {noreply, NewState}
%%                                               | {noreply, NewState, Timeout}
%% @end
%% ------------------------------------------------------------------------------

send_requests_to_board( State ) ->
    EmptyQueue = queue:is_empty(State#state.queue),
    if
      length(State#state.in_flight) =:= State#state.max_in_flight ->
        {_, TimeSent, _, RetryCount, _, _} = lists:nth(1, State#state.in_flight),
        {noreply, State, updated_timeout((?UDP_RESPONSE_TIMEOUT * (RetryCount+1)), TimeSent)};
      EmptyQueue ->
        {noreply, State, ?DEVICE_CLIENT_SHUTDOWN_AFTER};
      true ->
        {{value, H = {_,_}},NewQ} = queue:out(State#state.queue),
        send_requests_to_board(H, State#state{queue=NewQ})
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

send_requests_to_board({Packet, ClientPid}, S = #state{socket=Socket, target_ip_tuple=TargetIPTuple, target_port=TargetPort}) when is_binary(Packet), is_pid(ClientPid) ->
    ?CH_LOG_DEBUG("Request packet from PID ~w is being forwarded to the board.", [ClientPid]),
    <<OrigHdr:4/binary, _/binary>> = Packet,
    case reset_packet_id(Packet, S#state.next_id) of
        {error, _Type, MsgForClient} ->
            ?CH_LOG_DEBUG("ERROR encountered in resetting packet ID - returning following message to Transaction Manager (PID ~w): ~w", [ClientPid, MsgForClient]),
            ClientPid ! MsgForClient,
            {noreply, S#state{ipbus_v=unknown, next_id=unknown} };
        {IPbusVer, ModRequest, PktId} ->
            gen_udp:send(Socket, TargetIPTuple, TargetPort, ModRequest),
            <<ModHdr:4/binary, _/binary>> = ModRequest,
            NewInFlightList = lists:append( S#state.in_flight, [{ModHdr, now(), ModRequest, 0, ClientPid, OrigHdr}] ),
            ch_stats:udp_out(),
            NewS = if
                     is_integer(PktId) ->
                       S#state{ipbus_v=IPbusVer, in_flight=NewInFlightList, next_id = increment_pkt_id(PktId)};
                     true ->
                       S#state{in_flight=NewInFlightList}
                   end,
           ?CH_LOG_DEBUG("Request packet sent. Now entering state ~w , with timeout of ~wms", [NewS, ?UDP_RESPONSE_TIMEOUT]),
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
    << ReplyHdr:4/binary, ReplyBody/binary>> = ReplyBin,
    [H | T] = S#state.in_flight,
    case H of
        {ClientPid, Bin} ->
            ClientPid ! { device_client_response, get(target_ip_u32), get(target_port), ?ERRCODE_SUCCESS, Bin},
            forward_replies_to_transaction_manager(ReplyBin, S#state{in_flight=T});
        _ when S#state.ipbus_v =:= {1,3} ->
            element(5,H) ! { device_client_response, get(target_ip_u32), get(target_port), ?ERRCODE_SUCCESS, ReplyBin},
            send_requests_to_board(S#state{in_flight=[]});
        {ReplyHdr, _TimeSent, _, NrRetries, ClientPid, OrigHdr} ->
            ClientPid ! { device_client_response, get(target_ip_u32), get(target_port), ?ERRCODE_SUCCESS, <<OrigHdr/binary, ReplyBody/binary>>},
            if
              NrRetries>0 ->
                ?CH_LOG_INFO( "Recovered lost packet!", [], S),
                ch_stats:udp_response_timeout(recovered);
              true -> void
            end,
            send_requests_to_board(S#state{in_flight=T});
        _ ->
            {ReplyHdr, _TimeSent, _, _NrRetries, ClientPid, OrigHdr} = lists:keyfind(ReplyHdr, 1, S#state.in_flight),
            send_requests_to_board(S#state{ in_flight = lists:keyreplace(ReplyHdr, 1, S#state.in_flight, {ClientPid, <<OrigHdr/binary, ReplyBody/binary>>}) })
    end.



%% ---------------------------------------------------------------------
%% @doc ... TODO ...
%% @spec reset_packet_id(RawIPbusRequestBin, Id) -> {ok, IPbusVer, ModIPbusRequest, PacketId}
%%                                                | {error, malformed, MsgForTransManager}
%%                                                | {error, timeout, MsgForTransManager}
%% @end
%% ---------------------------------------------------------------------

reset_packet_id(RawIPbusRequest, NewId) ->
    {Ver, _, End} = parse_ipbus_packet(RawIPbusRequest),
    case {Ver, NewId} of
        {{2,0}, _} when is_integer(NewId) ->
             <<H1:8, _:16, H2:8, PktBody/binary>> = RawIPbusRequest,
             case End of
                 big    -> {Ver, <<H1:8, NewId:16/big, H2:8, PktBody/binary>>, NewId};
                 little -> {Ver, <<H1:8, NewId:16/little, H2:8, PktBody/binary>>, NewId}
             end;
        {{2,0}, _} ->
            case get_device_status({2,0}) of 
                {error, _Type, _MsgForTransManager} = X ->
                   X;
                {ok, _, IdFromStatus} ->
                   reset_packet_id(RawIPbusRequest, IdFromStatus)
             end;
        _ ->
             {Ver, RawIPbusRequest, notset}
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
resend_request_pkt(Id, End) when Id =< 16#ffff, Id >= 0 ->
    Value = (2 bsl 28) + (Id bsl 8) + 16#f2,
    case End of
        big    -> <<Value:32/big>>;
        little -> <<Value:32/little>>
    end.


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
    {TargetIPTuple, TargetPort} = target_ip_port(),
    StatusReq13 = binary:copy(<<16#100000f8:32/native>>, 10),
    StatusReq20 = <<16#200000f1:32/big, 0:(15*32)>>,
    ?CH_LOG_INFO("Sending IPbus status request to target at ~w:~w (attempt ~w of ~w).",
                 [TargetIPTuple, TargetPort, AttemptNr, TotNrAttempts]),
    case IPbusVer of
         {1,3} when NrAttemptsLeft=:=TotNrAttempts ->
             gen_udp:send(Socket, TargetIPTuple, TargetPort, StatusReq13 ),
             ch_stats:udp_out();
         {1,3} ->
             void;
         {2,0} ->
             gen_udp:send(Socket, TargetIPTuple, TargetPort, StatusReq20 ),
             ch_stats:udp_out();
         unknown ->
             gen_udp:send(Socket, TargetIPTuple, TargetPort, StatusReq20 ),
             timer:sleep(2),
             gen_udp:send(Socket, TargetIPTuple, TargetPort, StatusReq13 ),
             ch_stats:udp_out()
    end,
    receive
        {udp, Socket, TargetIPTuple, TargetPort, <<16#100000fc:32/native, _/binary>>} ->
            ?CH_LOG_INFO("Received an IPbus 1.3 'status response' from target at ~w:~w on attempt ~w of ~w.",
                         [TargetIPTuple, TargetPort, AttemptNr, TotNrAttempts]),
            ch_stats:udp_in(),
            {ok, {1,3}, {}};
        {udp, Socket, TargetIPTuple, TargetPort, <<16#200000f1:32/big, _/binary>> = ReplyBin} ->
            ch_stats:udp_in(),
            case parse_ipbus_packet(ReplyBin) of 
                {{2,0}, {status, MTU, NBuffers, NextId}, big} ->
                    ?CH_LOG_INFO("Received a well-formed IPbus 2.0 'status response' from target at ~w:~w on attempt ~w of ~w. MTU=~w, NBuffers=~w, NextExpdId=-~w",
                                 [TargetIPTuple, TargetPort, AttemptNr, TotNrAttempts, MTU, NBuffers, NextId]),
                    {ok, {2,0}, {MTU, NBuffers, NextId}};
                _Details ->
                    ?CH_LOG_ERROR("Received a malformed IPbus 2.0 'status response' (correct IPbus packet header, but body wrong format) from target at ~w:~w. parse_ipbus_packet returned ~w",
                                 [TargetIPTuple, TargetPort, _Details]),
                    {error, malformed, {device_client_response, TargetIPTuple, TargetPort, ?ERRCODE_MALFORMED_STATUS, <<>>} }
            end
    after ?UDP_RESPONSE_TIMEOUT ->
        ?CH_LOG_WARN("TIMEOUT waiting for response in get_device_status! No response from target at ~w:~w on attempt ~w of ~w, ipbus version ~w.",
                     [TargetIPTuple, TargetPort, AttemptNr, TotNrAttempts, IPbusVer]),
        ch_stats:udp_response_timeout(status),
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
                  "         in_flight = ~w~n"
                  "         queue     = ~w~n",
                  [S#state.mode, S#state.ipbus_v, S#state.next_id, 
                   S#state.target_ip_tuple, S#state.target_port,
                   S#state.in_flight, 
                   S#state.queue]
                  ).
