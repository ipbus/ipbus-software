%%% ===========================================================================
%%% @author Robert Frazier
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
-export([start_link/2, enqueue_requests/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
         reset_packet_id/2, parse_packet_header/1]).

-record(state, {mode = setup,       % Running mode of device_client - either setup, normal, recover_lost_pkt, timeout
                socket,             % Holds network socket to target device 
                target_ip_tuple,
                target_ip_u32,
                target_port,
                ipbus_v = unknown,  % IPbus version of target (only set when there is successful communication with target)
                next_id,            % Next packet ID to be sent to target
                in_flight = none,   % Details of packet in-flight to board
                queue = []          % Queue of packets waiting to be sent to board
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
start_link(IPaddrU32, PortU16) when is_integer(IPaddrU32), is_integer(PortU16) ->
    gen_server:start_link(?MODULE, [IPaddrU32, PortU16], []).


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
enqueue_requests(IPaddrU32, PortU16, IPbusRequests) when is_binary(IPbusRequests) ->
    {ok, Pid} = ch_device_client_registry:get_pid(IPaddrU32, PortU16),
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
    put(target_ip_u32, IPaddrU32),
    IPTuple = ch_utils:ipv4_u32_addr_to_tuple(IPaddrU32),
    put(target_ip_tuple, IPTuple),   
    put(target_port, PortU16),

    % Try opening ephemeral port and we want data delivered as a binary.
    case gen_udp:open(0, [binary]) of   
        {ok, Socket} ->
            put(socket, Socket),
            {ok, #state{socket = Socket, target_ip_tuple=IPTuple,
                        target_ip_u32 = IPaddrU32, target_port=PortU16}};
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
    log({error, State}, "Unexpected call received : ~p", [_Request]),
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
    ?DEBUG_TRACE("IPbus request packet received from Transaction Manager with PID = ~w.", [ClientPid]),
    ?PACKET_TRACE(RequestPacket, "The following IPbus request have been received from Transaction "
                  "Manager with PID = ~w.", [ClientPid]),
    if
      S#state.in_flight =:= none ->
        send_request_to_board({RequestPacket, ClientPid}, S);
      true ->
        {_HdrSent, TimeSent, _PktSent, RetryCount, _Pid, _OrigHdr} = S#state.in_flight,
        TimeSinceSent = round(timer:now_diff( now(), TimeSent )/1000.0),
        NewTimeout = max(0, (?UDP_RESPONSE_TIMEOUT * (RetryCount+1)) - TimeSinceSent),
        log(info, "Request packet from ~w is being queued (max nr packets already in flight, new queue length ~w, new timeout ~wms).", [ClientPid, length(S#state.queue), NewTimeout]),
        {noreply, S#state{queue=lists:append(Queue, [{RequestPacket, ClientPid}])}, NewTimeout}
    end;

%% Default handle cast
handle_cast(_Msg, State) -> 
    log({error, State}, "Unexpected cast received : ~p", [_Msg]),
    {noreply, State, ?DEVICE_CLIENT_SHUTDOWN_AFTER}.


%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

% handle_info callback for udp packets from device
handle_info({udp, Socket, TargetIPTuple, TargetPort, ReplyBin}, S = #state{socket=Socket, target_ip_tuple=TargetIPTuple, target_port=TargetPort}) when S#state.in_flight=/=none ->
    ?DEBUG_TRACE("Received response from target hardware at IP addr=~w, port=~w (in_flight=~w) "
                 "Passing it to originating Transaction Manager...", [TargetIPTuple, TargetPort, S#state.in_flight]),
    ch_stats:udp_in(),
    {HdrSent, TimeSent, _PktSnt, RetryCount, ClientPid, OrigHdr} = S#state.in_flight,
    if
      S#state.mode =:= recover_lost_pkt ->
        log({info, S}, "Recovered lost packet!"),
        ch_stats:udp_response_timeout(recovered);
      true -> void
    end,
    MoveOn = case {S#state.ipbus_v, ReplyBin} of
                 {{2, 0}, <<HdrSent:4/binary, ReplyBody/binary>>} ->
                     ClientPid ! { device_client_response, S#state.target_ip_u32, TargetPort, ?ERRCODE_SUCCESS, <<OrigHdr/binary, ReplyBody/binary>>};
                 {{2, 0}, _} ->
                     log({warning,S}, "Received UDP response packet with wrong header - either a slightly delayed reply packet that I requested re-send of, or an error!~n"
                                      "Received packet is: ~w", [ReplyBin]),
                     no;
                 _ ->
                     ClientPid ! { device_client_response, S#state.target_ip_u32, TargetPort, ?ERRCODE_SUCCESS, ReplyBin }
             end,
    if
      MoveOn =:= no ->
        TimeSinceSent = round(timer:now_diff( now(), TimeSent )/1000.0),
        NewTimeout = max(0, (?UDP_RESPONSE_TIMEOUT * (RetryCount+1)) - TimeSinceSent),
        {noreply, S, NewTimeout};
      S#state.queue =:= [] ->
        {noreply, S#state{in_flight=none, mode = norm}, ?DEVICE_CLIENT_SHUTDOWN_AFTER};
      true ->
        [H|T] = S#state.queue,
        send_request_to_board(H, S#state{queue=T, in_flight=none, mode = norm})
    end;

% handle_info callback for device response timeout
handle_info(timeout, S = #state{socket=Socket, target_ip_tuple=TargetIPTuple, target_port=TargetPort, next_id=NextId}) when S#state.in_flight=/=none ->
    {HdrSent, TimeSent, PktSent, RetryCount, ClientPid, _OrigHdr} = S#state.in_flight,
    ?DEBUG_TRACE("TIMEOUT! No response from target hardware at IP addr=~w, port=~w. "
                 "Checking on status of hardware...", [TargetIPTuple, TargetPort]),
    case RetryCount of
         0 -> ch_stats:udp_response_timeout(normal);
         _ -> ch_stats:udp_response_timeout(resend)
    end,
    if
      % Packet-loss recovery for IPbus 2.0
      (S#state.ipbus_v=:={2,0}) and (RetryCount<3) ->
        log({warning, S}, "Timeout when waiting for response from target; starting packet loss recovery (attempt ~w) ... ",[RetryCount+1]),
        NewInFlight = {HdrSent, TimeSent, PktSent, RetryCount+1, ClientPid, _OrigHdr},
        NextIdMinusOne = decrement_pkt_id(NextId),
        case get_device_status() of 
            % Request packet lost => re-send
            {ok, _, HwNextId} when HwNextId =:= NextIdMinusOne -> 
                gen_udp:send(Socket, TargetIPTuple, TargetPort, PktSent),
                ch_stats:udp_out(),
                {noreply, S#state{in_flight=NewInFlight, mode=recover_lost_pkt}, ?UDP_RESPONSE_TIMEOUT};
            % Response packet lost => Ask board to re-send
            {ok, _, HwNextId} when HwNextId =:= NextId ->
                {_, Id, End} = parse_packet_header(HdrSent),
                gen_udp:send(Socket, TargetIPTuple, TargetPort, resend_request_pkt(Id, End)),
                ch_stats:udp_out(),
                {noreply, S#state{in_flight=NewInFlight, mode=recover_lost_pkt}, ?UDP_RESPONSE_TIMEOUT};
            % Error in getting device status
            {error, _Type, MsgForTransManager} ->
                ClientPid ! MsgForTransManager,
                {noreply, S#state{in_flight=none, mode=timeout}}
        end;
      % Workaround for softer timeout with non-IPbus 2.0 packets
      RetryCount<3 ->
         log({warning, S}, "Timeout nr. ~w when waiting for response from target; not IPbus 2.0 and so will just wait for another ~wms ... ", [RetryCount+1, ?UDP_RESPONSE_TIMEOUT]),
         NewInFlight = {HdrSent, TimeSent, PktSent, RetryCount+1, ClientPid, _OrigHdr},
         {noreply, S#state{in_flight=NewInFlight}, ?UDP_RESPONSE_TIMEOUT};
      % Clause for ultimate irrecoverable timeout
      true ->
        log({error, S}, "TIMEOUT! No response from target~n. Generating and sending a timeout response to originating Transaction Manager..."),
        ClientPid ! { device_client_response, S#state.target_ip_u32, TargetPort, ?ERRCODE_TARGET_CONTROL_TIMEOUT, <<>> },
        if 
          S#state.queue =:= [] ->
            {noreply, S#state{in_flight=none, mode=timeout}, ?DEVICE_CLIENT_SHUTDOWN_AFTER};
          true ->
            [H = {ClientPid, _Pkt}|T] = S#state.queue,
            log(info, "Request packet from ~w is now being processed.", [ClientPid]),
            send_request_to_board(H, S#state{queue=T, in_flight=none, mode=timeout})
        end
    end;

% handle_info for when no communication through through this device client in given time
handle_info(timeout, State) -> 
    log({info, State}, "No communication has passed through this device client in ~pms ; shutting down now", [?DEVICE_CLIENT_SHUTDOWN_AFTER]),
    {stop, normal, State};

% Default handle_info callback
handle_info(_Info, State) ->
    log({error,State}, "Unexpected handle_info message received : ~p", [_Info]),
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
%% @doc Sends a control request packet to the board, returning the 
%%      new return value for the handle_{call,cast,info} function that 
%%      it's called from
%% @spec send_request_to_board({Packet::binary(), ClientPid::pid()}, S::state())
%%                        -> {noreply, NewState}
%%                        -> {noreply, NewState, Timeout}
%% ------------------------------------------------------------------------------

send_request_to_board({Packet, ClientPid}, S = #state{socket=Socket, target_ip_tuple=TargetIPTuple, target_port=TargetPort}) ->
    ?DEBUG_TRACE("Request packet from PID ~w is being forwarded to the board at IP ~w, port ~w...", [ClientPid, TargetIPTuple, TargetPort]),
    <<OrigHdr:4/binary, _/binary>> = Packet,
    case reset_packet_id(Packet, S#state.next_id) of
        {error, _Type, MsgForClient} ->
            ?DEBUG_TRACE("ERROR encountered in resetting packet ID - returning following message to Transaction Manager (PID ~w): ~w", [ClientPid, MsgForClient]),
            ClientPid ! MsgForClient,
            {noreply, S#state{ipbus_v=unknown, next_id=unknown} };
        {IPbusVer, ModRequest, PktId} ->
            gen_udp:send(Socket, TargetIPTuple, TargetPort, ModRequest),
            <<ModHdr:4/binary, _/binary>> = ModRequest,
            InFlightPkt = {ModHdr, now(), ModRequest, 0, ClientPid, OrigHdr},
            ch_stats:udp_out(),
            NewS = if
                     is_integer(PktId) ->
                       S#state{ipbus_v=IPbusVer, in_flight=InFlightPkt, next_id = increment_pkt_id(PktId)};
                     true ->
                       S#state{in_flight=InFlightPkt}
                   end,
           ?DEBUG_TRACE("Request packet sent. Now entering state ~w , with timeout of ~wms", [NewS, ?UDP_RESPONSE_TIMEOUT]),
           {noreply, NewS, ?UDP_RESPONSE_TIMEOUT}
    end.


%% ---------------------------------------------------------------------
%% @doc 
%% @throws Same as get_device_status/0
%% @spec reset_packet_id(RawIPbusRequestBin, Id) -> {ok, IPbusVer, ModIPbusRequest, PacketId}
%%                                                | {error, malformed, MsgForTransManager}
%%                                                | {error, timeout, MsgForTransManager}
%% ---------------------------------------------------------------------

reset_packet_id(RawIPbusRequest, NewId) ->
    {Ver, _, End} = parse_packet_header(RawIPbusRequest),
    case {Ver, NewId} of
        {{2,0}, _} when is_integer(NewId) ->
             <<H1:8, _:16, H2:8, PktBody/binary>> = RawIPbusRequest,
             case End of
                 big    -> {Ver, <<H1:8, NewId:16/big, H2:8, PktBody/binary>>, NewId};
                 little -> {Ver, <<H1:8, NewId:16/little, H2:8, PktBody/binary>>, NewId}
             end;
        {{2,0}, _} ->
            case get_device_status() of 
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
%% ---------------------------------------------------------------------

decrement_pkt_id(Id) when is_integer(Id), Id>0 ->
    if
      Id =:= 1 ->
        16#ffff;
      true ->
        Id - 1
    end.


%% ---------------------------------------------------------------------
%% @doc Increments packet ID looping round from 0xffff to 1 
%% @spec increment_pkt_id( Id:integer() ) -> IdPlusOne::integer
%% ---------------------------------------------------------------------

increment_pkt_id(Id) when is_integer(Id), Id>0 ->
    if
      Id=:=16#ffff ->
        1;
      true ->
        Id + 1
    end.

%% ---------------------------------------------------------------------
%% @doc ... TODO ...
%% @spec parse_packet_header( RawHeader::binary() ) -> {IPbusVer, Id, End}
%% where 
%%    IPbusVer = {2,0} | {1,3} | unknown
%%    Id::integer()
%%    End = big | little | unknown
%% @end
%% ---------------------------------------------------------------------

parse_packet_header(PacketBin) when size(PacketBin)>4 ->
    <<Header:4/binary, _/binary>> = PacketBin,
    parse_packet_header(Header);

parse_packet_header(<<16#20:8/big, Id:16/big, 16#f0:8/big>>) ->
    {{2,0}, Id, big};
parse_packet_header(<<16#f0:8/big, Id:16/little, 16#20:8/big>>) ->
    {{2,0}, Id, little};

parse_packet_header(<<1:4/big, _:12/big, 0:8, 16#f8/big>>) ->
    {{1,3}, notset, big};
parse_packet_header(<<16#f8/big, 0:8/big, _:8, 1:4/big, _:4>>) ->
    {{1,3}, notset, little};

parse_packet_header(_) ->
    {unknown, notset, unknown}.


%% ------------------------------------------------------------------------------------
%% @doc Returns re-send request packet with specified packet ID and endian-ness
%% @spec resend_request_pkt(Id :: integer(), End :: atom() ) ->
%%       where
%%         End = big | little
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
%% @spec get_device_status() ->   {ok, NrResponseBuffers, NextExpdId}
%%                              | {error, malformed, MsgForTransManager}
%%                              | {error, timeout, MsgForTransManager}
%% @end
%% ------------------------------------------------------------------------------------

get_device_status() ->
    RequestBin = <<2:4, 0:20, 16#f1:8, 0:(32*15)>>,
    ExpdResponseHdr = <<2:4, 0:20, 16#f1:8>>, 
    try sync_send_reply(RequestBin, ExpdResponseHdr, 4, ?UDP_RESPONSE_TIMEOUT) of 
        {ok, << 16#200000f1:32, _Word1:4/binary, 
                NrBuffers:32, 16#20:8, NextId:16, 16#f0:8, 
                _TheRest/binary >>} ->
            {ok, NrBuffers, NextId};
        {ok, _Response} ->
            ?DEBUG_TRACE("Malformed status response received from target at IP addr=~w, ipbus port=~w. Will now throw the atom 'malformed'",
                         [get(target_ip_tuple), get(target_port)]),
            ?PACKET_TRACE(_Response, "The following malformed status response has been received from target at IP addr=~w, ipbus port=~w.",
                                     [get(target_ip_tuple), get(target_port)]),
            log(error, "Malformed status response ~w received from target at ~w:~w", [_Response, get(target_tuple), get(target_port)]),
            {error, malformed, {device_client_response, get(target_ip_u32), get(target_port), ?ERRCODE_MALFORMED_STATUS, <<>> } }
    catch 
        throw:timeout ->
            log(error,"No response from target in get_device_status"),
            {error, timeout, {device_client_response, get(target_ip_u32), get(target_port), ?ERRCODE_TARGET_STATUS_TIMEOUT, <<>> } }
    end.


%% ------------------------------------------------------------------------------------
%% @doc Sends packet to device, waits for reply, and if timeout occurs, 
%%      retries specified number of times 
%% @throws timeout
%% @spec sync_send_reply(RequestBin, MaxNrSends, TimeoutEachSend) -> {ok, ResponseBin}
%%   where 
%%     RequestBin::binary()
%%     MaxNrSends::integer() > 0
%%     TimeoutEachSend::float()
%%   end
%% ------------------------------------------------------------------------------------

sync_send_reply(BinToSend, ReplyHdr, MaxNrSends, TimeoutEachSend) ->
    sync_send_reply(BinToSend, ReplyHdr, MaxNrSends, TimeoutEachSend, 0).


%% ------------------------------------------------------------------------------------
%% @doc Sends packet to device, and waits for reply, and loops back round to retry if 
%%      timeout occurs
%% @throws timeout
%% @spec sync_send_reply(RequestBin, ReplyHdr, MaxNrSends, TimeoutEachSend, NrAttemptsAlready)
%%                        -> {ok, ResponseBin}
%%   where
%%     RequestBin::binary()
%%     ReplyHdr::binary()
%%     MaxNrSends::integer() > 0
%%     TimeoutEachSend::float()
%%   end
%% ------------------------------------------------------------------------------------

sync_send_reply(_BinToSend, _ReplyHdr, MaxNrSends, TimeoutEachSend, MaxNrSends) ->
    log(error, "MAX NUMBER OF TIMEOUTS reached in sync_send_reply/4! No response from target after ~w attempts, each with timeout of ~wms. Throwing now.", [MaxNrSends, TimeoutEachSend]),
    throw(timeout);

sync_send_reply(BinToSend, ReplyHdr, MaxNrSends, TimeoutEachSend, SendCount) when is_binary(ReplyHdr), size(ReplyHdr)=:=4 ->
    Socket = get(socket),
    TargetIPTuple = get(target_ip_tuple),
    TargetPort = get(target_port),
    gen_udp:send(Socket, TargetIPTuple, TargetPort, BinToSend),
    ch_stats:udp_out(),
    receive
        {udp, Socket, TargetIPTuple, TargetPort, <<ReplyHdr:4/binary, _/binary>> = ReplyBin} -> 
            ?DEBUG_TRACE("In sync_send_reply/5 : Received response from target (IP addr=~w, port=~w) on attempt no. ~w of ~w", 
                         [TargetIPTuple, TargetPort, SendCount+1, MaxNrSends]),
            ch_stats:udp_in(),
            {ok, ReplyBin}
    after TimeoutEachSend ->
        log(warning, "TIMEOUT waiting for response in sync_send_reply/5! No response from target on attempt no. ~w of ~w.", [SendCount+1, MaxNrSends]),
        ch_stats:udp_response_timeout(status),
        sync_send_reply(BinToSend, ReplyHdr, MaxNrSends, TimeoutEachSend, SendCount+1)
    end.


%% ------------------------------------------------------------------------------------
%% @doc Returns tuple containing target IP tuple & port retrieved from target_ip_tuple
%%       and target_port entries in process dict
%% @spec target_ip_port() -> {TargetIPTuple, TargetPort}
%% @end
%% ------------------------------------------------------------------------------------
target_ip_port() ->
    {get(target_ip_tuple), get(target_port)}.


%% ------------------------------------------------------------------------------------
%% @doc Prints an info/warning/error message (currently to std::out), with the State
%%      appended to the end of the message in an easy-to-read format if given as an argument
%%
%% @spec log(Level | {Level, State}, MsgString) -> ok
%% @end
%% ------------------------------------------------------------------------------------

log(Arg1, String) ->
   log(Arg1, String, []).


%% ------------------------------------------------------------------------------------
%% @doc Prints an info/warning/error message (currently to std::out), with the State
%%      appended to the end of the message in an easy-to-read format if given as an argument
%%
%% @spec log(Level | {Level, State}, MsgFmtString, MsgData) -> ok
%% @end
%% ------------------------------------------------------------------------------------

log(Level, MsgString, MsgData) when Level=:=info; Level=:=warning; Level=:=error ->
    {IPTuple, Port} = target_ip_port(),
    Preamble = io_lib:format(" ch_device_client : Pid=~w , target @ ~w:~w      Time: ~w~n", [self(), IPTuple, Port, now()]),
    case Level of
        info    -> error_logger:info_msg(   lists:append(Preamble, MsgString), MsgData);
        warning -> error_logger:warning_msg(lists:append(Preamble, MsgString), MsgData);
        error   -> error_logger:error_msg(  lists:append(Preamble, MsgString), MsgData)
    end,
    ok;
log({Level, State}, MsgString, MsgData) when is_record(State, state) ->
    log(Level, lists:flatten([state_as_string(State), MsgString, "~n"]), MsgData).


%% ------------------------------------------------------------------------------------
%% @doc Returns string summarising state record in nice easy to read (multi-line) format
%%
%% @spec state_as_string( State::string() ) -> string()
%% @end
%% ------------------------------------------------------------------------------------

state_as_string(S) when is_record(S, state) ->
    io_lib:format(" State:  mode = ~w   ||   ipbus version = ~w   ||   next_id = ~w~n"
                  "         target is at ~w (~w) port ~w~n"
                  "         in_flight = ~w~n"
                  "         queue     = ~w~n",
                  [S#state.mode, S#state.ipbus_v, S#state.next_id, 
                   S#state.target_ip_tuple, S#state.target_ip_u32, S#state.target_port,
                   S#state.in_flight, 
                   S#state.queue]
                  ).
