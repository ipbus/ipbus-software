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

-record(state, {socket, v_major=unknown, nextpktid, in_flight=none, queue=[]}).           % Holds network socket to target device.


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
    put(target_ip_tuple, ch_utils:ipv4_u32_addr_to_tuple(IPaddrU32)),
    put(target_port, PortU16),
    
    % Try opening ephemeral port and we want data delivered as a binary.
    case gen_udp:open(0, [binary]) of   
        {ok, Socket} ->
            put(socket, Socket),
            {ok, #state{socket = Socket}};
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

% handle_cast callback for enqueue_requests API call.

handle_cast({send, RequestPacket, ClientPid}, S = #state{queue=Queue}) ->
    ?DEBUG_TRACE("IPbus request packet received from Transaction Manager with PID = ~w.", [ClientPid]),
    ?PACKET_TRACE(RequestPacket, "The following IPbus request have been received from Transaction "
                  "Manager with PID = ~w.", [ClientPid]),
    if
      S#state.in_flight =:= none ->
        send_request_to_board({RequestPacket, ClientPid}, S);
      true ->
        ?DEBUG_TRACE("This request packet is being queued (max nr packets already in flight)."),
        {noreply, S#state{queue=lists:append(Queue, [{RequestPacket, ClientPid}])}}
    end;

%% Default handle cast
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({udp, Socket, TargetIPTuple, TargetPort, HardwareReply}, S) when Socket=:=S#state.socket ->
    ?DEBUG_TRACE("Received response from target hardware at IP addr=~w, port=~w. "
                 "Passing it to originating Transaction Manager...", [TargetIPTuple, TargetPort]),
    ch_stats:udp_in(),
    {_HdrSent, _TimeSent, _PktSent, _RetryCount, ClientPid, OrigHdr} = S#state.in_flight,
    ?DEBUG_TRACE("State.v_major is ~w~n", [S#state.v_major]),
    case S#state.v_major of
        2 ->
            ?DEBUG_TRACE("Entered case statement with state.v_major = 2"),
            <<_:4/binary, ReplyBody/binary>> = HardwareReply,
            ClientPid ! { device_client_response, get(target_ip_u32), TargetPort, ?ERRCODE_SUCCESS, <<OrigHdr/binary, ReplyBody/binary>>},
            ?DEBUG_TRACE("Hardware response was just sent back to Transaction Manager.");
        _ ->
            ClientPid ! { device_client_response, get(target_ip_u32), TargetPort, ?ERRCODE_SUCCESS, HardwareReply }
    end,
    if
      S#state.queue =:= [] ->
        {noreply, S#state{in_flight=none}};
      true ->
        [H|T] = S#state.queue,
        send_request_to_board(H, S#state{queue=T, in_flight=none})
    end;

handle_info(timeout, S = #state{socket=Socket, nextpktid=NextId}) when S#state.in_flight=/=none ->
    TargetIPTuple = get(target_ip_tuple),
    TargetPort = get(target_port),
    {_HdrSent, TimeSent, PktSent, RetryCount, ClientPid, _OrigHdr} = S#state.in_flight,
    ?DEBUG_TRACE("No response from target hardware at IP addr=~w, port=~w. "
                 "Checking on status of hardware...", [TargetIPTuple, TargetPort]),
    if
      (S#state.v_major=:=2) and (RetryCount<3) ->
        NewInFlight = {_HdrSent, TimeSent, PktSent, RetryCount+1, ClientPid, _OrigHdr},
        case get_device_status() of 
            % Request packet lost => re-send
            {ok, HwNextId} when HwNextId =:= (NextId-1) -> 
                gen_udp:send(Socket, TargetIPTuple, TargetPort, PktSent),
                ch_stats:udp_out(),
                {noreply, S#state{in_flight=NewInFlight}, ?UDP_RESPONSE_TIMEOUT};
            % Response packet lost => Ask board to re-send
            {ok, HwNextId} when HwNextId =:= NextId ->
                gen_udp:send(Socket, TargetIPTuple, TargetPort+2, <<16#deadbeef:32>>),
                ch_stats:udp_out(),
                {noreply, S#state{in_flight=NewInFlight}, ?UDP_RESPONSE_TIMEOUT};
            % Error in getting device status
            {error, _Type, MsgForTransManager} ->
                ClientPid ! MsgForTransManager,
                {noreply, S#state{in_flight=none}}
        end;
      true ->
        ?DEBUG_TRACE("TIMEOUT REACHED! No response from target (IPaddr=~w, port=~w) . Generating and sending "
                     "a timeout response to originating Transaction Manager...", [TargetIPTuple, TargetPort]),
        ch_stats:udp_response_timeout(),
        ClientPid ! { device_client_response, get(target_ip_u32), TargetPort, ?ERRCODE_TARGET_CONTROL_TIMEOUT, <<>> },
        {noreply, S#state{in_flight=none}}
    end;

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

%% ------------------------------------------------------------------------------
%% @doc Sends a control request packet to the board, returning the 
%%      new return value for the handle_{call,cast,info} function that 
%%      it's called from
%% @spec send_request_to_board({Packet::binary(), ClientPid::pid()}, S::state())
%%                        -> {noreply, NewState}
%%                        -> {noreply, NewState, Timeout}
%% ------------------------------------------------------------------------------

send_request_to_board({Packet, ClientPid}, S = #state{socket=Socket}) ->
    TargetIPTuple = get(target_ip_tuple),
    TargetPort = get(target_port),
    ?DEBUG_TRACE("Request packet from PID ~w is being forwarded to the board at IP ~w, port ~w...", [ClientPid, TargetIPTuple, TargetPort]),
    <<OrigHdr:4/binary, _/binary>> = Packet,
    case reset_packet_id(Packet, S#state.nextpktid) of
        {error, _Type, MsgForClient} ->
            ?DEBUG_TRACE("ERROR encountered in resetting packet ID - returning following message to Transaction Manager (PID ~w): ~w", [ClientPid, MsgForClient]),
            ClientPid ! MsgForClient,
            {noreply, S#state{v_major=unknown, nextpktid=unknown} };
        {MajVer, ModRequest, PktId} ->
            gen_udp:send(Socket, get(target_ip_tuple), get(target_port), ModRequest),
            <<ModHdr:4/binary, _/binary>> = ModRequest,
            InFlightPkt = {ModHdr, now(), ModRequest, 0, ClientPid, OrigHdr},
            ch_stats:udp_out(),
            NewS = if
                     is_integer(PktId) ->
                       S#state{v_major=MajVer, in_flight=InFlightPkt, nextpktid = increment_pkt_id(PktId)};
                     true ->
                       S#state{in_flight=InFlightPkt}
                   end,
           ?DEBUG_TRACE("Request packet sent. Now entering state ~w , with timeout of ~wms", [NewS, ?UDP_RESPONSE_TIMEOUT]),
           {noreply, NewS, ?UDP_RESPONSE_TIMEOUT}
    end.


%% ---------------------------------------------------------------------
%% @doc 
%% @throws Same as get_device_status/0
%% @spec reset_packet_id(RawIPbusRequestBin, Id) -> {ok, MajorVer, ModIPbusRequest, PacketId}
%%                                                | {error, malformed, MsgForTransManager}
%%                                                | {error, timeout, MsgForTransManager}
%% ---------------------------------------------------------------------

reset_packet_id(RawIPbusRequest, NewId) ->
    {MajorVer, MinorVer, _, End} = parse_packet_header(RawIPbusRequest),
    case {MajorVer, MinorVer, NewId} of
        {2, 0, _} when is_integer(NewId) ->
             <<H1:8, _:16, H2:8, PktBody/binary>> = RawIPbusRequest,
             case End of
                 big    -> {MajorVer, <<H1:8, NewId:16/big, H2:8, PktBody/binary>>, NewId};
                 little -> {MajorVer, <<H1:8, NewId:16/little, H2:8, PktBody/binary>>, NewId}
             end;
        {2, 0, _} ->
            case get_device_status() of 
                {error, _Type, _MsgForTransManager} = X ->
                   X;
                {_, IdFromStatus} ->
                   reset_packet_id(RawIPbusRequest, IdFromStatus)
             end;
        _ ->
             {MajorVer, RawIPbusRequest, notset}
    end.

%% ---------------------------------------------------------------------
%% @doc Increments packet ID looping round from 0xffff to 1 
%% @spec increment_pkt_id( Id:integer() ) -> IdPlusOne::integer
%% ---------------------------------------------------------------------

increment_pkt_id(Id) when is_integer(Id) and Id>0 ->
    if
      Id=:=16#ffff ->
        1;
      true ->
        Id + 1
    end.

%% ---------------------------------------------------------------------
%% @doc ... TODO ...
%% @spec parse_packet_header( RawHeader::binary() ) -> {MajorVer, MinorVer, Id, End}
%% ---------------------------------------------------------------------

parse_packet_header(PacketBin) when size(PacketBin)>4 ->
    <<Header:4/binary, _/binary>> = PacketBin,
    parse_packet_header(Header);
parse_packet_header(<<16#20:8/big, Id:16/big, 16#f0:8/big>>) ->
    {2, 0, Id, big};
parse_packet_header(<<16#f0:8/big, Id:16/little, 16#20:8/big>>) ->
    {2, 0, Id, little};
parse_packet_header(_) ->
    {1, unknown, notset, unknown}.


%% ------------------------------------------------------------------------------------
%% @doc Retrieves number of response buffers and next expected packet ID for the
%%       device, assuming it's an IPbus 2.0 device. Returns tuple beginning with
%%       atom error in case there was a timeout or response was malformed.
%% @spec get_device_status() ->   {NrResponseBuffers, NextExpdId}
%%                              | {error, malformed, MsgForTransManager}
%%                              | {error, timeout, MsgForTransManager}
%% ------------------------------------------------------------------------------------

get_device_status() ->
    RequestBin = <<2:4, 0:20, 16#f1:8, 0:(32*15)>>,
    try sync_send_reply(RequestBin, 2, ?UDP_RESPONSE_TIMEOUT) of 
        {ok, << 16#200000f1:32, _Word1:4/binary, 
                NrBuffers:32, 16#20:8, NextId:16, 16#f0:8, 
                _TheRest/binary >>} ->
            {NrBuffers, NextId};
        {ok, _Response} ->
            ?DEBUG_TRACE("Malformed status response received from target at IP addr=~w, ipbus port=~w. Will now throw the atom 'malformed'",
                         [get(target_ip_tuple), get(target_port)]),
            ?PACKET_TRACE(_Response, "The following malformed status response has been received from target at IP addr=~w, ipbus port=~w.",
                                     [get(target_ip_tuple), get(target_port)]),
            {error, malformed, {device_client_response, get(target_ip_u32), get(target_port), ?ERRCODE_MALFORMED_STATUS, <<>> } }
    catch 
        throw:timeout ->
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

sync_send_reply(BinToSend, MaxNrSends, TimeoutEachSend) ->
    sync_send_reply(BinToSend, MaxNrSends, TimeoutEachSend, 0).


%% ------------------------------------------------------------------------------------
%% @doc Sends packet to device, and waits for reply, and loops back round to retry if 
%%      timeout occurs
%% @throws timeout
%% @spec sync_send_reply(RequestBin, MaxNrSends, TimeoutEachSend, NrAttemptsAlready)
%%                        -> {ok, ResponseBin}
%%   where
%%     RequestBin::binary()
%%     MaxNrSends::integer() > 0
%%     TimeoutEachSend::float()
%%   end
%% ------------------------------------------------------------------------------------

sync_send_reply(_BinToSend, MaxNrSends, TimeoutEachSend, MaxNrSends) ->
    ?DEBUG_TRACE("MAX NUMBER OF TIMEOUTS in sync_send_reply/4! No response from target (IPaddr=~w, port=~w) after ~w attempts, each with timeout of ~wms"
                 "Throwing now", [get(target_ip_tuple), get(target_port), MaxNrSends, TimeoutEachSend]),
    throw(timeout);

sync_send_reply(BinToSend, MaxNrSends, TimeoutEachSend, SendCount) ->
    Socket = get(socket),
    TargetIPTuple = get(target_ip_tuple),
    TargetPort = get(target_port),
    gen_udp:send(Socket, TargetIPTuple, TargetPort, BinToSend),
    ch_stats:udp_out(),
    receive
        {udp, Socket, TargetIPTuple, TargetPort, ReplyBin} -> 
            ?DEBUG_TRACE("In sync_send_reply/4 : Received response from target (IP addr=~w, port=~w) on attempt no. ~w of ~w", 
                         [TargetIPTuple, TargetPort, SendCount+1, MaxNrSends]),
            ch_stats:udp_in(),
            {ok, ReplyBin}
    after TimeoutEachSend ->
        ?DEBUG_TRACE("TIMEOUT REACHED in sync_send_reply/4! No response from target (IPaddr=~w, port=~w) on attempt no. ~w of ~w."
                     " I might try again ...", [TargetIPTuple, TargetPort, SendCount+1, MaxNrSends]),
        ch_stats:udp_response_timeout(),
        sync_send_reply(BinToSend, MaxNrSends, TimeoutEachSend, SendCount+1)
    end.


