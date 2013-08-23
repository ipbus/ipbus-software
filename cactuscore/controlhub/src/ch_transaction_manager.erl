%%% ===========================================================================
%%% @author Robert Frazier
%%% @author Tom Williams
%%%
%%% @since May 2012
%%%
%%% @doc A Transaction Manager is a process that accepts an incoming TCP
%%%      connection from a (microHAL) client, and then manages requests from
%%%      it for the lifetime of the connection.
%%% @end
%%% ===========================================================================
-module(ch_transaction_manager).

%%
%% Include files
%%
-include("ch_global.hrl").
-include("ch_timeouts.hrl").
-include("ch_tcp_server_params.hrl").
-include("ch_error_codes.hrl").

%%
%% Exported Functions
%%
-export([start_link/1, tcp_acceptor/1]).

%%
%% API Functions
%%

%% ---------------------------------------------------------------------
%% @doc Starts up a transaction manager process that will wait for a
%%      client to connect on the provided TCP socket. On acceptance of
%%      a connection (or an accept error), the function: 
%%        ch_tcp_listener:connection_accept_completed()
%%      is called.  The transaction manager process reaches end of life
%%      when the TCP connection is terminated by the microHAL client.
%%
%% @spec start_link(TcpListenSocket::socket) -> ok
%% @end
%% ---------------------------------------------------------------------
start_link(TcpListenSocket) ->
    ?CH_LOG_DEBUG("Spawning new transaction manager."),
    proc_lib:spawn_link(?MODULE, tcp_acceptor, [TcpListenSocket]),
    ok.
    

%% Blocks until a client connection is accepted on the given socket.
%% This function is really "private", and should only be called via
%% the start_link function above.  Need to find a way of refactoring
%% this module so it doesn't need to be an external function.
tcp_acceptor(TcpListenSocket) ->
    ?CH_LOG_DEBUG("Transaction manager born. Waiting for TCP connection..."),
    case gen_tcp:accept(TcpListenSocket) of
        {ok, Socket} ->
            ch_tcp_listener:connection_accept_completed(),
            ch_stats:client_connected(),
            case inet:peername(Socket) of
                {ok, {ClientAddr, ClientPort}} ->
                    TcpPid = proc_lib:spawn_link(fun tcp_proc_init/0),
                    put(tcp_pid, TcpPid),
                    gen_tcp:controlling_process(Socket, TcpPid),
                    TcpPid ! {start, Socket, self()},
                    ?CH_LOG_INFO("TCP socket accepted from client at ~s. Socket options: ~w~n",
                                 [ch_utils:ip_port_string(ClientAddr, ClientPort), inet:getopts(TcpListenSocket, [active, buffer, low_watermark, high_watermark, recbuf, sndbuf])]),
                    transaction_manager_loop(TcpPid, Socket, unknown, unknown, unknown, 1, queue:new(), []);
                _Else ->
                    ch_stats:client_disconnected(),
                    ?CH_LOG_ERROR("Socket error whilst getting peername.")
            end;
        {error, _Reason} ->
            ch_tcp_listener:connection_accept_completed(),
            % Something funny happened whilst trying to accept the socket.
            ?CH_LOG_ERROR("Error (~p) occurred during TCP accept.", [_Reason]);
        _Else ->  % Belt and braces...
            ch_tcp_listener:connection_accept_completed(),
            ?CH_LOG_ERROR("Unexpected event (~p) occurred during TCP accept.", [_Else])
    end,
    ?CH_LOG_INFO("I am now redundant; exiting normally."),
    ok.



%%% --------------------------------------------------------------------
%%% Internal functions
%%% --------------------------------------------------------------------


% initialisation function for TCP receive/send process
tcp_proc_init() ->
    receive
        {start, Socket, ParentPid} ->
            link(ParentPid),
            inet:setopts(Socket, [lists:keyfind(active, 1, ?TCP_SOCKET_OPTIONS)]),
            tcp_proc_loop(Socket, ParentPid)
    end.


%% Tail-recursive loop function for TCP receive/send process
tcp_proc_loop(Socket, ParentPid) ->
    % Give priority to receiving packets off TCP stream (wrt sending back to uHAL client)
    receive
        {tcp, Socket, Packet} ->
            ParentPid ! {tcp, Socket, Packet}
    after 0 ->
        receive
            {send, Pkt} ->
                true = erlang:port_command(Socket, Pkt, []);
            {inet_reply, Socket, ok} ->
                void;
            {inet_reply, Socket, SendError} ->
                ?CH_LOG_ERROR("Error in TCP async send: ~w", [SendError]),
                throw({tcp_send_error,SendError});
            Other ->
                ParentPid ! Other
        end
    end,
    tcp_proc_loop(Socket, ParentPid).


%% Main transaction_manager operation loop (tail-recursive)
%%   - entered when accept off TCP socket
%%   - if socket closes, then this function exits
transaction_manager_loop(TcpPid, Socket, TargetIPAddrArg, TargetPortArg, DevClientPid, NrInFlight, QNrReqsPerTcp, ReplyIoList) ->
    receive
        {tcp, Socket, RequestBin} ->
            if
              (byte_size(RequestBin) rem 4) =:= 0 ->
                ?CH_LOG_DEBUG("Received TCP chunk."),
                {TargetIPU32, TargetPort, NrReqs} = unpack_and_enqueue(RequestBin, pid_unknown, 0),
                transaction_manager_loop(TcpPid, Socket, TargetIPU32, TargetPort, none, NrInFlight+NrReqs, queue:in(NrReqs, QNrReqsPerTcp), ReplyIoList);
              true ->
                ?CH_LOG_ERROR("Each TCP chunk should be an integer number of 32-bit words, but received chunk of length ~w bytes for ~s. This TCP chunk will be ignored.", [byte_size(RequestBin), ch_utils:ip_port_string(TargetIPAddrArg, TargetPortArg)]),
                ch_stats:client_request_malformed(),
                transaction_manager_loop(TcpPid, Socket, TargetIPAddrArg, TargetPortArg, DevClientPid, NrInFlight, QNrReqsPerTcp, ReplyIoList)
            end;

        {device_client_response, TargetIPAddrArg, TargetPortArg, ErrorCode, ReplyIoData} ->
            NrRepliesToSend = element(2, queue:peek(QNrReqsPerTcp)),
            case ((lists:flatlength(ReplyIoList) + 3) div 3) of
                NrRepliesToSend ->
                    ?CH_LOG_DEBUG("Sending ~w IPbus response packets over TCP.", [NrRepliesToSend]),
                    TcpPid ! {send, [ReplyIoList, <<(iolist_size(ReplyIoData) + 8):32, TargetIPAddrArg:32, TargetPortArg:16, ErrorCode:16>>, ReplyIoData]},
                    transaction_manager_loop(TcpPid, Socket, TargetIPAddrArg, TargetPortArg, DevClientPid, NrInFlight-1, queue:drop(QNrReqsPerTcp), []);
                _NrRepliesAccumulated ->
                    ?CH_LOG_DEBUG("IPbus response packet received. Accumulating for TCP send (~w needed for next TCP chunk, ~w now accumulated).", [NrRepliesToSend, _NrRepliesAccumulated]),
                    transaction_manager_loop(TcpPid, Socket, TargetIPAddrArg, TargetPortArg, DevClientPid, NrInFlight-1, QNrReqsPerTcp, 
                                             [ReplyIoList, <<(iolist_size(ReplyIoData) + 8):32, TargetIPAddrArg:32, TargetPortArg:16, ErrorCode:16>>, ReplyIoData])
            end;

        {tcp_closed, Socket} ->
            ch_stats:client_disconnected(),
            if
              NrInFlight > 0 ->
                ?CH_LOG_WARN("TCP socket from ~s was closed early - there is still ~w replies pending for that socket. Presumably the socket was closed by the TCP client.", [ch_utils:tcp_peeername_string(Socket), NrInFlight]);
              true ->
                ?CH_LOG_INFO("TCP socket closed.")
            end;

        {tcp_error, Socket, _Reason} ->
            % Assume this ends up with the socket closed from a stats standpoint
            ch_stats:client_disconnected(),
            if
              NrInFlight > 0 ->
                ?CH_LOG_ERROR("TCP socket error (~p). The ~w pending IPbus reply packets will not be forwarded.", [_Reason, NrInFlight]);
              true ->
                ?CH_LOG_WARN("TCP socket error (~p). Currenly no pending IPbus reply packets.", [_Reason])
            end;

        Else ->
            ?CH_LOG_WARN("Received and ignoring unexpected message: ~p", [Else]),
            transaction_manager_loop(TcpPid, Socket, TargetIPAddrArg, TargetPortArg, DevClientPid, NrInFlight, QNrReqsPerTcp, ReplyIoList)
    end.


unpack_and_enqueue(<<TargetIPAddr:32, TargetPort:16, NrInstructions:16, IPbusReq:NrInstructions/binary-unit:32, Tail/binary>>, Pid, NrSent) ->
    RetPid = enqueue_request(TargetIPAddr, TargetPort, Pid, IPbusReq),
    if
      byte_size(Tail) > 0 ->
        unpack_and_enqueue(Tail, RetPid, NrSent+1);
      true ->
        ?CH_LOG_DEBUG("~w IPbus packets in last TCP chunk", [(NrSent+1)]),
        {TargetIPAddr, TargetPort, NrSent+1}
    end;
unpack_and_enqueue(Binary, _Pid, NrSent) when byte_size(Binary) < 12 ->
    ?CH_LOG_ERROR("Binary nr ~w unpacked from TCP chunk (~w) contained less than three 32-bit words, which is invalid according to uHAL-ControlHub protocol. Ignoring remainder of this chunk and continuing.", [NrSent+1, Binary]),
    ch_stats:client_request_malformed(),
    {unknown, unknown, NrSent};
unpack_and_enqueue(<<TargetIPAddr:32, TargetPort:16, NrInstructions:16, TailBin/binary>>, _Pid, NrSent) ->
    ?CH_LOG_ERROR("Not enough bytes (only ~w) left in TCP chunk to unpack IPbus packet of length ~w bytes for ~s (binary nr ~w in this TCP chunk). Ignoring remainder of this chunk and continuing.", [byte_size(TailBin), 4*NrInstructions, ch_utils:ip_port_string(TargetIPAddr, TargetPort), NrSent+1]),
    ch_stats:client_request_malformed(),
    {TargetIPAddr, TargetPort, NrSent}.


enqueue_request(_IPaddrU32, _PortU16, DestPid, IPbusRequest) when is_pid(DestPid) ->
    ?CH_LOG_DEBUG("Enqueueing IPbus request packet for ~s to PID ~w", [ch_utils:ip_port_string(_IPaddrU32,_PortU16), DestPid]),
    gen_server:cast(DestPid, {send, IPbusRequest, self()}),
    DestPid;
enqueue_request(IPaddrU32, PortU16, _NotPid, IPbusRequest) ->
    ?CH_LOG_DEBUG("Looking up device_client PID"),
    {ok, Pid} = ch_device_client_registry:get_pid(IPaddrU32, PortU16),
    enqueue_request(IPaddrU32, PortU16, Pid, IPbusRequest).
