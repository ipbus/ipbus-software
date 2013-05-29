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
        {ok, ClientSocket} ->
            ch_tcp_listener:connection_accept_completed(),
            ch_stats:client_connected(),
            case inet:peername(ClientSocket) of
                {ok, {_ClientAddr, _ClientPort}} ->
                    ?CH_LOG_INFO("TCP socket accepted from client at IP addr=~w, port=~p~n"
                                 "Socket options: ~w~n",
                                 [_ClientAddr, _ClientPort, inet:getopts(TcpListenSocket, [buffer,low_watermark,high_watermark, recbuf, sndbuf])]),
                    tcp_receive_handler_loop(ClientSocket, queue:new(), 0);
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

%% Receive loop the incoming TCP requests; if socket closes the function exits the receive loop.
tcp_receive_handler_loop(ClientSocket, RequestTimesQueue, QueueLen) ->
    TargetIPaddr = get(target_ip_addr),
    TargetPort   = get(target_port),
    NewTimeout = case queue:is_empty(RequestTimesQueue) of
                   true ->
                     infinity;
                   false ->
                     TimeSinceLastResponse = timer:now_diff(os:timestamp(), queue:get(RequestTimesQueue)) div 1000,
                     max(0, ?RESPONSE_FROM_DEVICE_CLIENT_TIMEOUT - TimeSinceLastResponse)
                 end,
    if
      QueueLen =:= 30 -> 
        inet:setopts(ClientSocket,[{active,once}]);
      QueueLen =:= 20 ->
        inet:setopts(ClientSocket,[{active,true}]);
      true ->
        void
    end,
    receive
        {tcp, ClientSocket, RequestBin} ->
%            inet:setopts(ClientSocket,[{activve,once}]),
            ?CH_LOG_DEBUG("Received a request from client (QueueLen=~w).", [QueueLen]),
            ch_stats:client_request_in(),
            forward_request(RequestBin),
            tcp_receive_handler_loop(ClientSocket, queue:in(os:timestamp(),RequestTimesQueue), QueueLen+1);
        {device_client_response, TargetIPaddr, TargetPort, ErrorCode, TargetResponseBin} ->
            ?CH_LOG_DEBUG("Received device client response from target IPaddr=~w, Port=~w",
                          [ch_utils:ipv4_u32_addr_to_tuple(TargetIPaddr), TargetPort]),
            reply_to_client(ClientSocket, <<(byte_size(TargetResponseBin) + 8):32, TargetIPaddr:32, TargetPort:16, ErrorCode:16, TargetResponseBin/binary>>),
            tcp_receive_handler_loop(ClientSocket, queue:drop(RequestTimesQueue), QueueLen-1 );
        {tcp_closed, ClientSocket} ->
            ch_stats:client_disconnected(),
            ?CH_LOG_INFO("TCP socket closed.");
        {tcp_error, ClientSocket, _Reason} ->
            % Assume this ends up with the socket closed from a stats standpoint
            ch_stats:client_disconnected(),
            ?CH_LOG_WARN("TCP socket error (~p).", [_Reason]);
        _Else ->
            ?CH_LOG_WARN("Received and ignoring unexpected message: ~p", [_Else]),
            tcp_receive_handler_loop(ClientSocket, RequestTimesQueue, QueueLen)
    after NewTimeout ->
        ?CH_LOG_WARN("Timeout whilst awaiting response from device client for target IPaddr=~w, Port=~p."
                     " Generating timeout error response for this target so we can continue.", [ch_utils:ipv4_u32_addr_to_tuple(TargetIPaddr), TargetPort]),
        reply_to_client(ClientSocket, <<8:32, TargetIPaddr:32, TargetPort:16, ?ERRCODE_CH_DEVICE_CLIENT_TIMEOUT:16>>),
        tcp_receive_handler_loop(ClientSocket, queue:drop(RequestTimesQueue), QueueLen-1 )
    end.


%% Top-level function that forwards the client requests to the device_client process.
forward_request(RequestBin) ->
    try unpack_target_request(RequestBin) of
        {TargetIPaddr, TargetPort, IPbusRequest} ->
            ch_device_client:enqueue_requests(TargetIPaddr, TargetPort, IPbusRequest),
            put(target_ip_addr, TargetIPaddr),
            put(target_port, TargetPort)
    catch
        throw:{malformed, _WhyMalformed} ->
          ?CH_LOG_WARN("Malformed (~w) client request received; will ignore.", [_WhyMalformed]),
          ?PACKET_TRACE(RequestBin, "WARNING!~n  Received and ignoring this malformed (~w) packet:", [_WhyMalformed]),
          ch_stats:client_request_malformed()
    end.


%% Breaks down received request into board IP address, port number and the IPbus request packet
unpack_target_request(RequestBin) ->
    % Test if there are an integer number of 32-bit words before proceeding
    case byte_size(RequestBin) of
        % Test for integer number of 32-bit words
        NrBytes when (NrBytes rem 4) /= 0 ->
            throw({malformed, 'non-integer number of 32-bit words'});
        % Test that received at least 3 words
        NrBytes when (NrBytes < 12) -> 
            throw({malformed, 'less than 3 words'});
        _ -> void
    end,
    <<TargetIPaddr:32,
      TargetPort:16, _NumInstructions:16,
      Remainder/binary>> = RequestBin,
    case byte_size(Remainder) =:= _NumInstructions*4 of
        false ->
            throw({malformed, 'Number of instructions in packet does not match header'});
        true ->
            {TargetIPaddr, TargetPort, Remainder}
    end.


%% Sends reply back to TCP client
reply_to_client(ClientSocket, ResponseBin) ->
    ?CH_LOG_DEBUG("Sending response to TCP client"),
    ?PACKET_TRACE(ResponseBin, "~n  Sending the following response to the TCP client:"),
    gen_tcp:send(ClientSocket, ResponseBin),
    ch_stats:client_response_sent().
