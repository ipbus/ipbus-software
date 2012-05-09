%%% ===========================================================================
%%% @author Robert Frazier
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
%% @spec start(TcpListenSocket::socket) -> ok
%% @end
%% ---------------------------------------------------------------------
start_link(TcpListenSocket) ->
    ?DEBUG_TRACE("Spawning new transaction manager."),
    spawn_link(?MODULE, tcp_acceptor, [TcpListenSocket]),
    ok.
    

%% Blocks until a client connection is accepted on the given socket.
%% This function is really "private", and should only be called via
%% the start_link function above.  Need to find a way of refactoring
%% this module so it doesn't need to be an external function.
tcp_acceptor(TcpListenSocket) ->
    ?DEBUG_TRACE("Transaction manager born. Waiting for TCP connection..."),
    case gen_tcp:accept(TcpListenSocket) of
        {ok, ClientSocket} ->
            ch_tcp_listener:connection_accept_completed(),
            ch_stats:client_connected(),
            case inet:peername(ClientSocket) of
                {ok, {ClientAddr, ClientPort}} ->
                    ?DEBUG_TRACE("TCP socket accepted from client at IP addr=~p, port=~p", [ClientAddr, ClientPort]),
                    tcp_receive_handler_loop(ClientSocket);
                _Else ->
                    ch_stats:client_disconnected(),
                    ?DEBUG_TRACE("Socket error whilst getting peername.")
            end;
        {error, _Reason} ->
            ch_tcp_listener:connection_accept_completed(),
            % Something funny happened whilst trying to accept the socket.
            ?DEBUG_TRACE("Error (~p) occurred during TCP accept.", [_Reason]);
        _Else ->  % Belt and braces...
            ch_tcp_listener:connection_accept_completed(),
            ?DEBUG_TRACE("Unexpected event (~p) occurred during TCP accept.", [_Else])
    end,
    ?DEBUG_TRACE("I am now redundant; exiting normally."),
    ok.



%%% --------------------------------------------------------------------
%%% Internal functions
%%% --------------------------------------------------------------------

%% Receive loop the incoming TCP requests; if socket closes the function exits the receive loop.
tcp_receive_handler_loop(ClientSocket) ->
    receive
        {tcp, ClientSocket, RequestBin} ->
            ?DEBUG_TRACE("Received a request from client."),
            ch_stats:client_request_in(),
            process_request(ClientSocket, RequestBin),
            tcp_receive_handler_loop(ClientSocket);
        {tcp_closed, ClientSocket} ->
            ch_stats:client_disconnected(),
            ?DEBUG_TRACE("TCP socket closed.");
        {tcp_error, ClientSocket, _Reason} ->
            % Assume this ends up with the socket closed from a stats standpoint
            ch_stats:client_disconnected(),
            ?DEBUG_TRACE("TCP socket error (~p).", [_Reason]);
        _Else ->
            ?DEBUG_TRACE("WARNING! Received and ignoring unexpected message: ~p", [_Else]),
            tcp_receive_handler_loop(ClientSocket)
    end.


%% Top-level function for actually dealing with the client request and returning a response.
process_request(ClientSocket, RequestBin) ->
    try unpack_target_requests(RequestBin) of
       TargetRequestList ->
           TargetResponseOrder = dispatch_to_device_clients(TargetRequestList),
           FullResponseBin = gather_device_client_responses(TargetResponseOrder),
           reply_to_client(ClientSocket, FullResponseBin)
    catch
        throw:{malformed, WhyMalformed} ->
          ?DEBUG_TRACE("WARNING! Malformed (~p) client request received; will ignore.", [WhyMalformed]),
          ?PACKET_TRACE(RequestBin, "WARNING!~n  Received and ignoring this malformed (~p) packet:", [WhyMalformed]),
          ch_stats:client_request_malformed()
    end.


%% Top-level function for breaking down the request into its various 
%% sections (i.e. the sets of IPbus instructions for specific targets)
unpack_target_requests(RequestBin) ->
    % Test if there are an integer number of 32-bit words before proceeding
    case ((size(RequestBin) rem 4) /= 0) of 
        false -> 
            target_request_accumulator(RequestBin);
        true ->
            throw({malformed, 'non-integer number of 32-bit words'})
    end.

%% Accumulate a list of the target requests
%% @spec target_request_accumulator(RequestBin::binary()) -> TargetRequestList
%%       TargetRequestList = [TargetRequest, ..]
%%       TargetRequest = {IPaddrU32::integer(), PortU16::integer, Instructions::binary()}
target_request_accumulator(RequestBin) ->
  target_request_accumulator([], RequestBin).

%% Implements target_request_accumulator/1
target_request_accumulator(TargetRequestList,  <<TargetIPaddr:32,
                                                 NumInstructions:16,
                                                 TargetPort:16,
                                                 Remainder/binary>>) ->
    NumBitsForInstructions = 32 * NumInstructions,
    case Remainder of
        <<Instructions:NumBitsForInstructions/bits, Remainder2/binary>> ->
            TargetIPaddrTuple = ch_utils:ipv4_addr_to_tuple(TargetIPaddr),
            ?PACKET_TRACE(Instructions, "~n  Unpacked the following instructions "
                                        "for target IP addr=~p, port=~p:", [TargetIPaddrTuple, TargetPort]),
            ?DEBUG_TRACE("Unpacked ~p instruction words for target IP addr=~p, port=~p:", [NumInstructions, TargetIPaddrTuple, TargetPort]),
            target_request_accumulator([{TargetIPaddr, TargetPort, Instructions} | TargetRequestList], Remainder2);
        _ ->
            throw({malformed, {'bad match on target request body', length(TargetRequestList)}})
    end;

target_request_accumulator(TargetRequestList, <<>>) ->
    lists:reverse(TargetRequestList);

target_request_accumulator(TargetRequestList, _Else) ->
    throw({malformed, {'bad match on target request header', length(TargetRequestList)}}).


%% Sends each target request to the relevant device client
%% @spec dispatch_to_device_clients(TargetRequestList)) -> TargetResponseOrder
%%       TargetRequestList = [TargetRequest, ..]
%%       TargetRequest = {IPaddrU32::integer(), PortU16::integer(), Instructions::binary()}
%%       TargetResponseOrder = [ {IPaddrU32::integer(), PortU16::integer()}, .. ]
dispatch_to_device_clients(TargetRequestList) ->
    lists:foreach(fun dispatch_to_device_client/1, TargetRequestList),
    lists:map(fun({IPaddrU32, PortU16, _Instructions}) -> {IPaddrU32, PortU16} end, TargetRequestList).  

dispatch_to_device_client({IPaddrU32, PortU16, Instructions}) ->
    ch_device_client:enqueue_requests(IPaddrU32, PortU16, Instructions).


gather_device_client_responses(TargetResponseOrder) ->
    device_client_response_accumulator(TargetResponseOrder, []).


device_client_response_accumulator([{IPaddrU32, PortU16}| Tail], ResponsesList) ->
    ResponseBin = receive
                      {device_client_response, IPaddrU32, PortU16, ErrorCode, TargetResponseBin} ->
                          ?DEBUG_TRACE("Received device client response from target IPaddr=~p,"
                                       "Port=~p", [ch_utils:ipv4_addr_to_tuple(IPaddrU32), PortU16]),
                          <<IPaddrU32:32, ErrorCode:16, PortU16:16, TargetResponseBin/binary>>
                  after (?DEVICE_CLIENT_UDP_TIMEOUT * 3) -> % Give it 3 times as long as whatever device client timeout we have
                      ?DEBUG_TRACE("Timout whilst awaiting response from device client for target IPaddr=~p,"
                                   "Port=~p", [ch_utils:ipv4_addr_to_tuple(IPaddrU32), PortU16]),            
                          <<IPaddrU32:32, ?ERRCODE_CH_DEVICE_CLIENT_TIMEOUT:16, PortU16:16>>
                  end,
    device_client_response_accumulator(Tail, [ResponseBin | ResponsesList]);

device_client_response_accumulator([], ResponsesList) ->
    ?DEBUG_TRACE("Received all expected device client responses"),
    lists:reverse(ResponsesList),
    list_to_binary(ResponsesList).


reply_to_client(ClientSocket, FullResponseBin) ->
    ?DEBUG_TRACE("Sending response to TCP client"),
    ?PACKET_TRACE(FullResponseBin, "~n  Sending the following response to the TCP client:"),
    gen_tcp:send(ClientSocket, FullResponseBin),
    packet_stats:client_response_sent().
