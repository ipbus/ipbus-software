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
            end,
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
            ?DEBUG_TRACE("Received a request from client.")
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
            tcp_receive_handler(ClientSocket)
    end.


%% Top-level function for actually dealing with the client request and returning a response.
process_request(ClientSocket, RequestBin) ->
    try unpack_target_requests(RequestBin) of
       TargetRequestList ->
           TargetResponseOrder = dispatch_to_device_clients(TargetRequestList),
           CompleteResponse = gather_device_client_responses(TargetResponseOrder),
           reply_to_client(CompleteResponse)
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
            throw({malformed, {'bad match on target request body', length(TargetRequestList)}}).
    end.

target_request_accumulator(TargetRequestList, <<>>) ->
    lists:reverse(TargetRequestList);

target_request_accumulator(TargetRequestList, _Else) ->
    throw({malformed, {'bad match on target request header', length(TargetRequestList)}}).


%% Sends each target request to the relevant device client
%% @spec dispatch_to_device_clients(TargetRequestList)) -> TargetResponseOrder
%%       TargetRequestList = [TargetRequest, ..]
%%       TargetRequest = {IPaddrU32::integer(), PortU16::integer(), Instructions::binary()}
%%       TargetResponseOrder = [ {IPaddrU32::integer(), PortU16::integer()}, .. ]
dispatch_to_device_clients(TargetRequest) ->
    lists:foreach(dispatch_to_device_client/1, TargetRequestList),
    map(fun({IPaddrU32, PortU16, Instructions}) -> {IPaddrU32, PortU16} end, TargetRequestList).  

dispatch_to_device_client({IPaddrU32, PortU16, Instructions}) ->
    ch_device_client:enqueue_requests(IPaddrU32, PortU16, Instructions).


%% *** I AM HERE - just need to get the responses according to the TargetResponseOrder list ***


%%  ***** OLD CODE ****

    TargetSections = unpack_target_sections({malformed(size(RequestBin) rem 4), ClientSocket, RequestBin)
    dispatch_target_sections_to_device_clients(TargetSections)
    gather_device_client_responses()
    send_response_to_client()
    
    RequestSizeInBytes = size(RequestBin)
    case RequestSizeInBytes rem 4 of
        0 -> process_request_by_section(ClientSocket, RequestBin, []);
        _ -> ch_stats:client_request_malformed()
    process_request_section_loop(ClientSocket, RequestBin).


%% Attempt unpack and processes of each request subsection 
process_request_section_loop(ClientSocket, RequestBin)


%% Quickly examine the packet; if ok then we proceed with unpack.
check_and_process_packet(ClientSocket, RequestBin) ->
  case basic_packet_check(RequestBin) of
      ok ->
          unpack_packet(ClientSocket, RequestBin);
      bad_packet ->
          ?DEBUG_TRACE("WARNING! Received and ignoring malformed packet."),
          ?PACKET_TRACE(RequestBin, "WARNING!~n  Received and ignoring this malformed packet - did not pass basic packet checks:"),
          ch_stats:client_request_malformed(),
          bad_packet_logged
  end.



%% Performs some very basic checks on the packet, such as minimum size check
%% and does it contain an integer number of 32-bit words.
%% @spec basic_packet_check(RequestBin) -> ok | bad_packet
basic_packet_check(RequestBin) ->
    SizeInBytes = size(RequestBin),  % Size of packet in bytes 
    % Check if the packet is an integer number of 32-bit words or not
    case SizeInBytes rem 4 of
        0 -> case SizeInBytes div 4 of
                 Size32 when Size32 >= 3 -> ok;
                 _ -> bad_packet  % Size not three or more 32-bit words in size => rubbish.
             end;
        _ -> bad_packet  % Packet does not consist of an integer number of 32-bit words
    end.

%% Unpack the header: Target IP address (32-bit),
%%                    Num IPbus instruction words (16-bit),
%%                    Target Port(16-bit) 
unpack_packet(ClientSocket, <<TargetIPaddr:32,
                                   NumInstructions:16,
                                   TargetPort:16,
                                   Remainder/binary>>) ->
    NumBitsForInstructions = 32 * NumInstructions,
    case Remainder of
        <<Instructions:NumBitsForInstructions/bits, Remainder2/binary>> ->
            TargetIPaddrTuple = ch_utils:ipv4_addr_to_tuple(TargetIPaddr),
            ?PACKET_TRACE(Instructions, "~n  Unpacked the following instructions for target IP addr=~p, port=~p:", [TargetIPaddrTuple, TargetPort]),
            ?DEBUG_TRACE("Unpacked ~p instruction words for target IP addr=~p, port=~p:", [NumInstructions, TargetIPaddrTuple, TargetPort]),
            process_packet(DeviceIDs, Instructions),
            ?DEBUG_TRACE("Finished distributing instructions to devices; now awaiting responses..."),
            process_responses(ClientSocket, NumDevices),
            % TODO: If Remainder2 is not of zero size, then we need to treat it as a "new" packet...
            if 
                size(Remainder2) /= 0 -> 
                    check_and_process_packet(ClientSocket, Remainder2);
                true -> ok
            end;
        _ ->
            ?DEBUG_TRACE("WARNING! Received and ignoring malformed packet."),
            ?PACKET_TRACE(RequestBin, "WARNING!~n  Received and ignoring this malformed packet - did not pass Redwood packet checks:"),
            ch_stats:client_request_malformed(),
            bad_packet_logged
    end.


process_packet(DeviceIDs, Instructions) ->
    DeviceIDList = get_device_IDs_as_list(DeviceIDs),
    send_instructions_to_device_clients(DeviceIDList, Instructions).


get_device_IDs_as_list(DeviceIDs) ->
    get_device_IDs_as_list([], DeviceIDs).

get_device_IDs_as_list(ResultList, <<DeviceID:32, Remainder/binary>>) ->
    get_device_IDs_as_list([DeviceID | ResultList], Remainder);

get_device_IDs_as_list(ResultList, <<>>) ->
    lists:reverse(ResultList), %% Not really necessary... remove in the future for sake of speed.
    ResultList.


send_instructions_to_device_clients([DeviceID | RemainingDeviceIDs], Instructions) ->
    ?DEBUG_TRACE("Sending instructions to the device client for DeviceID = ~p", [DeviceID]),
    device_client:send(DeviceID, Instructions),
    send_instructions_to_device_clients(RemainingDeviceIDs, Instructions);

send_instructions_to_device_clients([], _Instructions) -> ok.


process_responses(ClientSocket, NumDevices) ->
    ResponsesList = get_hardware_responses(NumDevices, []),
    % TODO: check we have the responses from the devices we expect, and log any declared timeout responses. 
    send_responses_to_tcp_client(ClientSocket, ResponsesList).


get_hardware_responses(RemainingDevices, ResponsesList) when RemainingDevices > 0 ->
    receive
        {device_client_response, DeviceID, Status, Response} ->
            ?DEBUG_TRACE("Received response from the device client for DeviceID = ~p", [DeviceID]),            
            get_hardware_responses(RemainingDevices - 1, [{DeviceID, Status, Response}| ResponsesList]);
        _What ->
            io:format("Warning! Transaction Manager received unexpected response: ~p~n", [_What]),
            get_hardware_responses(RemainingDevices, ResponsesList)
    after (?DEVICE_CLIENT_UDP_TIMEOUT + 200) ->
        io:format("WARNING! Transaction Manager received only ~p of ~p expected responses!~n",
                  [length(ResponsesList), length(ResponsesList) + RemainingDevices]),
        % TODO: Log this error properly
        ResponsesList
    end;

get_hardware_responses(0, ResponsesList) -> 
    ResponsesList.
        

send_responses_to_tcp_client(ClientSocket, [{_DeviceID, ok, Response} | RemainingResponses]) ->
    ?DEBUG_TRACE("Sending response from DeviceID = ~p to Redwood.", [_DeviceID]),
    ?PACKET_TRACE(Response, "~n  Sending the following response packet to Redwood from DeviceID = ~p:", [_DeviceID]),
    gen_tcp:send(ClientSocket, Response),
    ch_stats:client_response_sent(),
    send_responses_to_tcp_client(ClientSocket, RemainingResponses);

send_responses_to_tcp_client(ClientSocket, [{_DeviceID, udp_response_timeout, Response} | RemainingResponses]) ->
    ?DEBUG_TRACE("Sending UDP timeout response from DeviceID = ~p to Redwood.", [_DeviceID]),
    ?PACKET_TRACE(Response, "~n  Sending the following UDP timeout response packet to Redwood from DeviceID = ~p:", [_DeviceID]),
    gen_tcp:send(ClientSocket, Response),
    ch_stats:client_response_sent(),
    send_responses_to_tcp_client(ClientSocket, RemainingResponses);

send_responses_to_tcp_client(_ClientSocket, []) -> ok.

