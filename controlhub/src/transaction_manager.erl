%% Author: rob
%% Created: Jan 13, 2011
%% Description: TODO: Add description to transaction_manager
-module(transaction_manager).

%%
%% Include files
%%
-include("ch_global.hrl").

%%
%% Exported Functions
%%
-export([start_transaction_manager_tcp_listen_pool/0, start_transaction_manager_tcp_listen_pool/2]).

%%
%% API Functions
%%

-define(TCP_OPTIONS, [binary, {packet, 4}, {reuseaddr, true}, {active, true}, {backlog, ?MAX_CONCURRENT_CLIENT_CONNECTIONS}]).

start_transaction_manager_tcp_listen_pool() ->
    start_transaction_manager_tcp_listen_pool(?MAX_CONCURRENT_CLIENT_CONNECTIONS, 10203).

start_transaction_manager_tcp_listen_pool(PoolSize, Port) when is_integer(PoolSize), PoolSize > 0,
                                                               is_integer(Port), Port > 0 ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, TcpListenSocket} -> create_pool(PoolSize, TcpListenSocket);
        {error, eaddrinuse} ->
           io:format("~n*****~nError starting Control Hub: port ~p is already in use!~n*****~n~n", [Port]),
           exit(eaddrinuse);
        {error, What} -> exit(What)
    end.




%%
%% Local Functions
%%

create_pool(PoolSize, TcpListenSocket) ->
    io:format("Attempting creation of pool of ~p Transaction Managers...~n", [PoolSize]),
    create_pool_recursive(PoolSize, TcpListenSocket).


create_pool_recursive(PoolServersToStart, TcpListenSocket) when PoolServersToStart > 0 ->
    spawn(fun() -> transaction_manager_listen(TcpListenSocket) end),
    create_pool_recursive(PoolServersToStart - 1, TcpListenSocket);

create_pool_recursive(PoolServersToStart, _TcpListenSocket) when PoolServersToStart =:= 0 ->
    io:format("Transaction Manger pool has been created successfully.~n", []).



transaction_manager_listen(TcpListenSocket) ->
    {ok, TcpAcceptedSocket} = gen_tcp:accept(TcpListenSocket),
    ?DEBUG_TRACE("TCP socket accepted!"),
    tcp_receive_handler(TcpAcceptedSocket),
    transaction_manager_listen(TcpListenSocket).


tcp_receive_handler(TcpAcceptedSocket) ->
    receive
        {tcp, TcpAcceptedSocket, PacketBinary} ->
            check_and_process_packet(TcpAcceptedSocket, PacketBinary),
            tcp_receive_handler(TcpAcceptedSocket);
        {tcp_closed, TcpAcceptedSocket} ->
            ?DEBUG_TRACE("TCP socket closed.");
        _Else ->
            ?DEBUG_TRACE("WARNING! Received and ignoring unexpected message: ~p", [_Else]),
            tcp_receive_handler(TcpAcceptedSocket)
    end.


check_and_process_packet(TcpAcceptedSocket, PacketBinary) ->
  ch_stats:client_request_in(),
  case basic_packet_check(PacketBinary) of
      ok ->
          unpack_packet(TcpAcceptedSocket, PacketBinary);
      bad_packet ->
          ?DEBUG_TRACE("WARNING! Received and ignoring malformed packet."),
          ?PACKET_TRACE(PacketBinary, "WARNING!~n  Received and ignoring this malformed packet - did not pass basic packet checks:"),
          ch_stats:client_request_malformed(),
          bad_packet_logged
  end.



%% Performs some very basic checks on the packet, such as minimum size check
%% and does it contain an integer number of 32-bit words.
%% @spec basic_packet_check(PacketBinary) -> ok | bad_packet
basic_packet_check(PacketBinary) ->
    SizeInBytes = size(PacketBinary),  % Size of packet in bytes 
    % Check if the packet is an integer number of 32-bit words or not
    case SizeInBytes rem 4 of
        0 -> case SizeInBytes div 4 of
                 Size32 when Size32 >= 3 -> ok;
                 _ -> bad_packet  % Size not greater than three 32-bit words 
             end;
        _ -> bad_packet  % Packet does not consist of an integer number of 32-bit words
    end.


unpack_packet(TcpAcceptedSocket, PacketBinary) ->
    %Process the packet
    <<NumInstructions:16, NumDevices:16, Remainder/binary>> = PacketBinary,
    NumBitsForDeviceIDs = 32 * NumDevices,
    NumBitsForInstructions = 32 * NumInstructions,
    
    case Remainder of
        <<DeviceIDs:NumBitsForDeviceIDs/bits, Instructions:NumBitsForInstructions/bits, Remainder2/binary>> ->
            ?PACKET_TRACE(PacketBinary, "~n  Received and unpacking the following valid Redwood packet:"),
            ?DEBUG_TRACE("Received Redwood packet has ~p instruction words for ~p devices.", [NumInstructions, NumDevices]),
            process_packet(DeviceIDs, Instructions),
            ?DEBUG_TRACE("Finished distributing instructions to devices; now awaiting responses..."),
            process_responses(TcpAcceptedSocket, NumDevices),
            % TODO: If Remainder2 is not of zero size, then we need to treat it as a "new" packet...
            if 
                size(Remainder2) /= 0 -> 
                    check_and_process_packet(TcpAcceptedSocket, Remainder2);
                true -> ok
            end;
        _ ->
            ?DEBUG_TRACE("WARNING! Received and ignoring malformed packet."),
            ?PACKET_TRACE(PacketBinary, "WARNING!~n  Received and ignoring this malformed packet - did not pass Redwood packet checks:"),
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


process_responses(TcpAcceptedSocket, NumDevices) ->
    ResponsesList = get_hardware_responses(NumDevices, []),
    % TODO: check we have the responses from the devices we expect, and log any declared timeout responses. 
    send_responses_to_tcp_client(TcpAcceptedSocket, ResponsesList).


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
        

send_responses_to_tcp_client(TcpAcceptedSocket, [{_DeviceID, ok, Response} | RemainingResponses]) ->
    ?DEBUG_TRACE("Sending response from DeviceID = ~p to Redwood.", [_DeviceID]),
    ?PACKET_TRACE(Response, "~n  Sending the following response packet to Redwood from DeviceID = ~p:", [_DeviceID]),
    gen_tcp:send(TcpAcceptedSocket, Response),
    ch_stats:client_response_sent(),
    send_responses_to_tcp_client(TcpAcceptedSocket, RemainingResponses);

send_responses_to_tcp_client(TcpAcceptedSocket, [{_DeviceID, udp_response_timeout, Response} | RemainingResponses]) ->
    ?DEBUG_TRACE("Sending UDP timeout response from DeviceID = ~p to Redwood.", [_DeviceID]),
    ?PACKET_TRACE(Response, "~n  Sending the following UDP timeout response packet to Redwood from DeviceID = ~p:", [_DeviceID]),
    gen_tcp:send(TcpAcceptedSocket, Response),
    ch_stats:client_response_sent(),
    send_responses_to_tcp_client(TcpAcceptedSocket, RemainingResponses);

send_responses_to_tcp_client(_TcpAcceptedSocket, []) -> ok.

