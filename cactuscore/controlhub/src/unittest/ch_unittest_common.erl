%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since May 2012
%%%
%%% @doc Common bits and pieces of code needed for the unit-testing.
%%% @end
%%% ===========================================================================
-module(ch_unittest_common).


%% Exported Functions
-export([spawn_device_emulator/2, device_emulator_init/2, dummy_request_data_generator/1, 
         udp_client_loop/7, tcp_client_loop/5, start_udp_echo_server/1, start_tcp_echo_server/2]).



udp_client_loop(_Socket, _TargetIP, _TargetPort, _Request, _Reply, {0,_MaxInFlight}, 0) ->
    ok;
udp_client_loop(Socket, TargetIP, TargetPort, Request, Reply, {NrInFlight,MaxInFlight}, NrIterationsLeft) when NrIterationsLeft>0, NrInFlight<MaxInFlight ->
    gen_udp:send(Socket, TargetIP, TargetPort, Request),
    receive
        {udp, Socket, TargetIP, TargetPort, Reply} ->
            udp_client_loop(Socket, TargetIP, TargetPort, Request, Reply, {NrInFlight, MaxInFlight}, NrIterationsLeft-1);
        {udp, Socket, TargetIP, TargetPort, Pkt} ->
            io:format("ERROR : Expected ~w , but received ~w~n", [Reply, Pkt]),
            fail
    after 0 ->
        udp_client_loop(Socket, TargetIP, TargetPort, Request, Reply, {NrInFlight+1,MaxInFlight}, NrIterationsLeft-1)
    end;
udp_client_loop(Socket, TargetIP, TargetPort, Request, Reply, {NrInFlight,MaxInFlight}, NrIterationsLeft) ->
    receive
        {udp, Socket, TargetIP, TargetPort, Reply} ->
            udp_client_loop(Socket, TargetIP, TargetPort, Request, Reply, {NrInFlight-1, MaxInFlight}, NrIterationsLeft);
        {udp, Socket, TargetIP, TargetPort, Pkt} ->
            io:format("ERROR : Expected ~w , but received ~w~n", [Reply, Pkt]),
            fail
    after 20 ->
        io:format("ERROR : Did not receive reply packet in 20ms~n"),
        fail
    end.

%    case gen_udp:recv(Socket, 0, 20) of
%        {ok, {TargetIP, TargetPort, Pkt}} ->
%           udp_client_loop(Socket, TargetIP, TargetPort, Request, Reply, {NrInFlight-1, MaxInFlight}, NrIterationsLeft);
%        {error, timeout} ->
%           io:format("ERROR : Did not receive reply packet in 20ms, with ~w in-flight and ~w iterations left~n", [NrInFlight,NrIterationsLeft]),
%           fail
%    end;


tcp_client_loop(_SocketTuple, _Request, _Reply, {0,_MaxInFlight}, 0) ->
    ok;
tcp_client_loop({Socket,ActiveVal}, Request, Reply, {NrInFlight,MaxInFlight}, NrItnsLeft) when NrItnsLeft>0, NrInFlight<MaxInFlight ->
    SendBytes = byte_size(Request),
    gen_tcp:send(Socket, <<SendBytes:32, Request/binary>>),
    case tcp_recv(Socket, ActiveVal, 0) of
        Pkt when is_binary(Pkt) ->
            tcp_client_loop({Socket,ActiveVal}, Request, Reply, {NrInFlight, MaxInFlight}, NrItnsLeft-1);
        timeout ->
            tcp_client_loop({Socket,ActiveVal}, Request, Reply, {NrInFlight+1, MaxInFlight}, NrItnsLeft-1)
    end;
tcp_client_loop({Socket, ActiveVal}, Request, Reply, {NrInFlight,MaxInFlight}, NrItnsLeft) ->
    case tcp_recv(Socket, ActiveVal, 20) of
        Pkt when is_binary(Pkt) ->
            tcp_client_loop({Socket,ActiveVal}, Request, Reply, {NrInFlight-1, MaxInFlight}, NrItnsLeft);
        timeout ->
            io:format("ERROR : Did not receive reply packet in 20ms~n"),
            fail
    end.


start_udp_echo_server(SocketOptions) ->
    {ok, Socket} =  gen_udp:open(0, SocketOptions),
    {ok, Port} = inet:port(Socket),
    io:format("~n UDP echo server is ready -- on port ~w~n", [Port]),
    udp_echo_server_loop(Socket).


udp_echo_server_loop(Socket) ->
    receive
        {udp, Socket, IP, Port, Packet} ->
            gen_udp:send(Socket, IP, Port, Packet)
    end,
    udp_echo_server_loop(Socket).

   
start_tcp_echo_server(SocketOptions, Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, SocketOptions),
    io:format("~n TCP echo server listening on port ~w~n", [Port]),
    tcp_echo_server_loop(LSocket, none).

tcp_echo_server_loop(LSocket, none) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    io:format("New client has connected!~n"),
    {ok, [active, ActiveVal]} = inet:getopts(Socket, [active]),
    tcp_echo_server_loop(LSocket, {Socket, ActiveVal});
tcp_echo_server_loop(LSocket, {Socket, ActiveVal}) ->
    case tcp_recv(Socket, ActiveVal, infinity) of 
        Pkt when is_binary(Pkt) ->
            SendBytes = byte_size(Pkt),
            gen_tcp:send(Socket, <<SendBytes:32, Pkt/binary>>),
            tcp_echo_server_loop(LSocket, Socket);
        tcp_closed ->
            io:format("Client disconnected. Going back to waiting for next client~n"),
            tcp_echo_server_loop(LSocket, none)
    end.


tcp_recv(Socket, false, Timeout) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, Pkt} ->
            Pkt;
        {error, timeout} ->
            timeout;
        {tcp_closed, Socket} ->
            tcp_closed
    end;
tcp_recv(Socket, once, Timeout) ->
    receive
        {tcp, Socket, Pkt} ->
            inet:setopts(Socket, [{active,once}]),
            Pkt;
        {tcp_closed, Socket} ->
            tcp_closed
    after Timeout ->
        timeout
    end;
tcp_recv(Socket, true, Timeout) ->
    receive
        {tcp, Socket, Pkt} ->
            Pkt;
        {tcp_closed, Socket} ->
            tcp_closed
    after Timeout ->
        timeout
    end.


%%% ---------------------------------------------------------------------------
%%% API Functions
%%% ---------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
%% @doc Spawns simple dummy hardware that checks the packet id is correct for 
%%      IPbus 2.0 packets (no check on header otherwise), and then just returns
%%      whatever it gets sent. To shut it down normally simply send it the atom 
%%      "shutdown" (no quotes, obviously). Note that this does not use 
%%      spawn_link - you have to shut down the server with the "shutdown" message.
%%
%% @spec spawn_device_emulator(Ver::integer(), PortU16::integer()) -> pid()
%% @end
%% ----------------------------------------------------------------------------
spawn_device_emulator(Ver, PortU16) ->
    spawn(?MODULE, device_emulator_init, [Ver, PortU16]).


%% The start-point for the dummy hardware
device_emulator_init(Ver, PortU16) ->
    {ok, Socket} = gen_udp:open(PortU16, [binary]),
    case Ver of
        {1, _} ->
            device_emulator_loop(Ver, Socket);
        {2, 0} ->
            device_emulator_loop(Ver, {Socket, 7645, none})
    end.


%% ----------------------------------------------------------------------------
%% @doc Returns a binary of random data made up of N 32-bit words, where N is
%%      a random integer value between 1 and MaxRequestLength (inclusive).
%%
%% @spec dummy_request_data_generator(MaxRequestLength::integer()) -> binary()
%% @end
%% ----------------------------------------------------------------------------
dummy_request_data_generator(MaxRequestLength) ->
    RequestLengthInBytes = random:uniform(MaxRequestLength) * 4,
    % slightly hacky way to produce random request data.
    list_to_binary([random:uniform(256)-1 || _X <- lists:seq(1, RequestLengthInBytes)]).


%%% ---------------------------------------------------------------------------
%%% Local Functions
%%% ---------------------------------------------------------------------------


%% The receive loop implementation for the hardware emulator
device_emulator_loop({1, _} = Ver, Socket) ->
    receive
        {udp, Socket, IP, Port, Packet} ->
            gen_udp:send(Socket, IP, Port, Packet),
            device_emulator_loop(Ver, Socket);
        shutdown -> 
            ok % For a receiving a clean/normal exit message.
    end;

device_emulator_loop({2, 0} = Ver, {Socket, NextId, LastResponse} = State) ->
    receive
        {udp, Socket, IP, Port, <<Hdr:4/binary, _/binary>> = Packet} ->
            case Hdr of
                % Control packet, correct non-zero ID
                <<16#20:8, NextId:16, 16#f0>> ->
                    gen_udp:send(Socket, IP, Port, Packet),
                    device_emulator_loop(Ver, {Socket, ch_device_client:increment_pkt_id(NextId), Packet});
                % Control packet, ID = 0
                <<16#20:8, 0:16, 16#f0>> ->
                    gen_udp:send(Socket, IP, Port, Packet),
                    device_emulator_loop(Ver, {Socket, NextId, Packet});
                % Status packet
                <<2:4, 0:20, 16#f1:8, 0:(15*32)>> ->
                    StatusResponse = <<2:4, 0:20, 16#f1:8,          % Header
                                       0:32,                        % MTU
                                       1:32,                        % nResponseBuffers
                                       16#20:8, NextId:16, 16#f0:8, % Next expected control header
                                       0:(32*12)>>,
                    gen_udp:send(Socket, IP, Port, StatusResponse),
                    device_emulator_loop(Ver, State);
                % Re-send request
                <<16#20:8, Id:16, 16#f2:8>> ->
                    case LastResponse of 
                        <<16#20:8, Id:16, 16#f0:8, _/binary>> ->
                            gen_udp:send(Socket, IP, Port, LastResponse);
                        _ ->
                            void
                    end,
                    device_emulator_loop(Ver, State)
            end
     end.
    
