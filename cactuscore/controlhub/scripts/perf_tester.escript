#!/bin/env escript
%% -*- erlang -*-
%%! -C -pa /cactusbuild/trunk/cactuscore/controlhub/ebin/unittest +zdbbl 2097151 

% +sbt db +scl false +swt very_low



% ---------------------------------------------------------------------------------------
%    MACROS / RECORDS
% ---------------------------------------------------------------------------------------

-define(UDP_SOCKET_OPTIONS, [binary, {active, true}, {buffer, 100000}, {recbuf, 100000}, {read_packets, 50}]). % {active, false}
%-define(UDP_SOCKET_OPTIONS, [binary, {active, true}]).

-define(TCP_SOCKET_OPTIONS, [binary, {packet, 4}, {nodelay, true}, {active, true}, {backlog, 256}, {buffer, 200000}, {low_watermark, 200000}, {high_watermark, 400000}, {recbuf, 200000}, {sndbuf, 200000}]).


-define(IPBUS_TXFULL_REQ, <<16#200000f0:32,
                            16#2cbaff1f:32, % Write, 0xff deep
                            (16#1000):32,
                            0:((16#ff)*32),
                            16#20006c1f:32, % Write, 0x66 deep
                            (16#1000):32,
                            0:((16#6c)*32)
                          >>).
-define(IPBUS_TXFULL_REP, <<16#200000f0:32,
                            16#2cbaff10:32, % Write, 0xff deep
                            16#20006c10:32  % Write, 0x66 deep
                          >>).
 

-define(IPBUS_BOTHFULL_REQ, <<16#200000f0:32,
                              16#2cbaff1f:32, % Write, 0xff deep
                              16#1000:32,
                              0:((16#ff)*32),
                              16#2abcff0f:32, % Read, 0xff deep
                              16#1000:32,
                              16#2def561f:32, % Write, 0x56 deep
                              16#1000:32,
                              0:((16#56)*32),
                              16#2fed5a0f:32, % Read, 0x5a deep
                              16#1000:32
                            >>).
-define(IPBUS_BOTHFULL_REP, <<16#200000f0:32,
                              16#2cbaff10:32, % Write, 0xff deep
                              16#2abcff00:32,  % Read, 0xff deep
                              0:((16#ff)*32),
                              16#2def5610:32, % Write, 0x56 deep
                              16#2fed5a00:32, % Read, 0x5a deep
                              0:((16#5a)*32)
                            >>).


-define(IPBUS_TXFULL_PRAM_REQ, <<16#200000f0:32,
                                 16#2000011f:32, % 1-word write for PRAM 
                                 16#00002000:32,
                                 16#00000000:32,
                                 16#2000ff3f:32, % NI write, 0xff deep
                                 16#00002001:32,
                                 0:(32*(16#ff)),
                                 16#20005c3f:32, % NI write, 0x5c deep
                                 16#00002001:32,
                                  0:((16#5c)*32)
                               >>).
-define(IPBUS_TXFULL_PRAM_REP, <<16#200000f0:32,
                                 16#20000110:32,
                                 16#2000ff30:32,
                                 16#20005c30:32 % 16#20005c30:32
                               >>).


-define(IPBUS_RXFULL_PRAM_REQ, <<16#200000f0:32,
                                 16#2000011f:32,
                                 16#00002000:32,
                                 16#00000000:32,
                                 16#2000ff2f:32,
                                 16#00002001:32,
                                 16#20005c2f:32,
                                 16#00002001:32
                               >>).
-define(IPBUS_RXFULL_PRAM_REP, <<16#200000f0:32,
                                 16#20000110:32,
                                 16#2000ff20:32,
                                 0:(32*(16#ff)),
                                 16#20005c20:32,
                                 0:(32*(16#5c))
                               >>).


-define(IPBUS_BOTHFULL_PRAM_REQ, <<16#200000f0:32,
                                   16#2000011f:32, % 1-word write for PRAM 
                                   16#00002000:32,
                                   16#00000000:32,
                                   16#2000ff3f:32, % NI write, 0xff deep
                                   16#00002001:32,
                                   0:(32*(16#ff)),
                                   16#2000553f:32, % NI write, 0x58 deep
                                   16#00002001:32,
                                   0:((16#55)*32),
                                   16#2000011f:32, % 1-word write for PRAM
                                   16#00002000:32,
                                   16#00000000:32,
                                   16#2000ff2f:32, % NI read, 0xff deep
                                   16#20002001:32,
                                   16#20005c2f:32, % NI read, 0x62 deep
                                   16#00002001:32
                                 >>).
-define(IPBUS_BOTHFULL_PRAM_REP, <<16#200000f0:32,
                                   16#20000110:32,
                                   16#2000ff30:32,
                                   16#20005530:32,
                                   16#20000110:32,
                                   16#2000ff20:32,
                                   0:((16#ff)*32),
                                   16#20005c20:32,
                                   0:((16#5c)*32)
                                 >>).



-define(DUMMY_JUMBO, <<0:(8*8000)>>).

% ---------------------------------------------------------------------------------------
%    MAIN, USAGE & OPTION PARSING
% ---------------------------------------------------------------------------------------

usage() ->
    io:format("usage: scripts/perf_tester.escript <runmode> <IP> <port> <nr_iterations> <nr_in_flight>~n~n").


parse_args([ArgIP, ArgPort, ArgItns, ArgInFlight]) ->
    {ok, IP} = inet:getaddr(ArgIP, inet),
    Port = list_to_integer(ArgPort),
    Itns = list_to_integer(ArgItns),
    NrInFlight = list_to_integer(ArgInFlight),
    io:format("Arguments: ~n"
              "   Target              @ ~w:~w~n"
              "   Number of iterations: ~w~n"
              "   Max in flight:        ~w~n",
              [IP, Port, Itns, NrInFlight]),
    {IP, Port, Itns, NrInFlight};
parse_args(_) ->
    io:format("Wrong number of arguments given!"),
    usage(),
    halt(1).


create_udp_socket() ->
    io:format("Creating UDP socket with options: ~w ~n", [?UDP_SOCKET_OPTIONS]),
    {ok, Socket} = gen_udp:open(0, ?UDP_SOCKET_OPTIONS),
    {ok, OptValues} =  inet:getopts(Socket, [active, buffer, delay_send, read_packets, recbuf, sndbuf]),
    io:format("Socket opened! Option values are: ~w~n", [OptValues]),
    Socket.


tcp_connect(TargetIP, TargetPort) ->
    SocketOptions = lists:keydelete(backlog, 1, ?TCP_SOCKET_OPTIONS),
    io:format("Connecting TCP stream to ~w:~w with options: ~w ~n", [TargetIP, TargetPort, SocketOptions]),
    {ok, Socket} = gen_tcp:connect(TargetIP, TargetPort, SocketOptions),
    io:format("Socket opened! Option values are: ~w~n",            
              [inet:getopts(Socket, [active, buffer, delay_send, read_packets, recbuf, sndbuf, low_watermark, high_watermark])]
             ),
    Socket.


print_results(NrItns, MicroSecs, ReqBytes, ReplyBytes) ->
    BytesSent = NrItns * ReqBytes,
    BytesRecd = NrItns * ReplyBytes,
    MicroSecsPerIt = MicroSecs / NrItns,
    MbitPerSecSent = (BytesSent * 8 / 1000000.0) / (MicroSecs / 1000000.0),
    MbitPerSecRecd = (BytesRecd * 8 / 1000000.0) / (MicroSecs / 1000000.0),
    io:format("~nSent/rcvd ~w packets (~w/~w bytes) in ~w secs, time per packet = ~wus~n", [NrItns, ReqBytes, ReplyBytes, MicroSecs/1000000.0, MicroSecsPerIt]),
    io:format("   Send B/W = ~w Mb/s~n", [MbitPerSecSent]),
    io:format("   Recv B/W = ~w Mb/s~n", [MbitPerSecRecd]),
    io:format("Test iteration frequency = ~w Hz~n", [1000000.0 * NrItns / MicroSecs]),
    io:format("Average rw bandwidth = ~w KB/s~n", [max(MbitPerSecSent,MbitPerSecRecd) * 125.0]).


main(["udp_ipbus_client" | OtherArgs]) ->
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    Socket = create_udp_socket(),
    Request = ?IPBUS_TXFULL_REQ,
    Reply = ?IPBUS_TXFULL_REP,
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:udp_client_loop(Socket, TargetIP, TargetPort, Request, Reply, {0,NrInFlight}, NrItns) end),
    print_results(NrItns, MicroSecs, byte_size(Request), byte_size(Reply));

main(["udp_ipbus_client2" | OtherArgs]) ->
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    Socket = create_udp_socket(),
    Request = ?IPBUS_BOTHFULL_REQ,
    Reply = ?IPBUS_BOTHFULL_REP,
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:udp_client_loop(Socket, TargetIP, TargetPort, Request, Reply, {0,NrInFlight}, NrItns) end),
    print_results(NrItns, MicroSecs, byte_size(Request), byte_size(Reply));

main(["udp_ipbus_client_pramtx" | OtherArgs]) ->
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    Socket = create_udp_socket(),
    Request = ?IPBUS_TXFULL_PRAM_REQ,
    Reply = ?IPBUS_TXFULL_PRAM_REP,
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:udp_client_loop(Socket, TargetIP, TargetPort, Request, Reply, {0,NrInFlight}, NrItns) end),
    print_results(NrItns, MicroSecs, byte_size(Request), byte_size(Reply));

main(["udp_ipbus_client_pramrx" | OtherArgs]) ->
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    Socket = create_udp_socket(),
    Request = ?IPBUS_RXFULL_PRAM_REQ,
    Reply = ?IPBUS_RXFULL_PRAM_REP,
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:udp_client_loop(Socket, TargetIP, TargetPort, Request, Reply, {0,NrInFlight}, NrItns) end),
    print_results(NrItns, MicroSecs, byte_size(Request), byte_size(Reply));

main(["udp_echo_client" | OtherArgs]) ->
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    Socket = create_udp_socket(),
    Request = ?IPBUS_TXFULL_PRAM_REQ,
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:udp_client_loop(Socket, TargetIP, TargetPort, Request, Request, {0,NrInFlight}, NrItns) end),
    print_results(NrItns, MicroSecs, byte_size(Request), byte_size(Request));

main(["udp_echo_server"]) ->
    io:format("Starting echo server with options: ~p~n", [?UDP_SOCKET_OPTIONS]),
    ch_unittest_common:start_udp_echo_server(?UDP_SOCKET_OPTIONS);

main(["tcp_echo_client" | OtherArgs]) ->
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    Socket = tcp_connect(TargetIP, TargetPort),
%    Request = <<0:(8*1430)>>,
    Request = <<0:(8*5*1430)>>,
    {ok, [{active, ActiveValue}]} = inet:getopts(Socket, [active]),
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:tcp_client_loop({Socket, ActiveValue}, Request, Request, {0,NrInFlight}, NrItns) end ),
    gen_tcp:close(Socket),
    print_results(NrItns, MicroSecs, byte_size(Request), byte_size(Request));

main(["tcp_echo_server", ArgPort]) ->
    Port = list_to_integer(ArgPort),
    io:format("Starting TCP echo server with options: ~p~n", [?TCP_SOCKET_OPTIONS]),
    ch_unittest_common:start_tcp_echo_server(?TCP_SOCKET_OPTIONS, Port);
    
main(["tcp_ch_client", ArgControlHubIP | OtherArgs]) ->
    {ok, ControlHubIP} = inet:getaddr(ArgControlHubIP, inet),
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    Socket = tcp_connect(ControlHubIP, 10203),
    TargetIPU32 = (element(1,TargetIP) bsl 24) + (element(2,TargetIP) bsl 16)
                    + (element(3,TargetIP) bsl 8) + element(4,TargetIP),
    Request = <<TargetIPU32:32,
                TargetPort:16, (byte_size(?IPBUS_RXFULL_PRAM_REQ) div 4):16,
                (?IPBUS_RXFULL_PRAM_REQ)/binary
              >>,
    Reply = <<(byte_size(?IPBUS_RXFULL_PRAM_REP)+8):32 ,
              TargetIPU32:32,
              TargetPort:16, 0:16,
              (?IPBUS_RXFULL_PRAM_REP)/binary>>,
    io:format("Request: ~w bytes~nReply: ~w bytes~n", [byte_size(Request), byte_size(Reply)]),
    {ok, [{active, ActiveValue}]} = inet:getopts(Socket, [active]),
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:tcp_client_loop({Socket, ActiveValue}, Request, Reply, {0,NrInFlight}, NrItns) end ),
    gen_tcp:close(Socket),
    print_results(NrItns, MicroSecs, byte_size(Request), byte_size(Reply));


main(["tcp_ch_client2", ArgControlHubIP | OtherArgs]) ->
    {ok, ControlHubIP} = inet:getaddr(ArgControlHubIP, inet),
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    Socket = tcp_connect(ControlHubIP, 10203),
    TargetIPU32 = (element(1,TargetIP) bsl 24) + (element(2,TargetIP) bsl 16)
                    + (element(3,TargetIP) bsl 8) + element(4,TargetIP),
    PartialReq = <<TargetIPU32:32,
                   TargetPort:16, (byte_size(?IPBUS_RXFULL_PRAM_REQ) div 4):16,
                   (?IPBUS_RXFULL_PRAM_REQ)/binary
                 >>,
    PartialReply = <<(byte_size(?IPBUS_RXFULL_PRAM_REP)+8):32 ,
                     TargetIPU32:32,
                     TargetPort:16, 0:16,
                     (?IPBUS_RXFULL_PRAM_REP)/binary
                   >>,
    Request = <<PartialReq/binary,
                PartialReq/binary,
                PartialReq/binary,
                PartialReq/binary,
                PartialReq/binary, PartialReq/binary,
                PartialReq/binary, PartialReq/binary,
                PartialReq/binary, PartialReq/binary%,
%                % 10 above here
%                PartialReq/binary, PartialReq/binary,
%                PartialReq/binary, PartialReq/binary,
%                PartialReq/binary,
%                % 15 above here
%                PartialReq/binary, PartialReq/binary,
%                PartialReq/binary, PartialReq/binary,
%                PartialReq/binary
              >>,
    Reply = <<PartialReply/binary,
              PartialReply/binary,
              PartialReply/binary,
              PartialReply/binary,
              PartialReply/binary, PartialReply/binary,
              PartialReply/binary, PartialReply/binary,
              PartialReply/binary, PartialReply/binary%,
              % 10 above here
%              PartialReply/binary, PartialReply/binary,
%              PartialReply/binary, PartialReply/binary,
%              PartialReply/binary,
%              % 15 above here
%              PartialReply/binary, PartialReply/binary,
%              PartialReply/binary, PartialReply/binary,
%              PartialReply/binary
            >>,
    io:format("Request: ~w bytes~nReply: ~w bytes~n", [byte_size(Request), byte_size(Reply)]),
    {ok, [{active, ActiveValue}]} = inet:getopts(Socket, [active]),
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:tcp_client_loop({Socket, ActiveValue}, Request, Reply, {0,NrInFlight}, NrItns) end ),
    gen_tcp:close(Socket),
    print_results(NrItns, MicroSecs, byte_size(Request), byte_size(Reply));



main(["device_client_bw" | OtherArgs]) ->
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    TargetIPU32 = (element(1,TargetIP) bsl 24) + (element(2,TargetIP) bsl 16)
                    + (element(3,TargetIP) bsl 8) + element(4,TargetIP),
    RequestBin = ?IPBUS_TXFULL_REQ,
    RequestMsg = {send, RequestBin, self()},
    ReplyBin = ?IPBUS_TXFULL_REP,
    ReplyMsg = {device_client_response, TargetIPU32, TargetPort, 0, ReplyBin},
    % 
    application:set_env(controlhub, max_in_flight, 16),
    io:format("~ncontrolhub max_in_flight is ~w~n", [application:get_env(controlhub, max_in_flight)]),
    ch_stats:start_link(),
    ch_device_client_registry:start_link(),
    {ok, DeviceClientPid} = ch_device_client_registry:get_pid(TargetIPU32, TargetPort),
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:gencast_msg_client_loop(DeviceClientPid, RequestMsg, ReplyMsg, {0,NrInFlight}, NrItns) end ),
    ch_stats:stop(),
    ch_device_client_registry:stop(),
    print_results(NrItns, MicroSecs, byte_size(RequestBin), byte_size(ReplyBin));

main(["dummy_hw", ArgPort]) ->
    Port = list_to_integer(ArgPort),
    io:format("~nStarting dummy hardware on port ~w~n", [Port]),
    ch_unittest_common:device_emulator_init({2,0}, Port);

main(["trans_manager_server"]) ->
    TargetIPU32 = 16#0,
    TargetPort = 1,
    % Setup dummy device client & device_client_registry ets table
    DevClientPid = spawn_link(ch_unittest_common, dummy_device_client_loop, [TargetIPU32, TargetPort]),
    DevClientTbl = ets:new(device_client_index, [named_table, protected, {read_concurrency, true}]),
    ets:insert(DevClientTbl, {{TargetIPU32,TargetPort}, DevClientPid}),
    % Start TCP listener
    ch_stats:start_link(),
    ch_tcp_listener:start_link(),
    io:format("~n ControlHub TCP listener ready on port 10203 as usual~n"),
    timer:sleep(infinity);

main(_) ->
    io:format("Incorrect usage!~n"),
    usage().


