#!/bin/env escript
%% -*- erlang -*-
%%! -C -pa ebin/unittest +zdbbl 2097151



% ---------------------------------------------------------------------------------------
%    MACROS / RECORDS
% ---------------------------------------------------------------------------------------

-define(UDP_SOCKET_OPTIONS, [binary, {active, true}, {buffer, 100000}, {recbuf, 100000}, {read_packets, 50}]). % {active, false}
%-define(UDP_SOCKET_OPTIONS, [binary, {active, true}]).

-define(TCP_SOCKET_OPTIONS, [binary, {packet, 4}, {nodelay, true}, {active, true}, {backlog, 256}, {buffer, 100000}, {low_watermark, 50000}, {high_watermark, 100000}]).
%-define(TCP_SOCKET_OPTIONS, [binary, {packet, 4}, {nodelay, true}, {active, true}, {backlog, 256}]).


-define(IPBUS_TXFULL_REQ, <<16#200000f0:32,
                             16#2cbaff1f:32, % Write, 0xff deep
                             (16#1000):32,
                             0:((16#ff)*32),
                             16#2000601f:32, % Write, 0x60 deep
                             (16#1000):32,
                             0:((16#60)*32)
                           >>).
-define(IPBUS_TXFULL_REP, <<16#200000f0:32,
                             16#2cbaff10:32, % Write, 0xff deep
                             16#20006010:32  % Write, 0x60 deep
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

% ---------------------------------------------------------------------------------------
%    MAIN, USAGE & OPTION PARSING
% ---------------------------------------------------------------------------------------

usage() ->
    io:format("usage: scripts/perf_tester.escript <runmode> <IP> <port> <nr_iterations> <nr_in_flight>~n~n").


parse_args([ArgIP, ArgPort, ArgItns, ArgInFlight]) ->
    {ok, IP} = inet_parse:address(ArgIP),
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
    io:format("   Recv B/W = ~w Mb/s~n", [MbitPerSecRecd]).


main(["udp_ipbus_client" | OtherArgs]) ->
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    Socket = create_udp_socket(),
    %
    Request = ?IPBUS_TXFULL_REQ,
    Reply = ?IPBUS_TXFULL_REP,
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:udp_client_loop(Socket, TargetIP, TargetPort, Request, Reply, {0,NrInFlight}, NrItns) end),
    print_results(NrItns, MicroSecs, byte_size(Request), byte_size(Reply));

main(["udp_ipbus_client2" | OtherArgs]) ->
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    Socket = create_udp_socket(),
    %
    Request = ?IPBUS_BOTHFULL_REQ,
    Reply = ?IPBUS_BOTHFULL_REP,
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:udp_client_loop(Socket, TargetIP, TargetPort, Request, Reply, {0,NrInFlight}, NrItns) end),
    print_results(NrItns, MicroSecs, byte_size(Request), byte_size(Reply));

main(["udp_echo_client" | OtherArgs]) ->
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    Socket = create_udp_socket(),
    %
    Request = ?IPBUS_TXFULL_REQ,
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:udp_client_loop(Socket, TargetIP, TargetPort, Request, Request, {0,NrInFlight}, NrItns) end),
    print_results(NrItns, MicroSecs, byte_size(Request), byte_size(Request));

main(["udp_echo_server"]) ->
    io:format("Starting echo server with options: ~p~n", [?UDP_SOCKET_OPTIONS]),
    ch_unittest_common:start_udp_echo_server(?UDP_SOCKET_OPTIONS);

main(["tcp_echo_client" | OtherArgs]) ->
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    Socket = tcp_connect(TargetIP, TargetPort),
    %
    Request = ?IPBUS_TXFULL_REQ,
    {ok, [{active, ActiveValue}]} = inet:getopts(Socket, [active]),
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:tcp_client_loop({Socket, ActiveValue}, Request, Request, {0,NrInFlight}, NrItns) end ),
    gen_tcp:close(Socket),
    print_results(NrItns, MicroSecs, byte_size(Request), byte_size(Request));

main(["tcp_echo_server", ArgPort]) ->
    Port = list_to_integer(ArgPort),
    io:format("Starting TCP echo server with options: ~p~n", [?TCP_SOCKET_OPTIONS]),
    ch_unittest_common:start_tcp_echo_server(?TCP_SOCKET_OPTIONS, Port);
    
main(["tcp_ch_client", ArgControlHubIP | OtherArgs]) ->
    {ok, ControlHubIP} = inet_parse:address(ArgControlHubIP),
    {TargetIP, TargetPort, NrItns, NrInFlight} = parse_args(OtherArgs),
    Socket = tcp_connect(ControlHubIP, 10203),
    %
    TargetIPU32 = (element(1,TargetIP) bsl 24) + (element(2,TargetIP) bsl 16)
                    + (element(3,TargetIP) bsl 8) + element(4,TargetIP),
    Request = <<TargetIPU32:32,
                TargetPort:16, (byte_size(?IPBUS_TXFULL_REQ) div 4):16,
                (?IPBUS_TXFULL_REQ)/binary
              >>,
    Reply = <<TargetIPU32:32,
              TargetPort:16, 0:16,
              (?IPBUS_TXFULL_REP)/binary>>,
    %
    {ok, [{active, ActiveValue}]} = inet:getopts(Socket, [active]),
    {MicroSecs, ok} = timer:tc( fun () -> ch_unittest_common:tcp_client_loop({Socket, ActiveValue}, Request, Reply, {0,NrInFlight}, NrItns) end ),
    gen_tcp:close(Socket),
    print_results(NrItns, MicroSecs, byte_size(Request), byte_size(Reply));

main(_) ->
    io:format("Incorrect usage!~n"),
    usage().


