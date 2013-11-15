#!/bin/env escript

% Basic Erlang script that sends IPbus 2.0 status request packet to a given target, and prints the response packet with nice formatting
% Tom Williams, Nov 2013



usage() -> 
   io:format("Usage:  ipbus2_status.escript hostname port~n").


main( [ ArgHost , ArgPort ] ) ->
   io:format("Parsing args~n"),
   Port = list_to_integer(ArgPort),
   run( ArgHost , Port );

main( ["-h"] ) ->
   usage();

main( ["--help"] ) ->
   usage();

main( _ ) ->
   io:format("Incorrect usage!~n"),
   usage().


run( Host, Port ) ->
   io:format("Opening socket~n"),
   {ok, Socket} = gen_udp:open(0, [{active,false}, binary]),
   io:format("Sending IPbus 2.0 status request to ~s:~w~n", [Host, Port]),
   gen_udp:send(Socket, Host, Port, <<16#200000f1:32/big, 0:(15*32)>>),
   io:format("Response:~n"),
   {ok, {_IP, Port, ReplyBin}} = gen_udp:recv(Socket, 0),
   [ io:format("    ~8.16.0B~n", [Int]) || <<Int:32>> <= ReplyBin ].


