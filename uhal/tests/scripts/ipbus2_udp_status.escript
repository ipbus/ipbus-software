#!/usr/bin/env escript

% Basic Erlang script that sends IPbus 2.0 status request packet to a given target, and prints the response packet with nice formatting
% Tom Williams, Nov 2013

-define(TIMEOUT, 1000).


usage() -> 
   io:format("Usage:  ipbus2_status.escript hostname port~n").



main( [ Host , ArgPort ] ) ->
    Port = list_to_integer(ArgPort),
    io:format("Opening UDP socket~n"),
    {ok, Socket} = gen_udp:open(0, [binary]),
    send_status_request(Socket, {Host, Port});

main( ["-h"] ) ->
   usage();

main( ["--help"] ) ->
   usage();

main( _ ) ->
   io:format("Incorrect usage!~n"),
   usage(),
   halt(1).



send_status_request(Socket, {Host,Port}) ->
    send_status_request(Socket, {Host,Port}, 5).


send_status_request(_Socket, {_Host, _Port}, 0) ->
    halt(1); 
send_status_request(Socket, {Host,Port}, RemainingAttempts) -> 
    io:format("~nSending IPbus 2.0 status request to ~s:~w~n", [Host, Port]),
    gen_udp:send(Socket, Host, Port, <<16#200000f1:32/big, 0:(15*32)>>),
    receive
        {udp, Socket, {IP1,IP2,IP3,IP4}, Port, StatusResponse} ->
            io:format("  Received response from IP ~w.~w.~w.~w~n", [IP1,IP2,IP3,IP4]),
            analyse_status_response(StatusResponse)
    after ?TIMEOUT ->
        io:format("  ERROR: No response after ~wms~n", [?TIMEOUT]),
        send_status_request(Socket, {Host,Port}, RemainingAttempts-1)
    end.


-spec analyse_status_response(binary()) -> 'ok' | no_return().
analyse_status_response(StatusResponse) ->
    case size(StatusResponse) of
        64 ->
            io:format("Response (hex):~n"),
            lists:foldl(fun(Word, Idx) -> analyse_status_response_word(Word,Idx), Idx+1 end, 0, [Int || <<Int:32>> <= StatusResponse]);
        Size ->
            io:format("ERROR: Status response has incorrect length, ~w bytes (should be 64 bytes)~n", [Size]),
            io:format("  Packet contents (hex) ...~n"
                      "    "),
            lists:foreach(fun(Byte) -> io:format("~2.16.0B", [Byte]) end, [Int || <<Int:8>> <= StatusResponse]),
            io:format("~n")
    end.


analyse_status_response_word(Word, 0) ->
    Desc = case Word of
               16#200000f1 -> "Packet header";
               _Else -> "ERROR: invalid packet header"
           end,
    io:format("    ~8.16.0B   < ~s~n", [Word, Desc]);
analyse_status_response_word(Word, 1) ->
    io:format("    ~8.16.0B   < MTU (~w bytes)~n", [Word,Word]);
analyse_status_response_word(Word, 2) ->
    io:format("    ~8.16.0B   < Number of response buffers (i.e. max packets in flight)~n", [Word]);
analyse_status_response_word(Word, 3) ->
    io:format("    ~8.16.0B   < Next expected control request header~n", [Word]);
analyse_status_response_word(Word, Idx) when Idx>=8, Idx<12 ->
    io:format("    ~8.16.0B   < Received IPbus control packet header ~w~n", [Word,Idx-8]);
analyse_status_response_word(Word, Idx) when Idx>=12, Idx<16 ->
    io:format("    ~8.16.0B   < Outgoing IPbus control packet header ~w~n", [Word, Idx-12]);
analyse_status_response_word(Word, _Idx) ->
    io:format("    ~8.16.0B~n", [Word]).
