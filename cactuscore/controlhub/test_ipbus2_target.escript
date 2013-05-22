#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa /opt/cactus/lib/controlhub/lib/controlhub-1.1.0/ebin opt/cactus/lib/controlhub/lib/controlhub-1.1.0/ebin/unittest

% ---------------------------------------------------------------------
% Script that sends stream of packets/transactions to control port of 
% hardware and checks that responses are correct. Inbetween each packet
% to control port, it also checks that status packet and re-send port are
% working correctly
% 
% Tom Williams, Jan 2013
% ---------------------------------------------------------------------

%% --------------------------------------------------------------------
%% MACROS -- for global var type stuff
%% --------------------------------------------------------------------
-define(HW_IP_ADDR, {127, 0, 0, 1}). %{192,168,200,32}).

-define(TARGET_PORT, 50001).

-define(TARGET_MTU, 2000).

-define(TARGET_BUFFERSIZE, 5).

-define(HW_TIMEOUT, 140).

-define(MAX_IPBUS_PKT_SIZE, 368*4).

%% --------------------------------------------------------------------
%% RECORDS
%% --------------------------------------------------------------------
-record(trans, {type, id, addr, params, valid, before}).

-record(state, {runmode, totalnr, verbosity, i=1, i_max, nextpktid, remtrans=[], socket}).

%% --------------------------------------------------------------------
%% usage function
%% --------------------------------------------------------------------
usage() ->
    io:format("This script sends IPBus2.0 packets to H/W (looping over: status, write, re-send request, ping) and checks for correct response.~nIt exits immediately when there is an incorrect response (or no response at all).~n~n"), 
    io:format("Usage:~n   test_ipbus2_extensive.escript  NextPktId  NumIterations RunMode~n"),
    io:format("where~n"),
    io:format("    NextPktId     = Next packet ID expected by board~n"),
    io:format("    NumIterations = Number of packet IDs to loop over  (OPTIONAL, default is 100)~n"),
    io:format("    RunMode       = default OR resends   (OPTIONAL, default value is default!)~n").


%% --------------------------------------------------------------------
%% Main function
%% --------------------------------------------------------------------

main(["help"]) ->
    usage();
main(["-h"]) ->
    usage();
main(["--help"]) ->
    usage();


main([Arg1]) ->
    main([Arg1, "100"]);
main([Arg1, Arg2]) ->
    main([Arg1, Arg2, "default"]);
main([Arg1, Arg2, Arg3]) ->
    io:format("Parsing command line args ...~n"),
    FirstPktId = list_to_integer(Arg1),
    io:format("  + Next expected packet id = ~p~n", [FirstPktId]),
    case FirstPktId of 
        _ when FirstPktId > 65535 -> 
            io:format("    ERROR: Invalid packet ID ; it must be in range 0-65535~n"),
            halt(1);
        _ -> void
    end,
    NrIterations = list_to_integer(Arg2),
    io:format("  + Number iterations       = ~p~n", [NrIterations]),
    RunMode = list_to_atom(Arg3),
    io:format("  + Run mode : ~p~n", [RunMode]),
    %
     {ok, Socket} = gen_udp:open(10203, [binary]),
     try test_loop(#state{runmode=default, verbosity=verbose, i_max=NrIterations, nextpktid=FirstPktId, socket=Socket}) of
         X -> X
     catch
         throw:{bad_response, Request, ExpResponse, RealResponse} ->
             io:format(" ERROR : BAD RESPONSE received from H/W ...~n"),
             io:format("~n - Packet sent:~n"),
             ch_utils:print_binary_as_hex(Request),
             io:format("~n - I received:~n"),
             ch_utils:print_binary_as_hex(RealResponse),
             if
               ExpResponse=:=timeout ->
                 io:format("~n - But I expected a timeout.~n");
               true ->
                 io:format("~n - But I expected:~n"),
                 ch_utils:print_binary_as_hex(ExpResponse)
             end,
             io:format("~n~nBailing out early!~n");
         throw:{timeout, Request, ExpResponse} ->
             io:format(" ERROR : NO RESPONSE received from H/W ... ~n"),
             io:format("~n - Packet sent (~p bytes / ~p words):~n", [size(Request), size(Request)/4]),
             ch_utils:print_binary_as_hex(Request),
             io:format("~n - And I expected this response:~n"),
             ch_utils:print_binary_as_hex(ExpResponse);
         throw:{ping_timeout} ->
             io:format(" ERROR : PING TIMEOUT detected here~n~n")
     end,
     gen_udp:close(Socket);

main(_) ->
    io:format("Incorrect usage!~n~n"),
    usage().


%% --------------------------------------------------------------------
%% Internal functions  - TESTING LOOP
%% --------------------------------------------------------------------
    
%-record(state, {runmode, totalnr, verbosity, i=1, nextpktid, remtrans=[], socket}).

test_loop(S) when S#state.totalnr =:= S#state.i ->
    io:format("~n ** END OF TEST **~n");
test_loop(#state{runmode = _Mode, verbosity = Verb, i = I, nextpktid = ExpdId, remtrans = RemTrans, socket=Socket} = S) ->
    if
      Verb =:= verbose -> 
        io:format("~n~n============ Iteration ~p (expected packet ID ~p) =============~n", [I, ExpdId]);
      (I rem 1000)=:=0 ->
        io:format(" On iteration ~p of ~p ~n", [I, S#state.totalnr]);
      true ->
        void
    end,
    PktId = case random:uniform(2) of 
                1 -> ExpdId;
                2 -> 0
            end,
    io:format("Generating list of transactions ... ~n"),
    ListOfTrans = top_up_trans_seq(400, RemTrans),
    io:format("Generating next control packet ... ~n"),
    {ControlReq, ExpdControlRes, TransInPkt, NewRemTrans} = gen_pkt_from_head( rand_end(), PktId, rand_int(8, 340*4), ListOfTrans),
    % Check status packet
    send_and_check_reply(Socket, ?HW_IP_ADDR, gen_status_request(), ?TARGET_PORT, {expected_status_response(ExpdId), <<((1 bsl 32*4)-1):(32*4),0:(32*12)>>}, ?TARGET_PORT, "Sending status packet ..."),
    % Try valid packet
    io:format(" Control transactions in packet (ID=~w):~n", [PktId]),
    lists:foreach(fun(Rcd) -> io:format("    + ~s~n", [trans_as_string(Rcd)]) end, TransInPkt ),
    send_and_check_reply(Socket, ?HW_IP_ADDR, ControlReq, ?TARGET_PORT, ExpdControlRes, ?TARGET_PORT, "Sending control packet ..."),
    NewExpdId = if
                  PktId=:=0 ->
                    ExpdId;
                  ExpdId=:=65535 ->
                    1;
                  true ->
                    ExpdId+1
                end,
    % Try valid packet with wrong ID
%    send_and_check_reply(Socket, ?HW_IP_ADDR, change_pkt_id(ControlReq, [0, NewExpdId]), ?TARGET_PORT, timeout, ?TARGET_PORT, "Sending control packet with wrong ID ..."),
    % Check packet re-send
    ReSendReq = <<(16#200000f2 + (PktId bsl 8)):32>>,
    send_and_check_reply(Socket, ?HW_IP_ADDR, ReSendReq, ?TARGET_PORT, ExpdControlRes, ?TARGET_PORT, "Asking for re-send ..."),
    % Try re-send request with wrong ID
%    send_and_check_reply(Socket, ?HW_IP_ADDR, change_pkt_id(ReSendReq, [0, ExpdId | [decrement_pkt_id(ExpdId, X) || X <- lists:seq(1,?TARGET_BUFFERSIZE)]] ), ?TARGET_PORT, timeout, ?TARGET_PORT, "Asking for re-send with wrong packet ID ..."),
    % Pinging board
    test_ping(?HW_IP_ADDR, Verb),
    % Loop back around
    case I of 
         I when I=:=S#state.i_max ->
             success;
         _ ->
             test_loop(S#state{i=I+1, nextpktid=NewExpdId, remtrans=NewRemTrans})
    end.

test_ping(IpAddr, quiet) ->
    test_ping(IpAddr);
test_ping(IpAddr, verbose) ->
    io:format(" Pinging board ...~n"),
    test_ping(IpAddr),
    io:format("     successful!~n").

test_ping(IpAddr) ->
    PingOut = os:cmd("ping -c 1 "++ipv4_tuple_to_string(IpAddr)),
    NrLinesPingOut = length( lists:filter(fun(X) -> case X of
                                                        $\n -> true;
                                                        _ -> false
                                                    end
                                          end, PingOut) ),
    case NrLinesPingOut of
        6 ->
            void;
        _ ->
            io:format("~s~n", [PingOut]),
            throw({ping_timeout})
    end.
    
%decrement_pkt_id(PktId) when is_integer(PktId), PktId>1 ->
%    if
%      PktId=:=1 -> 16#ffff;
%      true -> PktId-1
%    end.

%decrement_pkt_id(Id, N) when is_integer(Id), is_integer(N), N>0 ->
%    decrement_pkt_id(Id, {0, N});
%decrement_pkt_id(Id, {N, N}) ->
%    Id;
%decrement_pkt_id(Id, {I, N}) ->
%    decrement_pkt_id(decrement_pkt_id(Id), {Id+1, N}).


%% --------------------------------------------------------------------
%% Internal functions  - GENERATING LISTS OF TRANSACTIONS
%% --------------------------------------------------------------------

top_up_trans_seq(TargetNr, TransRcdAcc) when length(TransRcdAcc)<TargetNr ->
    top_up_trans_seq(TargetNr, lists:append(TransRcdAcc, gen_trans_subseq()) );
top_up_trans_seq(_TargetNr, TransRcdAcc) ->
    TransRcdAcc.


gen_trans_subseq() ->
    case rand_int(1,2) of
         1 -> gen_trans_subseq(single);
         _ -> gen_trans_subseq(block)
    end.


gen_trans_subseq(single) ->
    Addr = 2,
    NrTrans = rand_int(1,10),
    gen_trans_subseq(Addr, NrTrans, [], unknown);
gen_trans_subseq(block) ->
    Addr = 16#1000,
    NrTrans = rand_int(1,10),
    Depth = rand_int(2, 255),
    gen_trans_subseq({Addr, Depth}, NrTrans, [], unknown).


gen_trans_subseq(AD, NrTrans, TransRcdAcc, PrevValues) when length(TransRcdAcc)<NrTrans ->
    {_Addr, Depth} = if
                      is_tuple(AD) -> AD;
                      true -> {AD, 1}
                    end,
    Type = if 
             PrevValues =:= unknown -> 
               write;
             Depth > 1 ->
               rand_block_trans_type();
             true ->
               rand_single_trans_type()
           end,
    Valid = valid,
    Params = case Type of 
                 read -> [];
                 write -> rand_uint32s(Depth);
                 rmwbits -> rand_uint32s(2);
                 rmwsum -> rand_uint32s(1)
             end,
    Rcd = #trans{type=Type, 
                 id=rand_int(1, 16#fff)-1,
                 addr = AD,
                 valid = Valid,
                 params = Params,
                 before = PrevValues},
    gen_trans_subseq(AD, NrTrans, [Rcd | TransRcdAcc], reg_values_after(Rcd));
gen_trans_subseq(_Addr, _NrTrans, TransRcdAcc, _PreviousValues) ->
    lists:reverse(TransRcdAcc).


reg_values_after(#trans{type=Type, before=PrevValues, params=Params}) ->
    case Type of
        read -> 
            PrevValues;
        write -> 
            Params;
        rmwbits ->
            [PrevVal] = PrevValues,
            [AndTerm, OrTerm] = Params,
            [( (PrevVal band AndTerm) bor OrTerm)];
        rmwsum ->
            [PrevVal] = PrevValues,
            [Addend] = Params,
            [(PrevVal + Addend) rem (1 bsl 32)]
    end.

%% ------------------------------------------------------------------------------------------------
%% Iternal functions -- GENERATING PACKETS
%% ------------------------------------------------------------------------------------------------

rand_int(A,B) when B > A ->
    A - 1 + random:uniform(B - A + 1).

rand_uint32() ->
    random:uniform(1 bsl 32) - 1.

rand_uint32s(N) when N > 0 ->
    rand_uint32s(N, []).

rand_uint32s(N, Acc) when length(Acc)<N ->
    rand_uint32s(N, [rand_uint32()|Acc]);
rand_uint32s(_N, Acc) ->
    Acc.

rand_end() ->
    big.

rand_single_trans_type() ->
    case rand_int(1,4) of
        1 -> read;
        2 -> write;
        3 -> rmwbits;
        4 -> rmwsum
    end.

rand_block_trans_type() ->
    case rand_int(1,2) of 
        1 -> read;
        2 -> write
    end.

% Given a list of transaction records, returns an IPbus 2.0 control request/response packet, a list of the transactions in the packet, and the remaining transactions
% @spec gen_pkt_from_head(End, Id, TargetSize, TransRcdList) -> {ReqPacket, ResPacket, TransInPkt, RemainingTrans}
% where
%    End = big | little | native
%    Id = unsigned_integer()
%    TargetSize = u_integer() = Desired packet size in 32-bit words
%    TransRcdList = [#trans()]
% end
gen_pkt_from_head(End, Id, TargetSize, TransRcdList) ->
    gen_pkt_from_head(End, Id, TargetSize, TransRcdList, pkt_header(End, Id), pkt_header(End,Id), []).

gen_pkt_from_head(End, _Id, TargetSize, [TransRcdH|TransRcdT] = TransRcdList, ReqPktAcc, ResPktAcc, TransInPktAcc) ->
    {ReqTransSize, ResTransSize} = trans_binary_size(TransRcdH),
    PktFull = if 
                size(ReqPktAcc) >= TargetSize ; size(ResPktAcc) >= TargetSize ->
                  true;
                (size(ReqPktAcc) + ReqTransSize) > ?MAX_IPBUS_PKT_SIZE ->
                  true;
                (size(ResPktAcc) + ResTransSize) > ?MAX_IPBUS_PKT_SIZE ->
                  true;
                true ->
                  false
              end, 
    if 
      PktFull ->
        {ReqPktAcc, ResPktAcc, lists:reverse(TransInPktAcc), TransRcdList};
      true ->
        {ReqTrans, ResTrans} = gen_trans(End, TransRcdH),
        gen_pkt_from_head(End, _Id, TargetSize, TransRcdT, <<ReqPktAcc/binary, ReqTrans/binary>>, <<ResPktAcc/binary, ResTrans/binary>>, [TransRcdH|TransInPktAcc])
    end.

change_pkt_id(Bin, Id) when is_integer(Id) ->
    <<Hdr:4/binary, Body/binary>> = Bin,
    case Hdr of
        % Big endian
        <<16#20:8, _:16, 16#f:4, Type:4>> ->
            <<16#20:8, Id:16/big, 16#f:4, Type:4, Body/binary>>;
        <<16#f:4, Type:4, _:16, 16#20:8>> ->
            <<16#f:4, Type:4, Id:16/little, 16#20:8, Body/binary>>
    end;

change_pkt_id(Bin, ExcludedIds) when is_list(ExcludedIds) ->
    NewId = random:uniform(16#10000)-1,
    case lists:member(NewId, ExcludedIds) of 
        true ->
            change_pkt_id(Bin, ExcludedIds);
        false ->
            change_pkt_id(Bin, NewId)
    end.


pkt_header(End, Id) ->
    X = (2 bsl 28) + (Id bsl 8) + (15 bsl 4),
    case End of 
        big -> 
            << X:32/big >>;
        little ->
            << X:32/little >>;
        native ->
            << X:32/native >>
    end.
%% ------------------------------------------------------------------------------------------------
%% Internal functions -- GENERATING TRANSACTION BINARIES
%% ------------------------------------------------------------------------------------------------


% Returns the size (in bytes) of the request & response binaries bor an IPbus 2.0 transaction
trans_binary_size(#trans{type=Type, params=Params, before=Before}) ->
    case Type of 
        read -> 
            Depth = length(Before),
            {8, 4+4*Depth};
        write ->
            {8+4*length(Params), 4};
        rmwbits ->
            {4*4, 4*2};
        rmwsum ->
            {4*3, 4*2}
    end.


% Returns the binary for an IPbus 2.0 transaction request, the expected response binary
% @spec gen_trans
gen_trans(End, #trans{type=Type, id=Id, params=Params, valid=Valid, before=Current} = Rcd) when is_list(Params), Valid=:=valid ->
    Addr = case Rcd#trans.addr of 
               {A, _} -> 
                   A;
               A when is_integer(A) ->
                   A
           end,
    case Type of
        read ->
            Req = trans_binary(End, Id, length(Current), 0, 15, [Addr]),
            Res = trans_binary(End, Id, length(Current), 0, 0, Current),
            {Req, Res};
        
        write ->
            Req = trans_binary(End, Id, length(Params), 1, 15, [Addr|Params]),
            Res = trans_binary(End, Id, length(Params), 1, 0, []),
            {Req, Res};

        rmwbits ->
            Req = trans_binary(End, Id, 1, 4, 15, [Addr|Params]),
            Res = trans_binary(End, Id, 1, 4, 0, Current),
            {Req, Res};

        rmwsum when length(Params)=:=1 ->
            Req = trans_binary(End, Id, 1, 5, 15, [Addr|Params]),
            Res = trans_binary(End, Id, 1, 5, 0, Current),
            {Req, Res}
    end.

    
%% Returns the binary for an IPbus 2.0 transaction
trans_binary(End, Id, Words, TypeId, InfoCode, WordsInBody) ->
    if 
      Id > 16#fff ; Words > 16#ff -> 
        trans_binary(End, Id rem 16#fff, Words rem 16#ff, TypeId, InfoCode, WordsInBody);
      true ->
        HdrAsUInt = (2 bsl 28)
                  + (Id bsl 16)
                  + (Words bsl 8)
                  + (TypeId bsl 4)
                  + InfoCode,
        create_binary(End, [HdrAsUInt|WordsInBody])
    end.

   
% Creates binary containing integers encoded in 32-bit words 
% @ spec create_binary(End, ListOfInts) -> binary()
% where 
%     End        = little | big | native
%     ListOfInts = [integer()]
% end
create_binary(End, ListOfInts) ->
    create_binary(End, ListOfInts, <<>>).

create_binary(_End, [], BinAcc) ->
    BinAcc;
create_binary(Endness, [H|T], BinAcc) ->
    BinH = case Endness of
               big -> 
                   << H:32/big >>;
               little -> 
                   << H:32/little >>;
               native ->
                   << H:32/native >>
           end,
    create_binary(Endness, T, <<BinAcc/binary, BinH/binary>>).
    

gen_status_request() ->
    << 16#200000f1:32, 0:(32*15) >>.

expected_status_response(NextPktId) ->
    << 2:4, 0:20, 16#f1:8,
       ?TARGET_MTU:32,
       ?TARGET_BUFFERSIZE:32,
       2:4, 0:4, NextPktId:16, 16#f0:8,
       0:(32*12) >>.


%% --------------------------------------------------------------------------
%% Internal functions  --  Interactions with H/W
%% --------------------------------------------------------------------------

%% Send reply to device and waits for response
%% @spec send_and_check_reply(IPaddr, Request::binary(), RequestPort, ExpResponse::binary(), ResponsePort) ->
send_and_check_reply(Socket, IPaddr, Request, RequestPort, {ExpResponse, BitMaskForCheck}, ResponsePort, IoStringBeforeSend) ->
    if 
      IoStringBeforeSend =/= "" ->
        io:format(" ~s~n", [IoStringBeforeSend]);
      true ->
        void
    end,
    ok = gen_udp:send(Socket, IPaddr, RequestPort, Request), 
    receive
        {udp, Socket, IPaddr, ResponsePort, ResponsePkt} ->
            MaskedRes = binary_and(ResponsePkt, BitMaskForCheck),
            case ExpResponse of
                MaskedRes ->
                    correct;
                BadResponse ->
                    throw({bad_response, Request, ExpResponse, MaskedRes})
            end
    after ?HW_TIMEOUT ->
        case ExpResponse of
            timeout -> correct;
            _ -> throw({timeout, Request, ExpResponse})
        end
    end,
    if
      IoStringBeforeSend =/= "", ExpResponse=/=timeout ->
        io:format("    Correct response received!~n");
      IoStringBeforeSend =/= "" ->
        io:format("    Timeout as expected!~n");
      true ->
        void
    end;

send_and_check_reply(Socket, IPaddr, Request, RequestPort, ExpResponse, ResponsePort, IoStringBeforeSend) ->
    send_and_check_reply(Socket, IPaddr, Request, RequestPort, {ExpResponse, 1}, ResponsePort, IoStringBeforeSend).


binary_and(A, 1) ->
    A;
binary_and(A, B) ->
    Size = size(A)*8,
    <<X:Size>> = A,
    <<Y:Size>> = B,
    <<(X band Y):Size>>.

%% -------------------------------------------------------------------------
%% string/print functions
%% -------------------------------------------------------------------------
ipv4_tuple_to_string({IP_1, IP_2, IP_3, IP_4}) ->
    integer_to_list(IP_1)++"."
        ++integer_to_list(IP_2)++"."
        ++integer_to_list(IP_3)++"."
        ++integer_to_list(IP_4).


trans_as_string(#trans{type=Type, params=Params, valid=Valid, before=PrevValues} = Rcd) ->%when size(PrevValues)=:=1 ->
    NewValues = reg_values_after(Rcd),
    {Addr, Depth} = case Rcd#trans.addr of 
                        {A, D} -> {A, D};
                        A -> {A, 1}
                    end,
    if 
        Type=:=read ->
            io_lib:format("read    @ ~s, ~p, ~p-word, reg value: ~s", [hex(Addr), Valid, Depth, hex(NewValues)]); 
        Type=:=write ->
            io_lib:format("write   @ ~s, ~p, ~p-word, reg value --> ~s", [hex(Addr), Valid, Depth, hex(NewValues)]);
        Type=:=rmwbits ->
            [AndTerm, OrTerm] = Params,
            io_lib:format("rmwbits @ ~s, ~p, (And=~s, Or=~s), reg value: ~s --> ~s", [hex(Addr), Valid, hex(AndTerm), hex(OrTerm), hex(PrevValues), hex(NewValues)]);
        Type=:=rmwsum ->
            [Addend] = Params,
            io_lib:format("rmwsum  @ ~s, ~p, (Addend=~s), reg value: ~s --> ~s", [hex(Addr), Valid, hex(Addend), hex(PrevValues), hex(NewValues)]);
        true ->
            "unknown transaction type"
    end.

hex([Int])->
    hex(Int);
hex(Int) when is_integer(Int), Int>=0 ->
    Prefix = "0x",
    Digits = [hex_digit((Int bsr X) band 16#f ) || X <- lists:seq(28, 0, -4) ],
    lists:append(Prefix,Digits);
hex(L) when is_list(L), length(L) < 6 ->
    lists:concat([hex(X)++", " || X <- L ]);
hex(_) ->
    "<several values>".

hex_digit(Int) when is_integer(Int), Int>=0, Int<16 ->
    case Int of
        0 -> "0";
        1 -> "1";
        2 -> "2";
        3 -> "3";
        4 -> "4";
        5 -> "5";
        6 -> "6";
        7 -> "7";
        8 -> "8";
        9 -> "9";
        10 -> "a";
        11 -> "b";
        12 -> "c";
        13 -> "d";
        14 -> "e";
        15 -> "f"
    end.

