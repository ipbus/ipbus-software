%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IPbus protocol bandwith testing module 
%
%       Robert Frazier March 2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(bandwidthTest).

-export([runRxTest/3, runTxTest/3]).


% Sends a large IPbus block-write request repeatedly to the specified device
% for a user-specified number of iterations, in order to generate as much
% outgoing traffic to the hardware device as possible from the client PC.
runTxTest(IPaddress, Port, Iterations) ->

    % BUILD TEST PACKET
    ByteOrderTransactionOut = 16#100000f8,

    TestPacket_start  = <<
                          ByteOrderTransactionOut:32/big,
                          16#10035e20:32/big,             % Block write, 350 deep
                          16#00001000:32/big              % Write starting from base address 0x00001000 ("TestRam" register)
                        >>,

    TestPacket_blockWrite350 = generateBlockWriteBinary(TestPacket_start, 350),
  
    %utils:print_binary_as_hex(TestPacket_blockWrite350),   % Debug - see the outbound packet.
  
    io:format("Process ~p: Transmit-bandwidth throughput test to IP address ~p, port ~p~n", [self(), IPaddress, Port]),
    io:format("Process ~p: Starting ~p iterations of a 350-deep block write request now...:~n", [self(), Iterations]),

    runTestWithTimer(IPaddress, Port, Iterations, TestPacket_blockWrite350).

% Sends a large IPbus block-read request repeatedly to the specified device
% for a user-specified number of iterations, in order to generate as much
% incoming traffic to the client PC as possible from the hardware device.
runRxTest(IPaddress, Port, Iterations) ->
    
    % BUILD TEST PACKET
    ByteOrderTransactionOut = 16#100000f8,

    TestPacket_blockRead350  = <<
                                 ByteOrderTransactionOut:32/big,
                                 ByteOrderTransactionOut:32/big,
                                 ByteOrderTransactionOut:32/big,
                                 ByteOrderTransactionOut:32/big,
                                 ByteOrderTransactionOut:32/big,
                                 16#10035e40:32/big,             % Non-incrementing block read, 350 deep  (change to 10035e18 for standard block-read)
                                 16#00001000:32/big              % Read address 0x00001000 ("TestRam" register)
                               >>,

    %utils:print_binary_as_hex(TestPacket_blockRead350),   % Debug - see the outbound packet.

    io:format("Process ~p: Receive-bandwidth throughput test to IP address ~p, port ~p~n", [self(), IPaddress, Port]),
    io:format("Process ~p: Starting ~p iterations of a 350-deep block read request now...:~n", [self(), Iterations]),

    runTestWithTimer(IPaddress, Port, Iterations, TestPacket_blockRead350).


% Runs the test for a given test packet binary, and times how long it takes
runTestWithTimer(IPaddress, Port, Iterations, TestPacket) ->
    T1 = now(),
    case runTest(IPaddress, Port, Iterations, TestPacket) of
      ok ->
          T2 = now(),
          TotalSeconds = timer:now_diff(T2, T1)/1000000,
          TotalPayloadKB = Iterations*350/256,
          DataRateKB_s = TotalPayloadKB/TotalSeconds,
          io:format("~n~nBandwidth Test Results:~n"),
          io:format("-----------------------~n~n"),
          io:format("Total IPbus payload transferred = ~.2f KB~n", [TotalPayloadKB]),
          io:format("Total time taken                = ~.2f s~n", [TotalSeconds]),
          io:format("Average bandwidth               = ~.2f KB/s~n", [DataRateKB_s]),
          ok;
      error ->
          error
    end.


% Runs the test for a given test packet binary
runTest(IPaddress, Port, Iterations, TestPacket) ->
    % Store the total iterations for later (this is a constant)
    put(totalIterations, Iterations),
    % Open up UDP port.
    {ok, Socket} = gen_udp:open(0, [binary]),
    % Send the packet lots of times.
    sendReceiveLoop(IPaddress, Port, Socket, TestPacket, Iterations, 0).    


sendReceiveLoop(IPaddress, Port, Socket, TestPacket, IterationsLeft, TotalErrors) when (IterationsLeft > 0) and (TotalErrors < 10) ->
    % Send packet
    ok = gen_udp:send(Socket, IPaddress, Port, TestPacket),
    % Get the response:
    receive
        {udp, Socket, _, _, _Bin} ->
            %utils:print_binary_as_hex(_Bin),   % Debug - see the inbound packet.
            sendReceiveLoop(IPaddress, Port, Socket, TestPacket, IterationsLeft-1, TotalErrors)
    after 500 ->
        CurrentInteration = get(totalIterations) - IterationsLeft,
        io:format("Process ~p: No UDP response received on iteration ~p!~n", [self(), CurrentInteration]),
        sendReceiveLoop(IPaddress, Port, Socket, TestPacket, IterationsLeft-1, TotalErrors+1)   
     end;


sendReceiveLoop(_IPaddress, _Port, _Socket, _TestPacket, _IterationsLeft, TotalErrors) when (TotalErrors < 10) ->
    io:format("Process ~p: Finished test!~n", [self()]),
    ok;


sendReceiveLoop(_IPaddress, _Port, _Socket, _TestPacket, _IterationsLeft, _TotalErrors) ->
    io:format("Process ~p: Test aborted - too many errors!~n", [self()]),
    error.


generateBlockWriteBinary(Bin, ValuesRemainingToFill) when ValuesRemainingToFill > 0 ->
    NewBin = <<Bin/binary, 16#deadbeef:32/big>>,
    generateBlockWriteBinary(NewBin, ValuesRemainingToFill-1);


generateBlockWriteBinary(Bin, 0) -> Bin.

