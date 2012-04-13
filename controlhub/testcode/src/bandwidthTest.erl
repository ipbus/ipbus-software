%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IPbus protocol bandwith testing module 
%
%       Robert Frazier March 2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(bandwidthTest).

-export([runRxTest/3, runTxTest/3, runJumboTxTest/3, runJumboTxTestNonIncr/3, runJumboRxTest/3, runJumboRxTestNonIncr/3]).


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

    TestPacket_blockWrite350 = generateBlockWriteBinary(TestPacket_start, 0, 350),
  
    %utils:print_binary_as_hex(TestPacket_blockWrite350),   % Debug - see the outbound packet.
  
    io:format("Process ~p: Transmit-bandwidth throughput test to IP address ~p, port ~p~n", [self(), IPaddress, Port]),
    io:format("Process ~p: Starting ~p iterations of a 350-deep block write request now...:~n", [self(), Iterations]),

    runTestWithTimer(IPaddress, Port, Iterations, TestPacket_blockWrite350, 350).

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

    runTestWithTimer(IPaddress, Port, Iterations, TestPacket_blockRead350, 350).


% A crude and rather laborious test for sending the equivalent of a 2200 deep block write; however, it's done in
% several concatenated requests as the IPbus v1.3 headers only support block reads/writes up to a depth of ~500.
runJumboTxTest(IPaddress, Port, Iterations) ->

    % BUILD TEST PACKET
    ByteOrderTransactionOut = 16#100000f8,

    TestPacket_start  = <<
                          ByteOrderTransactionOut:32/big,
                          16#1003F420:32/big,             % Block write, 500 deep, transaction id=1
                          16#00001000:32/big              % Write starting from base address 0x00001000 ("TestRam" register)
                        >>,
    
    TestPacket_blockWrite_1 = generateBlockWriteBinary(TestPacket_start, 0, 500),
  
    TestPacket_blockWrite_2a = <<TestPacket_blockWrite_1/binary,
                                    16#1005F420:32/big,      % Block write, 500 deep, transaction id=2
                                    16#000011F4:32/big       % Write starting from base address 0x000011F4 (500 into "TestRam" register)
                                  >>,

    TestPacket_blockWrite_2b = generateBlockWriteBinary(TestPacket_blockWrite_2a, 500, 1000),

    TestPacket_blockWrite_3a = <<TestPacket_blockWrite_2b/binary,
                                    16#1007F420:32/big,      % Block write, 500 deep, transaction id=3
                                    16#000013E8:32/big       % Write starting from base address 0x000013e8 (1000 into "TestRam" register)
                                  >>,

    TestPacket_blockWrite_3b = generateBlockWriteBinary(TestPacket_blockWrite_3a, 1000, 1500),
 
    TestPacket_blockWrite_4a = <<TestPacket_blockWrite_3b/binary,
                                    16#1009F420:32/big,      % Block write, 500 deep, transaction id=4
                                    16#000015DC:32/big       % Write starting from base address 0x000015DC (1500 into "TestRam" register)
                                  >>,    

    TestPacket_blockWrite_4b = generateBlockWriteBinary(TestPacket_blockWrite_4a, 1500, 2000),
    
    TestPacket_blockWrite_5a = <<TestPacket_blockWrite_4b/binary,
                                    16#100AC820:32/big,      % Block write, 200 deep, transaction id=5
                                    16#000017D0:32/big       % Write starting from base address 0x000017D0 (2000 into "TestRam" register)
                                  >>,    

    TestPacket_blockWrite_5b = generateBlockWriteBinary(TestPacket_blockWrite_5a, 2000, 2200),

    
    %utils:print_binary_as_hex(TestPacket_blockWrite_5b),   % Debug - see the outbound packet.
  
    io:format("Process ~p: Transmit-bandwidth throughput test to IP address ~p, port ~p~n", [self(), IPaddress, Port]),
    io:format("Process ~p: Starting ~p iterations of a 2200-deep block write request now...:~n", [self(), Iterations]),

    runTestWithTimer(IPaddress, Port, Iterations, TestPacket_blockWrite_5b, 2200).


% A crude and rather laborious test for sending the equivalent of a 2200 deep non-incrementing block write; however, it's done in
% several concatenated requests as the IPbus v1.3 headers only support block reads/writes up to a depth of ~500.
runJumboTxTestNonIncr(IPaddress, Port, Iterations) ->

    % BUILD TEST PACKET
    ByteOrderTransactionOut = 16#100000f8,

    TestPacket_start  = <<
                          ByteOrderTransactionOut:32/big,
                          16#1003F448:32/big,             % Non-incrementing block write, 500 deep, transaction id=1
                          16#00000001:32/big              % Write starting from base address 0x00000001 ("Test" register)
                        >>,
    
    TestPacket_blockWrite_1 = generateBlockWriteBinary(TestPacket_start, 0, 500),
  
    TestPacket_blockWrite_2a = <<TestPacket_blockWrite_1/binary,
                                    16#1005F448:32/big,      % Non-incrementing block write, 500 deep, transaction id=2
                                    16#00000001:32/big       % Write starting from base address 0x00000001 ("Test" register)
                                  >>,

    TestPacket_blockWrite_2b = generateBlockWriteBinary(TestPacket_blockWrite_2a, 500, 1000),

    TestPacket_blockWrite_3a = <<TestPacket_blockWrite_2b/binary,
                                    16#1007F448:32/big,      % Non-incrementing block write, 500 deep, transaction id=3
                                    16#00000001:32/big       % Write starting from base address 0x00000001 ("Test" register)
                                  >>,

    TestPacket_blockWrite_3b = generateBlockWriteBinary(TestPacket_blockWrite_3a, 1000, 1500),
 
    TestPacket_blockWrite_4a = <<TestPacket_blockWrite_3b/binary,
                                    16#1009F448:32/big,      % Non-incrementing block write, 500 deep, transaction id=4
                                    16#00000001:32/big       % Write starting from base address 0x00000001 ("Test" register)
                                  >>,    

    TestPacket_blockWrite_4b = generateBlockWriteBinary(TestPacket_blockWrite_4a, 1500, 2000),
    
    TestPacket_blockWrite_5a = <<TestPacket_blockWrite_4b/binary,
                                    16#100AC848:32/big,      % Non-incrementing block write, 200 deep, transaction id=5
                                    16#00000001:32/big       % Write starting from base address 0x00000001 ("Test" register)
                                  >>,    

    TestPacket_blockWrite_5b = generateBlockWriteBinary(TestPacket_blockWrite_5a, 2000, 2200),

    
    %utils:print_binary_as_hex(TestPacket_blockWrite_5b),   % Debug - see the outbound packet.
  
    io:format("Process ~p: Transmit-bandwidth throughput test to IP address ~p, port ~p~n", [self(), IPaddress, Port]),
    io:format("Process ~p: Starting ~p iterations of a 2200-deep non-incrementing block write request now...:~n", [self(), Iterations]),

    runTestWithTimer(IPaddress, Port, Iterations, TestPacket_blockWrite_5b, 2200).


% A crude and rather laborious test for sending the equivalent of a 2200 deep block read; however, it's done in
% several concatenated requests as the IPbus v1.3 headers only support block reads/writes up to a depth of ~500.
runJumboRxTest(IPaddress, Port, Iterations) ->
    
    % BUILD TEST PACKET
    ByteOrderTransactionOut = 16#100000f8,

    TestPacket_blockRead     = <<
                                 ByteOrderTransactionOut:32/big,
                                 16#1003F418:32/big,             % Block read, 500 deep, transaction id=1
                                 16#00001000:32/big,             % Read address 0x00001000 ("TestRam")
                                 16#1005F418:32/big,             % Block read, 500 deep, transaction id=2
                                 16#000011F4:32/big,             % Read starting from base address 0x000011F4 (500 into "TestRam" register)
                                 16#1007F418:32/big,             % Block read, 500 deep, transaction id=3
                                 16#000013E8:32/big,             % Read starting from base address 0x000013E8 (1000 into "TestRam" register)
                                 16#1009F418:32/big,             % Block read, 500 deep, transaction id=4
                                 16#000015DC:32/big,             % Read starting from base address 0x000015DC (1500 into "TestRam" register)
                                 16#100AC818:32/big,             % Block read, 200 deep, transaction id=5
                                 16#000017D0:32/big              % Read starting from base address 0x000017D0 (2000 into "TestRam" register)
                               >>,

    %utils:print_binary_as_hex(TestPacket_blockRead),   % Debug - see the outbound packet.

    io:format("Process ~p: Receive-bandwidth throughput test to IP address ~p, port ~p~n", [self(), IPaddress, Port]),
    io:format("Process ~p: Starting ~p iterations of a 2200-deep block read request now...:~n", [self(), Iterations]),

    runTestWithTimer(IPaddress, Port, Iterations, TestPacket_blockRead, 2200).


% A crude and rather laborious test for sending the equivalent of a 2200 deep non-incr block read; however, it's done in
% several concatenated requests as the IPbus v1.3 headers only support block reads/writes up to a depth of ~500.
runJumboRxTestNonIncr(IPaddress, Port, Iterations) ->
    
    % BUILD TEST PACKET
    ByteOrderTransactionOut = 16#100000f8,

    TestPacket_blockRead     = <<
                                 ByteOrderTransactionOut:32/big,
                                 16#1003F440:32/big,             % Non-incrementing block read, 500 deep, transaction id=1
                                 16#00000001:32/big,             % Read address 0x00000001 ("Test" register)
                                 16#1005F440:32/big,             % Non-incrementing block read, 500 deep, transaction id=2
                                 16#00000001:32/big,              
                                 16#1007F440:32/big,             % Non-incrementing block read, 500 deep, transaction id=3
                                 16#00000001:32/big,              
                                 16#1009F440:32/big,             % Non-incrementing block read, 500 deep, transaction id=4
                                 16#00000001:32/big,              
                                 16#100AC840:32/big,             % Non-incrementing block read, 200 deep, transaction id=5
                                 16#00000001:32/big              
                               >>,

    %utils:print_binary_as_hex(TestPacket_blockRead),   % Debug - see the outbound packet.

    io:format("Process ~p: Receive-bandwidth throughput test to IP address ~p, port ~p~n", [self(), IPaddress, Port]),
    io:format("Process ~p: Starting ~p iterations of a 2200-deep non-incrementing block read request now...:~n", [self(), Iterations]),

    runTestWithTimer(IPaddress, Port, Iterations, TestPacket_blockRead, 2200).


% Runs the test for a given test packet binary, and times how long it takes
runTestWithTimer(IPaddress, Port, Iterations, TestPacket, TotalDepth) ->
    T1 = now(),
    case runTest(IPaddress, Port, Iterations, TestPacket) of
      ok ->
          T2 = now(),
          TotalSeconds = timer:now_diff(T2, T1)/1000000,
          IterationFreq = Iterations/TotalSeconds,
          TotalPayloadKB = Iterations*TotalDepth/256,
          DataRateKB_s = TotalPayloadKB/TotalSeconds,
          io:format("~n~nBandwidth Test Results:~n"),
          io:format("-----------------------~n~n"),
          io:format("Total IPbus payload transferred = ~.2f KB~n", [TotalPayloadKB]),
          io:format("Total time taken                = ~.2f s~n", [TotalSeconds]),
          io:format("Iteration frequency             = ~.2f Hz~n", [IterationFreq]),
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
    {ok, Socket} = gen_udp:open(0, [binary, {buffer, 36000}]),
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
    after 5000 ->
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


generateBlockWriteBinary(Bin, CurrentValue, EndValue) when CurrentValue < EndValue ->
    NewBin = <<Bin/binary, CurrentValue:32/big>>,
    generateBlockWriteBinary(NewBin, CurrentValue+1, EndValue);


generateBlockWriteBinary(Bin, CurrentValue, EndValue) when CurrentValue >= EndValue ->
    Bin.

