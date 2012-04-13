#!/usr/bin/env escript

%% Starts up a Control Hub Host instance, creates a simple test packet
%% consisting of 6 Byte-order transactions in a row that are to be sent
%% to a single device (DeviceID 1).  The test script then waits for a 
%% response and checks it is correct.

main(_) ->
  startup:start("/Users/rob/Documents/EclipseWorkspace/ControlHubHost/test/config/DeviceAddressFile.erl"),

  % TestPacket stuff
  NumInstructions = 6,
  NumDevices = 1,
  DeviceID = 1,
  ByteOrderTransactionOut = 16#000000f8,
  ByteOrderTransactionRtn = 16#000000fc,


  % Byte-order transaction - pack the actual hardware payload as native, otherwise erlang
  % will try to deliver it as big-endian, because Erlang is always big-endian.  Network
  % stack at both ends should then correctly do host-to-network and network-to-host
  % byte-order conversions for the hardware payload.
  TestPacket = <<
                 NumInstructions:16, NumDevices:16,
                 DeviceID:32,
                 ByteOrderTransactionOut:32/native,
                 ByteOrderTransactionOut:32/native,
                 ByteOrderTransactionOut:32/native,
                 ByteOrderTransactionOut:32/native,
                 ByteOrderTransactionOut:32/native,
                 ByteOrderTransactionOut:32/native
               >>,   
  
  ExpectedResponse =  <<
                        DeviceID:32,
                        ByteOrderTransactionRtn:32/native,
                        ByteOrderTransactionRtn:32/native,
                        ByteOrderTransactionRtn:32/native,
                        ByteOrderTransactionRtn:32/native,
                        ByteOrderTransactionRtn:32/native,
                        ByteOrderTransactionRtn:32/native
                      >>,   

  io:format("About to send following test packet:~n", []),
  utils:print_binary_as_hex(TestPacket),

  % Connect to Control Hub Host and send packet:
  {ok, Socket} = gen_tcp:connect("localhost", 10203, [binary, {packet, 4}]),
  ok = gen_tcp:send(Socket, TestPacket),

  % Get the response:
  ReceivedBin = receive
                    {tcp, Socket, Bin} -> Bin
                end,

  io:format("Received response is:~n", []),
  utils:print_binary_as_hex(ReceivedBin),

  io:format("~n", []),

  % Wait for any subprocess code to finish before the runtime gets terminated by the end of escript.
  timer:sleep(100),

  packet_stats:report(),

  TestPass = (ReceivedBin =:= ExpectedResponse) and
             (packet_stats:get_tcp_in() =:= 1) and
             (packet_stats:get_tcp_out() =:= 1) and
             (packet_stats:get_udp_in() =:= 1) and
             (packet_stats:get_udp_out() =:= 1),

  if
     TestPass ->
        io:format("~n------------~nTest Passed!~n------------~n", []);
     true ->  %% The crap Erlang syntax for "Else"
        io:format("~n------------~nTEST FAILED!~n------------~n", []),
        halt(1)   % Non-zero exit code for calling environment to pick up.
  end.
