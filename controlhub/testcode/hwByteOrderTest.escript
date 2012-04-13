#!/usr/bin/env escript

main(_) ->

  % PRE-BUILT TEST PACKETS

  ByteOrderTransactionOut = 16#000000f8,

  TestPacket_singleRegisterRead  = <<
                                     ByteOrderTransactionOut:32/big,
                                     ByteOrderTransactionOut:32/big,
                                     ByteOrderTransactionOut:32/big,
                                     ByteOrderTransactionOut:32/big,
                                     ByteOrderTransactionOut:32/big,
                                     16#00020118:32/big,             % Single register read
                                     16#00001010:32/big              % Register address 0x00001010 ("Test" register)
                                  >>,   

  TestPacket_singleRegisterWrite = <<
                                     ByteOrderTransactionOut:32/big,
                                     ByteOrderTransactionOut:32/big,
                                     ByteOrderTransactionOut:32/big,
                                     ByteOrderTransactionOut:32/big,
                                     ByteOrderTransactionOut:32/big,
                                     16#00020128:32/big,             % Read-Modify-Write bits request - will right 0xdeadbeef to the register
                                     16#00001010:32/big,             % Register address 0x00001010 ("Test" register)
                                     16#00000000:32/big,             % And term
                                     16#deadbeef:32/big              % Or term
                                  >>,   

  TestPacket_blockRead200        = <<
                                     ByteOrderTransactionOut:32/big,
                                     ByteOrderTransactionOut:32/big,
                                     ByteOrderTransactionOut:32/big,
                                     ByteOrderTransactionOut:32/big,
                                     ByteOrderTransactionOut:32/big,
                                     16#0002c818:32/big,             % Single register read
                                     16#00001010:32/big              % Register address 0x00001010 ("Test" register)
                                  >>,   

  % CHOOSE YOUR TEST PACKET
  TestPacket = TestPacket_blockRead200,


  % SEND AND RECEIVE TO/FROM HARDWARE
  io:format("About to send following test packet:~n", []),
  utils:print_binary_as_hex(TestPacket),

  % Connect to Control Hub Host and send packet:
  {ok, Socket} = gen_udp:open(0, [binary]),

  DeviceIPaddr = "192.168.200.3",

  ok = gen_udp:send(Socket, DeviceIPaddr, 791, TestPacket),

  % Get the response:
  ReceivedBin = receive
                    {udp, Socket, _, _, Bin} -> Bin
                after 500 ->
                    io:format("No UDP response received!"), halt(1)
                end,

  io:format("Received response is:~n", []),
  utils:print_binary_as_hex(ReceivedBin),

  io:format("~n", []).