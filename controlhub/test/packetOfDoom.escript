#!/usr/bin/env escript

main(_) ->

  ByteOrderTr = 16#10fe00f8,

  TestPacket  = <<
                    ByteOrderTr:32/little,   % BOT
                    16#10000018:32/little,   % Read hdr, depth = 0
                    16#00001010:32/little,   % Read addr
                    ByteOrderTr:32/little,   % BOT
                    ByteOrderTr:32/little,   % BOT
                    ByteOrderTr:32/little,   % BOT
                    ByteOrderTr:32/little,   % BOT
                    ByteOrderTr:32/little    % BOT
                >>,

  {ok, Socket} = gen_udp:open(0, [binary]),
  ok = gen_udp:send(Socket, "192.168.200.51", 50001, TestPacket).

