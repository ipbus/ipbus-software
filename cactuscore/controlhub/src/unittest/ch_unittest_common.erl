%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since May 2012
%%%
%%% @doc Common bits and pieces of code needed for the unit-testing.
%%% @end
%%% ===========================================================================
-module(ch_unittest_common).


%% Exported Functions
-export([spawn_device_emulator/2, device_emulator_init/2, dummy_request_data_generator/1]).


%%% ---------------------------------------------------------------------------
%%% API Functions
%%% ---------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
%% @doc Spawns simple dummy hardware that checks the packet id is correct for 
%%      IPbus 2.0 packets (no check on header otherwise), and then just returns
%%      whatever it gets sent. To shut it down normally simply send it the atom 
%%      "shutdown" (no quotes, obviously). Note that this does not use 
%%      spawn_link - you have to shut down the server with the "shutdown" message.
%%
%% @spec spawn_device_emulator(Ver::integer(), PortU16::integer()) -> pid()
%% @end
%% ----------------------------------------------------------------------------
spawn_device_emulator(Ver, PortU16) ->
    spawn(?MODULE, device_emulator_init, [Ver, PortU16]).


%% The start-point for the dummy hardware
device_emulator_init(Ver, PortU16) ->
    case Ver of
         1 -> 
             {ok, Socket} = gen_udp:open(PortU16, [binary]),
             device_emulator_loop(Ver, Socket);
         2 -> 
             {ok, Socket0} = gen_udp:open(PortU16, [binary]),
             {ok, Socket1} = gen_udp:open(PortU16+1, [binary]),
             {ok, Socket2} = gen_udp:open(PortU16+2, [binary]),
             device_emulator_loop(Ver, {Socket0, Socket1, Socket2, 7645})
    end.


%% ----------------------------------------------------------------------------
%% @doc Returns a binary of random data made up of N 32-bit words, where N is
%%      a random integer value between 1 and MaxRequestLength (inclusive).
%%
%% @spec dummy_request_data_generator(MaxRequestLength::integer()) -> binary()
%% @end
%% ----------------------------------------------------------------------------
dummy_request_data_generator(MaxRequestLength) ->
    RequestLengthInBytes = random:uniform(MaxRequestLength) * 4,
    % slightly hacky way to produce random request data.
    list_to_binary([random:uniform(256)-1 || _X <- lists:seq(1, RequestLengthInBytes)]).


%%% ---------------------------------------------------------------------------
%%% Local Functions
%%% ---------------------------------------------------------------------------


%% The receive loop implementation for the hardware emulator
device_emulator_loop(1, Socket) ->
    receive
        {udp, Socket, IP, Port, Packet} ->
            gen_udp:send(Socket, IP, Port, Packet),
            device_emulator_loop(1, Socket);
        shutdown -> 
            ok % For a receiving a clean/normal exit message.
    end;

device_emulator_loop(2, {ControlSocket, StatusSocket, ResendSocket, NextExpdId, LastControlResponse} = State) ->
    receive
        {udp, ControlSocket, IP, Port, <<Header:4/binary, _/binary>> = Packet} ->
            ExpdHdrUI32 = (16#200000f0 + (NextExpdId bsl 8)),
            if 
              (Header =:= <<ExpdHdrUI32:32/big>>) or (Header =:= <<ExpdHdrUI32:32/little>>) ->
                gen_udp:send(ControlSocket, IP, Port, Packet),
                device_emulator_loop(2, {ControlSocket, StatusSocket, ResendSocket, NextExpdId+1, Packet});
              true ->
                device_emulator_loop(2, State)
            end;
        {udp, StatusSocket, IP, Port, <<16#200000f0:32, 16#200000f0:32, 16#200000f0:32, 16#200000f0:32, 
                                        16#200000f0:32, 16#200000f0:32, 16#200000f0:32, 16#200000f0:32, 
                                        16#200000f0:32, 16#200000f0:32, 16#200000f0:32, 16#200000f0:32, 
                                        16#200000f0:32, 16#200000f0:32, 16#200000f0:32, 16#200000f0:32>>} ->
            Response = <<16#200000ff:32, 8184:32, 
                         1:32, ((16#2 bsl 28) + (NextExpdId bsl 8) + 16#f0):32,
                         0:(32*12)>>,
            gen_udp:send(StatusSocket, IP, Port, Response),
            device_emulator_loop(2, State);
        {udp, ResendSocket, IP, Port, _} ->
            gen_udp:send(ControlSocket, IP, Port, LastControlResponse),
            device_emulator_loop(2, State);
        shutdown ->
            ok
    end.
    
