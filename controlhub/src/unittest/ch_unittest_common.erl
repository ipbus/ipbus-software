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
-export([spawn_udp_echo_server/1, udp_echo_server/1, dummy_request_data_generator/1]).


%%% ---------------------------------------------------------------------------
%%% API Functions
%%% ---------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
%% @doc Spawn a UDP echo server - basically it's very simple dummy hardware and
%%      just returns whatever it gets sent. To shutdown the server normally,
%%      simply send it the atom "shutdown" (no quotes, obviously). Note that
%%      this does not use spawn_link - you have to shut down the server with
%%      the "shutdown" message.
%%
%% @spec spawn_udp_echo_server(PortU16::integer()) -> pid()
%% @end
%% ----------------------------------------------------------------------------
spawn_udp_echo_server(PortU16) ->
    spawn(?MODULE, udp_echo_server, [PortU16]).


%% The start-point for the UDP echo server
udp_echo_server(PortU16) ->
    {ok, Socket} = gen_udp:open(PortU16, [binary]),
    udp_echo_server_loop(Socket).


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


%% The receive loop implementation for the udp echo server.
udp_echo_server_loop(Socket) ->
    receive
        {udp, Socket, IP, Port, Packet} ->
            gen_udp:send(Socket, IP, Port, Packet),
            udp_echo_server_loop(Socket);
        shutdown -> ok % For a receiving a clean/normal exit message.
    end.