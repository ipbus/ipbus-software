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
-export([udp_echo_server/1]).


%%% ---------------------------------------------------------------------------
%%% API Functions
%%% ---------------------------------------------------------------------------

%% ---------------------------------------------------------------------
%% @doc UDP echo server - basically it's very simple dummy hardware and
%%      just returns whatever it gets sent.  Note, you'll probably want
%%      to spawn a subprocess to run this.  To shutdown the server
%%      normally, simply send it the atom "die" (no quotes, obviously).
%%
%% @spec udp_echo_server(PortU16::integer())
%% @end
%% ---------------------------------------------------------------------
udp_echo_server(PortU16) ->
    {ok, Socket} = gen_udp:open(PortU16, [binary]),
    udp_echo_server_loop(Socket).


%%% ---------------------------------------------------------------------------
%%% Local Functions
%%% ---------------------------------------------------------------------------

%% The receive loop implementation for the udp echo server.
udp_echo_server_loop(Socket) ->
    receive
        {udp, Socket, IP, Port, Packet} ->
            gen_udp:send(Socket, IP, Port, Packet),
            udp_echo_server_loop(Socket);
        die -> ok % For a receiving a clean/normal exit message.
    end.