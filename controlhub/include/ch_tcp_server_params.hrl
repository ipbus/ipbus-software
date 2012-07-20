%%% ---------------------------------------------------------------------------
%%% @author Robert Frazier
%%%
%%% @since May 2012
%%%
%%% @doc Options that affect how the Control Hub behaves as a TCP server.
%%%      
%%% @end
%%% ---------------------------------------------------------------------------

% The port on which the Control Hub will listen for TCP connections.
-define(CONTROL_HUB_TCP_LISTEN_PORT, 10203).

% The maximum number of TCP user-client connections we will offer.
-define(MAX_CONCURRENT_CLIENT_CONNECTIONS, 256).

% The various TCP socket options the ControlHub uses
% Note that {packet, 4} implies that there is a 4-byte, big-endian "logical stream-size header" at
% the front of every logically complete TCP request stream sent to the control hub.  This 32-bit
% header simply tells Erlang how many bytes will follow in order that it can correctly receive
% the complete TCP stream. 
-define(TCP_SOCKET_OPTIONS, [binary, {packet, 4}, {reuseaddr, true}, {active, true}, {backlog, ?MAX_CONCURRENT_CLIENT_CONNECTIONS}]).
