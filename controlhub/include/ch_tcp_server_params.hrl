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
-define(MAX_CONCURRENT_CLIENT_CONNECTIONS, 64).