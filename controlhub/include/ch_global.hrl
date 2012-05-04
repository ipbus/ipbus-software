%%% ---------------------------------------------------------------------------
%%% @author Robert Frazier
%%%
%%% @since April 2012
%%%
%%% @doc Control Hub system-wide includes, macro constants needed by multiple
%%%      files, etc.
%%% @end
%%% ---------------------------------------------------------------------------

% Unit-test framework include - needed by ~all.
-include_lib("eunit/include/eunit.hrl").

% Trace macro module - needed by ~all.
-include("ch_trace_macros.hrl").

% The port on which the Control Hub will listen for TCP connections.
-define(CONTROL_HUB_TCP_LISTEN_PORT, 10203).

% The maximum number of TCP user-client connections we will offer.
-define(MAX_CONCURRENT_CLIENT_CONNECTIONS, 64).

% Device Client UDP timeout when communicating with a target hardware device.
-define(DEVICE_CLIENT_UDP_TIMEOUT, 200).

