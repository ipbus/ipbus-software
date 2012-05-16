%%% ---------------------------------------------------------------------------
%%% @author Robert Frazier
%%%
%%% @since May 2012
%%%
%%% @doc Various possible error codes that can be sent as part of the ControlHub
%%%      responses to the user client (uHAL)
%%% @end
%%% ---------------------------------------------------------------------------

% Everything was fine
-define(ERRCODE_SUCCESS, 0).

% Implies a dead / unresponsive / non-existant hardware target.
-define(ERRCODE_TARGET_TIMEOUT, 1).

% Implies that device client probably died (i.e. a ControlHub problem).
-define(ERRCODE_CH_DEVICE_CLIENT_TIMEOUT, 2).