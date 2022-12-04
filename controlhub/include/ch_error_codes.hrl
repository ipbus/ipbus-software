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

% Implies a dead / unresponsive / non-existant hardware target - based on no reply from control port
-define(ERRCODE_TARGET_CONTROL_TIMEOUT, 1).

% Implies a dead / unresponsive hardware target (for IPbus >= 2.0) - based on no reply from status port
-define(ERRCODE_TARGET_STATUS_TIMEOUT, 3).

% Implies a dead / unresponsive hardware target (for IPbus >= 2.0) - based on no reply from resend port
-define(ERRCODE_TARGET_RESEND_TIMEOUT, 4).

% Implies a malfunctioning hardware target (for IPbus >= 2.0) - based on malformed status packet being received.
-define(ERRCODE_MALFORMED_STATUS, 5).

% Implies that sending IPbus 1.3 request to IPbus 2.0 target, or a 2.0 request to an IPbus 1.3 target.
-define(ERRCODE_WRONG_PROTOCOL_VERSION, 6).

% Implies that device client probably died (i.e. a ControlHub problem).
-define(ERRCODE_CH_DEVICE_CLIENT_TIMEOUT, 2).

% Implies that sending IPbus request to a target that is not in the allow list.
-define(ERRCODE_TARGET_BLOCKED, 7).
