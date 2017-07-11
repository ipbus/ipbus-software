%%% ===========================================================================
%%% @author Robert Frazier 
%%% @author Tom Williams
%%%
%%% @since May 2012
%%%
%%% @doc Starting point for the Control Hub application.  This module provides
%%%      any startup logic for the system, and starts the root supervisor.
%%% @end
%%% ===========================================================================
-module(controlhub_app).

-behaviour(application).

-include("ch_global.hrl").

%% Behavioural exports
-export([start/2,stop/1]).


%%% ====================================================================
%%% Behavioural externals
%%% ====================================================================

%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
    Vsn = case application:get_key(vsn) of
              {ok, Version} -> Version;
              Else -> io_lib:format("~w", [Else])
          end,
    ch_utils:log(notice, "Starting the ControlHub application (version ~s).", [Vsn]),
    ch_config:init(),
%    fprof:trace(start),
    % Start the root supervisor and return its process ID.
    case ch_sup:start_link() of
      {ok, Pid} ->
          {ok, Pid};
      Other ->
          {error, Other}
    end.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ch_utils:log(notice, "Stopping the ControlHub application."),
    ok.



