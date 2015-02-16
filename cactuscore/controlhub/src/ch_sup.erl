%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since May 2012
%%%
%%% @doc The root supervisor for the Control Hub application.
%%% @end
%%% ===========================================================================
-module(ch_sup).

-behaviour(supervisor).

-include("ch_global.hrl").

%% API exports
-export([start_link/0]).

%% Behavioural exports
-export([init/1]).

-define(SERVER, ?MODULE).  %What is the point of this?  Came in the template...

%%% ====================================================================
%%% API functions (public interface)
%%% ====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%% ====================================================================
%%% Behavioural externals
%%% ====================================================================

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
    ch_utils:log(notice, "ControlHub root supervisor starting now."),
    StatsServer = {ch_stats, {ch_stats, start_link, []},
	               permanent, 2000, worker, [ch_stats]},
    DeviceClientRegistry = {ch_device_client_registry, {ch_device_client_registry, start_link,[]},
                            permanent, 2000, worker, [ch_device_client_registry]},
    TcpListener = {ch_tcp_listener, {ch_tcp_listener, start_link, []},
                   permanent, 2000, worker, [ch_tcp_listener]},
    {ok,{{one_for_one,10,3600}, [StatsServer, DeviceClientRegistry, TcpListener]}}.


