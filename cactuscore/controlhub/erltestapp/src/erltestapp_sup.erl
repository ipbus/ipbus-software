-module(erltestapp_sup).

-compile([{parse_transform, lager_transform}]).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1,talking_proc_loop/0]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    Pid = spawn_link(?MODULE,talking_proc_loop,[]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    spawn_link(?MODULE,talking_proc_loop,[]),
    {ok, { {one_for_one, 5, 10}, []} }.



talking_proc_loop() ->
    timer:sleep(5000),
    lager:info("Low-priority log message from proc ~p", [self()]),
    io:format("Hi! this is proc ~p", [self()]),
    lager:emergency("Super-critical log message from proc ~p", [self()]),
    talking_proc_loop().
