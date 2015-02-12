-module(erltestapp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1,talking_proc/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    spawn(?MODULE, talking_proc, [3]),
    spawn(?MODULE, talking_proc, [5]),
    spawn(?MODULE, talking_proc, [11]),
    spawn(?MODULE, talking_proc, [23]),
    {ok,
      { {one_for_one, 5, 10}, []}
    }.



talking_proc(NrTimes) ->
    talking_proc_loop(0, NrTimes). 


talking_proc_loop(MaxCount, MaxCount) ->
    lager:info("Proc ~p committing suicide now ...", [self()]),
    true = false;
talking_proc_loop(Counter, MaxCount) ->
    timer:sleep(2000),
    lager:info("Low-priority log message ~p/~p from proc ~p", [Counter, MaxCount, self()]),
    timer:sleep(1000),
    io:format("Hi! this is proc ~p~n", [self()]),
    timer:sleep(2000),
    lager:emergency("Super-critical log message ~p/~p from proc ~p", [Counter, MaxCount, self()]),
    talking_proc_loop(Counter+1, MaxCount).
