%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since April 2012
%%%
%%% @doc Test-code for the API of the ch_stats module.
%%% @end
%%% ===========================================================================

-module(ch_stats_tests).

-include("ch_global.hrl").

%% API exports
-export([]).


%%% ===========================================================================
%%% Test code begins
%%% ===========================================================================



%% ----------------------------------------------------------------------------
%% 
%% ----------------------------------------------------------------------------
basic_test()->
    ?assert(true).

%% basic_test_() ->
%%     { setup,
%%       fun ch_stats:start_link/0,
%%       fun ch_stats:stop/0,
%%       [ fun test_client_stats/0 ]
%%     }.
%% 
%% test_client_stats() ->
%%     ?assertEqual(ch_stats:get_active_client(), 0).