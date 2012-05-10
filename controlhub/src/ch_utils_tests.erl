%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since April 2012
%%%
%%% @doc Test-code for the API of the ch_utils module.
%%% @end
%%% ===========================================================================

-module(ch_utils_tests).

-include("ch_global.hrl").

%% API exports
-export([]).


%%% ===========================================================================
%%% Individual Test Functions.
%%% ===========================================================================


ipv4_u32_addr_to_tuple__correct_output_test() -> 
    ?assertEqual({192,168,0,255}, ch_utils:ipv4_u32_addr_to_tuple(16#c0a800ff)),
    ?assertEqual({0,0,0,0}, ch_utils:ipv4_u32_addr_to_tuple(0)),
    ?assertEqual({255,255,255,255}, ch_utils:ipv4_u32_addr_to_tuple(16#ffffffff)).

ipv4_u32_addr_to_tuple__input_negative_throws_test() -> 
    ?assertError(function_clause, ch_utils:ipv4_u32_addr_to_tuple(-1)).

ipv4_u32_addr_to_tuple__input_too_large_throws_test() ->
    ?assertError(function_clause, ch_utils:ipv4_u32_addr_to_tuple(16#100000000)). % 1 bigger than max 32-bit unsigned integer.