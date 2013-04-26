#!/usr/bin/env escript
%% -*- escript -*-
%% 

%%% ============================================================
%%% Checks that the behaviour of an IPbus 2.0 target matches
%%% the IPbus 2.0 specification.
%%%
%%% Tom Williams, April 2013
%%% ============================================================

-mode(compile).

%% -------------------------------------------------------------
%% MACROS
%% -------------------------------------------------------------

-define(HW_TIMEOUT, 500).

-define(MAX_IPBUS_PKT_SIZE, 8*4). % 368*4).

%% -------------------------------------------------------------
%% MAIN / ARG PARSING FUNCTIONS
%% -------------------------------------------------------------

% @spec main(Args::[term()]) -> ok
main(Args) ->
    parse_arguments(Args),
    io:format("IPbus 2.0 target testing parameters ...~n"
              "    - IP addr / port: ~w:~w~n"
              "    - Writeable registers start @ ~w, width ~w~n",
              [get(target_ip_tuple), get(target_port), get(base_reg), get(width)]),
    % Basic transaction tests
    test_read(),
    test_ni_read(),
    test_write(),
    test_ni_write(),
    test_rmwbits(),
    test_rmwsum(),
    % Basic status/resend/ping tests
    test_status_and_pkt_id_logic(),
    test_resend(),
    test_ping(),
    % Extensive test
    test_continuous().


parse_arguments([IPAddrPortArg, "-b", BaseAddrArg, "-w", WidthArg]) ->
    [IP1, IP2, IP3, IP4, PortString] = string:tokens(IPAddrPortArg, ":."),
    put(target_ip_tuple, {list_to_integer(IP1),
                          list_to_integer(IP2),
                          list_to_integer(IP3),
                          list_to_integer(IP4)}),
    put(target_port, list_to_integer(PortString)),
    put(base_reg, list_to_integer(BaseAddrArg)),
    put(width, list_to_integer(WidthArg));

parse_arguments(_) ->
    io:format("Incorrect usage!~n"),
    halt(1).



%% ---------------------------------------------------------------
%% HIGH-LEVEL TEST FUNCTIONS CALLED FROM MAIN
%% ---------------------------------------------------------------

% @spec test_read() -> pass | fail
test_read() ->
    todo("test_read").

test_ni_read() ->
    todo("test_ni_read").

test_write() ->
    todo("test_write").

test_ni_write() ->
    todo("test_ni_write").

test_rmwbits() ->
    todo("test_rmwbits").

test_rmwsum() ->
    todo("test_rmwsum").

test_status_and_pkt_id_logic() ->
    todo("test_status_and_pkt_id_logic").

test_resend() ->
    todo("test_resend").

test_ping() ->
    todo("test_ping").

test_continuous() ->
    todo("test_continuous").


todo(FuncName) when is_list(FuncName) ->
    io:format("~n~s~n    Function not implemented yet~n", [FuncName]).


