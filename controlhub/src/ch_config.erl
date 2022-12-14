%%% ===========================================================================
%%% @author Tom Williams
%%%
%%% @since February 2015
%%%
%%% @doc Module that provides uniform interface to configuration parameters for
%%%      ControlHub processes; some of these are taken from the system config
%%%      file, others may currently be hardcoded here.
%%%
%%% ===========================================================================

-module(ch_config).

%% Exported functions

-export([init/0, get/1, get_start_time/0, get_parameter_names/0, get_device_access_status/2]).

-type config_param() :: config_file | app_start_timestamp
                       | tcp_listen_port | tcp_socket_opts
                       | max_udp_in_flight | device_response_timeout | device_client_shutdown_after
                       | device_allowlist | device_allowlist_file | device_allowlist_mode .


-define(TCP_SOCKET_OPTS, [binary, {packet, 4}, {reuseaddr, true}, {nodelay, true}, {active, true}, {backlog, 256}, {buffer, 200000}, {low_watermark, 200000}, {high_watermark, 400000}, {recbuf, 200000}, {sndbuf, 200000}]).



%%% ------------------------------------------------------------------------------------
%%% API functions (public interface)
%%% ------------------------------------------------------------------------------------

%% -------------------------------------------------------------------------------------
%% @doc Extracts the configuration parameter values from the relevant sources.
%%
%% @spec init() -> ok
%% @end
%% -------------------------------------------------------------------------------------

-spec init() -> ok.

init() -> 
    ets:new(ch_config_table, [set, named_table, {read_concurrency, true}]),
    set_param(app_start_timestamp, os:timestamp()),
    set_param(config_file, case application:get_env(config_file) of
                               {ok, Value} -> Value; 
                               undefined -> undefined 
                           end),  
    set_param(tcp_socket_opts, ?TCP_SOCKET_OPTS),
    set_param_from_ch_env(tcp_listen_port),
    set_param_from_ch_env(max_udp_in_flight),
    set_param_from_ch_env(device_response_timeout),
    set_param_from_ch_env(device_client_shutdown_after),
    set_param_from_ch_env(device_allowlist_mode),
    set_param_from_ch_env(device_allowlist_file),
    case get_app_env(controlhub, device_allowlist_file) of
        none ->
            set_param(device_allowlist, none),
            ok;
        FilePath ->
            case parse_device_allowlist(FilePath) of
                {error, ErrorMessage} ->
                    {stop, ErrorMessage};
                {ok, AllowList} ->
                    set_param(device_allowlist, AllowList),
                    ok
            end
    end.


%% ------------------------------------------------------------------------------------
%% @doc Returns value of configuration parameter
%%
%% @spec get(Parameter :: config_param()) -> Value :: any()
%% @end
%% ------------------------------------------------------------------------------------

-spec get(config_param()) -> any().
get(Parameter) ->
   [{Parameter, Value}] = ets:lookup(ch_config_table, Parameter),
   Value.



%% ------------------------------------------------------------------------------------
%% @doc Returns the timestamp for when ch_config was initialised
%%
%% @spec get_start_time() -> erlang:timestamp() 
%% @end
%% ------------------------------------------------------------------------------------

-spec get_start_time() -> erlang:timestamp().
get_start_time() ->
   ?MODULE:get(app_start_timestamp).


-spec get_parameter_names() -> [any()].
get_parameter_names() ->
    [tcp_listen_port, tcp_socket_opts, max_udp_in_flight,
     device_response_timeout, device_client_shutdown_after,
     device_allowlist, device_allowlist_file, device_allowlist_mode].


get_device_access_status(IPAddr, Port) ->
    case ?MODULE:get(device_allowlist) of
        none ->
            allow;
        AllowDict ->
            case dict:find(IPAddr, AllowDict) of
                {ok, any} ->
                    allow;
                {ok, Port} ->
                    allow;
                {ok, _AnotherPort} ->
                    deny;
                error ->
                    deny
            end
    end.


%% Internal functions

-spec get_app_env(App :: atom(), Param :: config_param()) -> Value :: any() | no_return().
get_app_env(App, Param) ->
    case  application:get_env(App, Param) of
        {ok, Value} ->
            Value;
        undefined ->
            erlang:error({undefined_env, App, Param})
    end.


-spec set_param(Param :: config_param(), Value :: any()) -> ok.
set_param(Param, Value) ->
    ets:insert(ch_config_table, {Param, Value}).


-spec set_param_from_ch_env(Param :: config_param()) -> ok.
set_param_from_ch_env(Param) ->
    set_param(Param, get_app_env(controlhub, Param)).


parse_device_allowlist(FilePath) ->
    {ok, File} = file:open(FilePath, [read]),
    parse_device_allowlist(File, dict:new(), 1).

parse_device_allowlist(File, Dict, LineNumber) ->
    case file:read_line(File) of
        {ok, Line} ->
            TrimmedLine = string:strip(string:strip(string:strip(Line, both, $\n), both, $\t), both, $ ),
            % ch_utils:log(debug, "allowlist, line ~w: \"~s\"", [LineNumber, TrimmedLine]),
            case TrimmedLine of
                % Lines starting with '#' are comments - ignore them (# = 35)
                [35 | _] ->
                    parse_device_allowlist(File, Dict, LineNumber + 1);
                % Also lines that were empty or just contained spaces (removed by string:strip)
                [] ->
                    parse_device_allowlist(File, Dict, LineNumber + 1);
                % All other lines
                _ ->
                    case re:run(TrimmedLine, "[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}(?::[0-9]+)?") of
                        {match, _} ->
                            case re:split(TrimmedLine, "[\.:]", [{return, list}]) of
                                [IP1Str, IP2Str, IP3Str, IP4Str] ->
                                    IP1 = element(1, string:to_integer(IP1Str)),
                                    IP2 = element(1, string:to_integer(IP2Str)),
                                    IP3 = element(1, string:to_integer(IP3Str)),
                                    IP4 = element(1, string:to_integer(IP4Str)),
                                    ch_utils:log(debug, "Device allowlist: Adding ~w.~w.~w.~w (any port) ", [IP1, IP2, IP3, IP4]),
                                    NewDict = dict:store({IP1, IP2, IP3, IP4}, any, Dict);
                                [IP1Str, IP2Str, IP3Str, IP4Str, PortStr] ->
                                    IP1 = element(1, string:to_integer(IP1Str)),
                                    IP2 = element(1, string:to_integer(IP2Str)),
                                    IP3 = element(1, string:to_integer(IP3Str)),
                                    IP4 = element(1, string:to_integer(IP4Str)),
                                    Port = element(1, string:to_integer(PortStr)),
                                    ch_utils:log(debug, "Device allowlist: Adding ~w.~w.~w.~w:~w", [IP1, IP2, IP3, IP4, Port]),
                                    NewDict = dict:store({IP1, IP2, IP3, IP4}, Port, Dict)
                            end,
                            parse_device_allowlist(File, NewDict, LineNumber + 1);
                        nomatch ->
                            ch_utils:log(error, "Line ~w of allowlist file doesn't match expected format", [LineNumber]),
                            {error, io:format("Line ~w of allowlist file doesn't match expected format", [LineNumber])}
                    end
            end;
        eof ->
            {ok, Dict}
    end.
