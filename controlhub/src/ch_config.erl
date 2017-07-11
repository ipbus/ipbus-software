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

-export([init/0, get/1, get_start_time/0]).

-type config_param() :: config_file | app_start_timestamp
                       | tcp_listen_port | tcp_socket_opts
                       | max_udp_in_flight | device_response_timeout | device_client_shutdown_after .


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
    ok.


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

