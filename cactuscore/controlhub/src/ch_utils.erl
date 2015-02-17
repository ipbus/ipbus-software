%%% =================================================================================================
%%% @author Robert Frazier
%%% @author Tom Williams
%%%
%%% @since Dec 2012
%%%
%%% @doc General functions used by more than one of the main ControlHub modules.
%%% @end
%%% =================================================================================================

-module(ch_utils).

-include("ch_global.hrl").


%% Exported Functions
-export([log/3, log/2, ip_port_string/2, tcp_peername_string/1,
         print_binary_as_hex/1, ipv4_u32_addr_to_tuple/1]).



-type log_level() :: debug | info | notice | warning | error | critical | emergency.



%%% ------------------------------------------------------------------------------------
%%% API Functions (public interface)
%%% ------------------------------------------------------------------------------------


%% -------------------------------------------------------------------------------------
%% @doc Prints a log message (no data), and an optional prefix to message
%%
%% @spec log(Level :: (log_level() | {log_level()|string()}), MsgFmtString :: string()) -> ok
%% @end
%% -------------------------------------------------------------------------------------

log(LevelAndOptionalPrefix, MsgFmtString) ->
    log(LevelAndOptionalPrefix, MsgFmtString, []).


%% -------------------------------------------------------------------------------------
%% @doc Prints a log message with data, and an optional prefix to message
%%
%% @spec log(Level :: (log_level() | {log_level()|string()}), MsgFmtString :: string(), MsgData :: list()) -> ok
%% @end
%% -------------------------------------------------------------------------------------

log({Level, PrefixString}, MsgFmtString, MsgData) ->
    implement_log(Level, [$~, $s, $ , $-, $  | MsgFmtString], [ PrefixString | MsgData]);
log(Level, MsgFmtString, MsgData) when is_atom(Level) ->
    implement_log(Level, MsgFmtString, MsgData).


%% -------------------------------------------------------------------------------------
%% @doc Prints a log message with data
%%
%% @spec implement_log(Level :: log_level(), MsgFmt :: string(), MsgData :: list()) -> ok 
%% @end
%% -------------------------------------------------------------------------------------

implement_log(Level, MsgFmt, MsgData) when is_list(MsgFmt), is_list(MsgData) ->
    case Level of 
         emergency ->
             lager:emergency(MsgFmt, MsgData);
         alert ->
             lager:alert(MsgFmt, MsgData);
         critical ->
             lager:critical(MsgFmt, MsgData);
         error ->   
             lager:error(MsgFmt, MsgData);
         warning ->
             lager:warning(MsgFmt, MsgData);
         notice ->
             lager:notice(MsgFmt, MsgData);
         info ->
             lager:info(MsgFmt, MsgData);
         debug ->
             lager:debug(MsgFmt, MsgData)
    end,
    ok.


%% -------------------------------------------------------------------------------------
%% @doc Converts IP address (either U32 integer, or 4-tuple) and port into string of 
%%      standard format ip1.ip2.ip3.ip4:port
%%
%% @spec ip_port_string(IP :: (tuple() || integer() >= 0), Port :: (integer() >= 0) ) -> string()
%%
%% @end
%% -------------------------------------------------------------------------------------

ip_port_string(IP, Port) when is_tuple(IP) ->
    io_lib:format("~s:~w", [inet_parse:ntoa(IP), Port]);
ip_port_string(IP, Port) when is_integer(IP) ->
    ip_port_string( ipv4_u32_addr_to_tuple(IP), Port );
ip_port_string(IP, Port) ->
    io_lib:format("invalid_ip(~w):~w", [IP, Port]).


%% -------------------------------------------------------------------------------------
%% @doc Returns IP & port at other end of TCP socket in format ip1.ip2.ip3.ip4:port
%%
%% @spec tcp_peername_string(Socket :: socket()) -> string()
%% @end
%% -------------------------------------------------------------------------------------

tcp_peername_string( Socket ) when is_port(Socket) ->
    {ok, {IP, Port}} = inet:peername(Socket),
    ip_port_string(IP, Port).


%% -------------------------------------------------------------------------------------
%% @doc Prints timestamp in nice string format for log messages
%% 
%% @spec timestamp_string( {integer() >= 0, integer() >= 0, integer() >= 0} ) -> string()
%% @end
%% -------------------------------------------------------------------------------------

timestamp_string( {_,_,Micro} = TS ) ->
    {{Yr,Month,Day},{Hr,Min,Sec}} = calendar:now_to_universal_time(TS),
    io_lib:format("~2..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~6..0w", [Day,Month,Yr-2000,Hr,Min,Sec,Micro]).


% %% -------------------------------------------------------------------------------------
% %% @doc Converts binary to short, descriptive string containing first 2 32-bit words and
% %%      last word as hex numbers.
% %%
% %% @spec short_hex_string( binary() ) -> string()
% %% @end
% %% -------------------------------------------------------------------------------------

%short_hex_string(Binary) when is_binary(Binary) ->
%    "<<>>".


%% -------------------------------------------------------------------------------------
%% @doc Given a binary containing an integer number of 32-bit words, it will print to
%%      console the content as a column of 32-bit-wide hex-numbers.
%% 
%% @spec print_binary_as_hex(Binary::binary()) -> ok
%% @end
%% -------------------------------------------------------------------------------------
print_binary_as_hex(Binary) when is_binary(Binary) ->
    case size(Binary) rem 4 of
        0 -> do_print_binary_as_hex(Binary);
        _ -> io:format("    * Binary printout skipped! *~n"
                       "    *   Does not contain an    *~n"
                       "    *    integer number of     *~n"
                       "    *      32-bit words!       *~n")
    end.

%% -------------------------------------------------------------------------------------
%% @doc Converts an IPv4 address in raw u32 form into a tuple of the four address bytes.
%%      E.g.: the 32-bit hex number 0x89abcdef goes to { 137, 171, 205, 239 }.
%% 
%% @spec ipv4_u32_addr_to_tuple(IPaddrRawU32::integer()) -> ip4_address()
%% where
%%       ip4_address() = {0..255, 0..255, 0..255, 0..255}
%% @end
%% -------------------------------------------------------------------------------------
ipv4_u32_addr_to_tuple(IPaddrRawU32) when is_integer(IPaddrRawU32),
                                      IPaddrRawU32 >= 0,
                                      IPaddrRawU32 < 4294967296 ->
    <<Byte3:8/integer, Byte2:8/integer, Byte1:8/integer, Byte0:8/integer>> = <<IPaddrRawU32:32/integer>>,
    {Byte3, Byte2, Byte1, Byte0}.
    

%%% --------------------------------------------------------------------
%%% Internal functions (private)
%%% --------------------------------------------------------------------

%% -------------------------------------------------------------------------------------
%% @doc Extracts a 32-bit value from the binary and breaks it down into eight nibbles
%%      that get converted into the characters 0-9,a-f as appropriate and printed to
%%      the console. Rinse and repeat until binary is sucked dry.
%% @spec do_print_binary_as_hex(binary()) -> ok
%% @end
%% -------------------------------------------------------------------------------------

do_print_binary_as_hex(<<Nibble1:4/integer, Nibble2:4/integer, Nibble3:4/integer, Nibble4:4/integer,
                         Nibble5:4/integer, Nibble6:4/integer, Nibble7:4/integer, Nibble8:4/integer,
                         Remainder/binary>>) ->
  HexNumbersLine = [Nibble1, Nibble2, Nibble3, Nibble4, Nibble5, Nibble6, Nibble7, Nibble8],
  HexLettersLine = lists:map(fun(X) -> hex_number_to_hex_letter(X) end, HexNumbersLine),
  io:format("    0x~c~c~c~c~c~c~c~c~n", HexLettersLine),
  do_print_binary_as_hex(Remainder);

do_print_binary_as_hex( << >> ) -> ok.


%% Converts a number between 0-15 to the appropriate ASCII hex chararacter (0123456789abcdef)
%% @spec hex_number_to_hex_letter(HexNumber::integer()) -> integer()
hex_number_to_hex_letter(HexNumber) ->
    case HexNumber of
        0  -> $0;
        1  -> $1;  
        2  -> $2;
        3  -> $3;
        4  -> $4;
        5  -> $5;
        6  -> $6;
        7  -> $7;
        8  -> $8;
        9  -> $9;
        10 -> $a;
        11 -> $b;
        12 -> $c;
        13 -> $d;
        14 -> $e;
        15 -> $f
    end.


%% Prints a mini state string for given module, based off process's dictionary
%%  variables in
%% @spec mini_state_string( Module :: atom() ) -> string()

mini_state_string(ch_device_client) ->
    {IP1,IP2,IP3,IP4} = get(target_ip_tuple),
    io_lib:format(" -> ~w.~w.~w.~w:~w", [IP1, IP2, IP3, IP4, get(target_port)]);
mini_state_string(_) ->
    "".
    

