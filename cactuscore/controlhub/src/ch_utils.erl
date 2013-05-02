%% Author: rob
%% Created: Dec 15, 2010
%% Description: TODO: Add description to utils
-module(ch_utils).

-include("ch_global.hrl").


%% Exported Functions
-export([log/5, log/4, log/3,
         print_binary_as_hex/1, ipv4_u32_addr_to_tuple/1]).


%%% ------------------------------------------------------------------------------------
%%% API Functions (public interface)
%%% ------------------------------------------------------------------------------------

%% -------------------------------------------------------------------------------------
%% @doc Prints a debug/info/warning/error message, with the State appended to the end of
%%      the message
%%
%% @spec log(Level, Module :: atom(), MsgFmtString :: string(), MsgData :: list(), State :: tuple()) -> ok
%%  where
%%       Level = debug | info | warning | error
%% @end
%% -------------------------------------------------------------------------------------

log(Level, Module, MsgFmtString, MsgData, State) ->
    log(Level, Module, lists:flatten([MsgFmtString, "~n", Module:state_as_string(State)]), MsgData).


%% -------------------------------------------------------------------------------------
%% @doc Prints a debug/info/warning/error message
%%
%% @spec log(Level, Module :: atom(), MsgFmtString :: string(), MsgData :: list() | State :: tuple() ) -> ok
%%  where
%%       Level = debug | info | warning | error
%% @end
%% -------------------------------------------------------------------------------------

log(Level, Module, MsgFmtString, MsgData) when is_list(MsgData) ->
    Preamble = io_lib:format("~w - ~w - ~w ~w ~s", [now(), Level, Module, self(), mini_state_string(Module)]),
    case Level of
         error ->   
             error_logger:error_msg(lists:append(Preamble, MsgFmtString), MsgData);
         warning ->
             error_logger:warning_msg(lists:append(Preamble, MsgFmtString), MsgData);
         _ ->
             error_logger:info_msg(lists:append(Preamble, MsgFmtString), MsgData)
    end,
    ok;
log(Level, Module, MsgFmtString, State) when is_tuple(State) ->
    log(Level, Module, MsgFmtString, [], State).


%% -------------------------------------------------------------------------------------
%% @doc Prints a debug/info/warning/error log message
%%
%% @spec log( Level, Module :: atom(), MsgString :: string() )  ->  ok
%% where
%%      Level = debug | info | warnig | error
%% @end
%% -------------------------------------------------------------------------------------

log(Level, Module, MsgString) ->
    log(Level, Module, MsgString, []).


%% -------------------------------------------------------------------------------------
%% @doc Converts binary to short, descriptive string containing first 2 32-bit words and
%%      last word as hex numbers.
%%
%% @spec short_hex_string( binary() ) -> string()
%% @end
%% -------------------------------------------------------------------------------------

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
    io_lib:format("target ~w:~w", [get(target_ip_tuple), get(target_port)]);
mini_state_string(_) ->
    "".
    

