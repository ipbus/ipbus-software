%% Author: rob
%% Created: Dec 15, 2010
%% Description: TODO: Add description to utils
-module(utils).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([print_binary_as_hex/1]).

%%
%% API Functions
%%

%% Given a binary containing an integer number of 32-bit words, it will print to screen
%% the content as a column of 32-bit-wide hex-numbers.

print_binary_as_hex(Binary) when is_binary(Binary) ->
    case size(Binary) rem 4 of
        0 -> do_print_binary_as_hex(Binary);
        _ -> {error, needs_integer_number_32bit_words}
    end.


%%
%% Local Functions
%%

do_print_binary_as_hex(<<Nibble1:4/integer, Nibble2:4/integer, Nibble3:4/integer, Nibble4:4/integer,
                         Nibble5:4/integer, Nibble6:4/integer, Nibble7:4/integer, Nibble8:4/integer,
                         Remainder/binary>>) ->
  HexNumbersLine = [Nibble1, Nibble2, Nibble3, Nibble4, Nibble5, Nibble6, Nibble7, Nibble8],
  HexLettersLine = lists:map(fun(X) -> hex_number_to_hex_letter(X) end, HexNumbersLine),
  io:format("    0x~c~c~c~c~c~c~c~c~n", HexLettersLine),
  do_print_binary_as_hex(Remainder);
  

do_print_binary_as_hex( << >> ) -> ok.


%% Converts a number between 0-15 to the appropriate ASCII hex chararacter (0123456789abcdef)
hex_number_to_hex_letter(HexNumber) ->
    case HexNumber of
        0  -> 48;   % "0"
        1  -> 49;  
        2  -> 50;
        3  -> 51;
        4  -> 52;
        5  -> 53;
        6  -> 54;
        7  -> 55;
        8  -> 56;
        9  -> 57;   % "9"
        10 -> 97;   % "a"
        11 -> 98;
        12 -> 99;
        13 -> 100;
        14 -> 101;
        15 -> 102   % "f"
    end.
