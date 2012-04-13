%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since April 2012
%%%
%%% @doc Control Hub trace macros. Comment/uncomment and recompile to
%%%      deactivate/active as appropriate.  One of the macros (DEBUG_TRACE) is
%%%      for simple text-based debug messages, and a second (PACKET_TRACE) is 
%%%      for printing out packet binaries in hex form.
%%% @end
%%% ===========================================================================



%% Uncomment line below to globally switch on the simple debug trace messages.
%-define(debug_trace, true).

%% Uncomment line below to globally switch on packet trace messages.
%-define(packet_trace, true).





%% ----------------------------------------------------------------------------
%% @doc Debug Trace Macro - for simple text trace messages.
%%      It works in a similar fashion to io:format/1 and io:format/2.
%%      To print a simple trace message use as follows:
%%        DEBUG_TRACE("My message")
%%      or something like:
%%        DEBUG_TRACE("Debug Value 1 = ~p, Debug Value 2 = ~p.", [DebugValue1, DebugValue2])
%% 
%%      Either enable the macro globally (see top of this file) or...
%%      Compile module with c(module_name, {d, debug_trace}) to activate debug
%%      trace messages for that module.
%% ----------------------------------------------------------------------------
-ifdef(debug_trace).

-define(DEBUG_TRACE(MESSAGE),
        io:format(lists:append(lists:append("DEBUG_TRACE : PID=~p : ~p : ", MESSAGE), "~n"), [self(), ?MODULE])
       ).

-define(DEBUG_TRACE(MESSAGE, MESSAGE_DATA),
        io:format(lists:append(lists:append("DEBUG_TRACE : PID=~p : ~p : ", MESSAGE), "~n"), [self(), ?MODULE | MESSAGE_DATA])
       ).

-else.

-define(DEBUG_TRACE(MESSAGE), void).
-define(DEBUG_TRACE(MESSAGE, MESSAGE_DATA), void).

-endif.


%% ----------------------------------------------------------------------------
%% @doc Packet Trace Macro - for printing packets in hex form for debug purposes.
%%      Use as follows:
%%        PACKET_TRACE(MyPacketBinary, "My message")
%%      or something like:
%%        PACKET_TRACE(MyPacketBinary, "Debug Value 1 = ~p, Debug Value 2 = ~p.", [DebugValue1, DebugValue2])
%%       The message will be printed followed by a printout of MyPacketBinary.
%%
%%      Either enable the macro globally (see top of this file) or...
%%      Compile module with c(module_name, {d, packet_trace}) to activate packet
%%      trace messages for that module.
%% ----------------------------------------------------------------------------
-ifdef(packet_trace).

-define(PACKET_TRACE(PACKET_BIN, MESSAGE),
        io:format(lists:append(lists:append("PACKET_TRACE : PID=~p : ~p : ", MESSAGE), "~n"), [self(), ?MODULE]),
        utils:print_binary_as_hex(PACKET_BIN)
       ).

-define(PACKET_TRACE(PACKET_BIN, MESSAGE, MESSAGE_DATA),
        io:format(lists:append(lists:append("PACKET_TRACE : PID=~p : ~p : ", MESSAGE), "~n"), [self(), ?MODULE | MESSAGE_DATA]),        
        utils:print_binary_as_hex(PACKET_BIN)
       ).       

-else.

-define(PACKET_TRACE(PACKET_BIN, MESSAGE), void).
-define(PACKET_TRACE(PACKET_BIN, MESSAGE, MESSAGE_DATA), void).

-endif.

