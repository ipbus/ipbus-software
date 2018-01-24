%%% ===========================================================================
%%% @author Robert Frazier
%%% @author Tom Williams
%%%
%%% @since April 2012
%%%
%%% @doc Control Hub logging macros - to remove low-level frequent log messages
%%%      at compile time (for performance reasons). Comment/uncomment and recompile to
%%%      deactivate/active as appropriate.  One of the macros (DEBUG_TRACE) is
%%%      for simple text-based debug messages, and a second (PACKET_TRACE) is 
%%%      for printing out packet binaries in hex form.
%%% @end
%%% ===========================================================================



%% Uncomment line below to globally switch on the simple debug trace messages.
%-define(log_debug, true).

%% Uncomment line below to globally switch on packet trace messages.
%-define(packet_trace, true).




%% ----------------------------------------------------------------------------
%% @doc Debug Trace Macro - for simple text trace messages.
%%      It works in a similar fashion to io:format/1 and io:format/2.
%%      To print a simple trace message use as follows:
%%        DEBUG_TRACE("My message")
%%      or something like:
%%        DEBUG_TRACE("Debug Value 1 = ~p, Debug Value 2 = ~p.", [DebugValue1, DebugValue2])
%%      or even:
%%        DEBUG_TRACE("Short prefix to log msg", "My cool log message, debug value = ~p.", [DebugValue])
%%      Either enable the macro globally (see top of this file) or...
%%      Compile module with c(module_name, {d, log_debug}) to activate debug
%%      trace messages for that module.
%% ----------------------------------------------------------------------------
-ifdef(log_debug).

-define( CH_LOG_DEBUG(MESSAGE), ch_utils:log(debug, MESSAGE, []) ).
-define( CH_LOG_DEBUG(MESSAGE, MESSAGE_DATA), ch_utils:log(debug, MESSAGE, MESSAGE_DATA) ).
-define( CH_LOG_DEBUG(PREFIX, MESSAGE, MESSAGE_DATA), ch_utils:log({debug, PREFIX}, MESSAGE, MESSAGE_DATA) ).

-else.

-define( CH_LOG_DEBUG(MESSAGE), void ).
-define( CH_LOG_DEBUG(MESSAGE, MESSAGE_DATA), void ).
-define( CH_LOG_DEBUG(PREFIX, MESSAGE, MESSAGE_DATA), void ).

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
        io:format(lists:append(lists:append("PACKET_TRACE @ ~p: PID=~p : ~p : ", MESSAGE), "~n"), [now(), self(), ?MODULE]),
        ch_utils:print_binary_as_hex(PACKET_BIN)
       ).

-define(PACKET_TRACE(PACKET_BIN, MESSAGE, MESSAGE_DATA),
        io:format(lists:append(lists:append("PACKET_TRACE @ ~p: PID=~p : ~p : ", MESSAGE), "~n"), [now(), self(), ?MODULE | MESSAGE_DATA]),        
        ch_utils:print_binary_as_hex(PACKET_BIN)
       ).       

-else.

-define(PACKET_TRACE(PACKET_BIN, MESSAGE), void).
-define(PACKET_TRACE(PACKET_BIN, MESSAGE, MESSAGE_DATA), void).

-endif.

