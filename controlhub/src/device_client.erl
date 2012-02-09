%%% -------------------------------------------------------------------
%%% Author  : rob
%%% Description :
%%%
%%% Created : Dec 20, 2010
%%% -------------------------------------------------------------------
-module(device_client).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("global_constants.hrl").
-include("trace_macro.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/3, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket = null}).


%% ====================================================================
%% External functions
%% ====================================================================

% Starts up a device client process and returns its process ID or an error reason.
%% @spec start( DeviceId::integer(), IPaddress::string(), Port::integer() ) -> {ok, Pid} | {error, Error} 
start(DeviceID, IPaddress, Port) when is_integer(DeviceID), is_list(IPaddress), is_integer(Port) ->
    gen_server:start(?MODULE, [DeviceID, IPaddress, Port], []).


% An asynchronous request to sends some instructions to a device. The response is returned to the calling
% seperately - i.e. not through the return value of this function.
send(DeviceID, Instructions) when is_integer(DeviceID),
                                  is_binary(Instructions) ->
    case device_client_index:get_pid(DeviceID) of
        undefined ->
            %TODO: Deal with this error properly
            io:format("Ignoring send request to non-existant DeviceID = ~p~n", [DeviceID]),
            unknown_device_id;
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, {send, Instructions, self()})
    end.
    
    

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([DeviceID, IPaddress, Port]) ->
    % These three are all "process constants", and hence OK to use process dict.
    put(deviceID, DeviceID),
    put(deviceIP, IPaddress),
    put(devicePort, Port),
    
    % Try opening ephemeral port and we want data delivered as a binary.
    case gen_udp:open(0, [binary]) of   
        {ok, Socket} -> {ok, #state{socket = Socket}};
        {error, Reason} when is_atom(Reason) ->
            ErrorMessage = "Couldn't open UDP socket for device ID '" ++ integer_to_list(DeviceID) ++
                           "'. Posix error code is: " ++ atom_to_list(Reason) ++ ".",
            {stop, ErrorMessage};
        _ ->
            ErrorMessage = "Couldn't open UDP socket for device ID '" ++ integer_to_list(DeviceID) ++
                           "'. Cause unknown - received unexpected error response.",
            {stop, ErrorMessage}
    end.
    
    

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({send, Instructions, ClientPid}, State = #state{socket = Socket}) ->
    ?DEBUG_TRACE("Instructions received from Transaction Manager with PID = ~p.  Forwarding to DeviceID = ~p...", [ClientPid, get(deviceID)]),
    packet_stats:udp_out(),
    ok = gen_udp:send(Socket, get(deviceIP), get(devicePort), Instructions),
    Reply = receive
                {udp, Socket, _, _, HardwareReplyBin} -> 
                    ?DEBUG_TRACE("Received response from DeviceID = ~p. Passing it to originating Transaction Manager...", [get(deviceID)]),
                    packet_stats:udp_in(),
                    % Now prepend the deviceID to the front of the HardwareReplyBin
                    CompleteResponse = list_to_binary([<<(get(deviceID)):32>>, HardwareReplyBin]),
                    {device_client_response, get(deviceID), ok, CompleteResponse}
            after ?DEVICE_CLIENT_UDP_TIMEOUT ->
                ?DEBUG_TRACE("TIMEOUT REACHED! No response from DeviceID = ~p. Generating and sending a timeout "
                             "response to originating Transaction Manager...", [get(deviceID)]),
                packet_stats:udp_response_timeout(),
                TimeoutResponseBin = <<
                                       (get(deviceID)):32,
                                       16#00000000:32
                                     >>,
                {device_client_response, get(deviceID), udp_response_timeout, TimeoutResponseBin}
            end,
    ClientPid ! Reply,
    {noreply, State};


handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

