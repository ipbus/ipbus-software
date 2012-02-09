%% Author: rob
%% Created: Dec 13, 2010
%% Description: TODO: Add description to device_address_file_reader
-module(device_address_file_reader).


%%
%% Include files
%%



%%
%% Exported Functions
%%
-export([read/1]).

%%
%% API Functions
%%

% Returns a list of devices in the system, where each device is described by a tuple containing the following
% three items: the device ID number, the device's IP address (as a string), and the device's receiving port number.
% If an error occurs whilst reading the file, then an error message is printed and erlang:exit is called.
% DeviceAddrFile is simply an erlang list of each device in a text/erl file, and should look something like this:
%
%     %% Provide a Device ID number, an IP Address in quotes, and a port number.
%     [
%       {1, "192.168.1.1", 791 },
%       {2, "192.168.1.2", 791 },
%       {3, "192.168.1.3", 791 }
%     ].
%
%
%%  @spec read(DeviceAddrFile) -> [ {DeviceID, IPAddr::string(), PortNumber}, ... ] | erlang:exit
read(DeviceAddrFile) when is_list(DeviceAddrFile) ->
   put(deviceAddrFile, DeviceAddrFile),  % Store the device address filename for future printouts
   Devices = checkFileContent(file:consult(DeviceAddrFile)),
   % At this point, we know we have correctly formatted device information
   io:format("The following devices have been declared:~n"),
   lists:map(fun prettyPrintDevice/1, Devices),  
   Devices.  % Return the correctly formatted list of devices.






%%
%% Local Functions
%%

%% Device address file content checks... this should get run if all is well
checkFileContent({ok, [Devices]}) when is_list(Devices) ->
    readDeclaredDevices(Devices),
    Devices;

%% Device address file content checks... this should get run if an ~expected type of error occurs.
checkFileContent({error, Reason}) ->
    PrettyReason = file:format_error(Reason),
    io:format("Error reading from device address file ~p; reason: ~p~n", [get(deviceAddrFile), PrettyReason]),
    erlang:exit({deviceAddressFileError, PrettyReason});

%% Device address file content checks... this should get run if a completely weird error occurs.
checkFileContent(_) ->
    io:format("An unknown error occurred whilst reading from device address file ~p", [get(deviceAddrFile)]),
    erlang:exit({deviceAddressFileError, "unknown error"}).


%% Read in the individual device-declaration tuples from a list.
readDeclaredDevices([H|T]) ->
    readDevice(H),
    readDeclaredDevices(T);
    
readDeclaredDevices([]) -> ok.


%% Checks that a device-declaration tuple is of the correct form 
readDevice({DeviceID, IpAddr, PortNum}) when is_integer(DeviceID),
                                           is_list(IpAddr),
                                           is_integer(PortNum) -> ok;

%% If a device-declaration tuple is not of form {DeviceID, IpAddr, PortNum}, this func throws an exception.
readDevice(_) ->
    io:format("Devices not properly declared/formated in device address file ~p!~n", [get(deviceAddrFile)]),
    erlang:exit({deviceAddressFileError, "file not properly formated"}).


%% Pretty prints device details to console
prettyPrintDevice({DeviceID, IpAddr, PortNum}) ->
    io:format("\tDeviceID = ~p, IP Address = ~p, Port Number = ~p~n", [DeviceID, IpAddr, PortNum]).
