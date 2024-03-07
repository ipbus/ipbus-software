%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since May 2012
%%%
%%% @doc Application metadata. List of modules, the "starting point" module
%%%      (controlhub_app), and application dependencies, etc, etc.  This file
%%%      is an OTP requirement.
%%% @end
%%% ===========================================================================
{application, controlhub,
 [{description, "Control Hub: multi-client packet routing for IPbus/UDP hardware"},
   {vsn, "2.8.13"},
   {modules, [controlhub_app,
              ch_config,
              ch_sup,
              ch_device_client_registry,
              ch_device_client,
              ch_stats,
              ch_tcp_listener,
              ch_transaction_manager,
              ch_utils]},
   {registered, [ch_device_client_registry,
                 ch_stats,
                 ch_sup,
                 ch_tcp_listener]},
   {applications, [stdlib, kernel, sasl,
                   observer,
                   % For lager syslog backend
                   syslog, lager_syslog,
                   % For lager, which depends on goldrush
                   syntax_tools, compiler, goldrush, lager]},
   {mod, {controlhub_app, []}},
   {env, [% The port on which the Control Hub will listen for TCP connections.
          {tcp_listen_port, 10203},
          % The maximum number of UDP packets in flight to any given target.
          {max_udp_in_flight, 16},
          % The timeout (ms) used when waiting for a response from a hardware target.
          {device_response_timeout, 20},
          % The time (ms) after which a device client process will shut down if not communicating with board
          {device_client_shutdown_after, 15000},
          % Device allowlist: File path and mode
          {device_allowlist_mode, enforcing},
          {device_allowlist_file, none}]}
 ]
}.
