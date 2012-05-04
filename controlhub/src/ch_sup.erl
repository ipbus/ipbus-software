%%% ===========================================================================
%%% @author Robert Frazier
%%%
%%% @since May 2012
%%%
%%% @doc The root supervisor for the Control Hub application.
%%% @end
%%% ===========================================================================
-module(ch_sup).

-behaviour(supervisor).


%% API exports
-export([]).

%% Behavioural exports
-export([init/1]).


%%% ====================================================================
%%% API functions (public interface)
%%% ====================================================================



%%% ====================================================================
%%% Behavioural externals
%%% ====================================================================

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
    AChild = {'AName',{'AModule',start_link,[]},
	      permanent,2000,worker,['AModule']},
    {ok,{{one_for_all,0,1}, [AChild]}}.


