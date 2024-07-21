%%% -------------------------------------------------------------------
%%% Author  : zzl
%%% Description :
%%%
%%% Created : 2010-5-5
%%% -------------------------------------------------------------------
-module(mod_team_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("mgeew.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
		 start_link/0,
		 init/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================


start_link() ->
	supervisor:start_link({global, atom_to_list(?MODULE)}, ?MODULE, []).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
    %% {ok,{{simple_one_for_one,10,10}, 
    %%      [{mod_team, 
    %%        {mod_team, start_link, []}, 
    %%        transient, brutal_kill, worker, [mod_team]}
    %%      ]}}.
   RestartStrategy = one_for_one,
   MaxRestarts = 1000,
   MaxSecondsBetweenRestarts = 3600,

   SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

   %% Restart = permanent,
   %% Shutdown = 2000,
   %% Type = worker,

   %% AChild = {'AName', {'AModule', start_link, []},
   %%           Restart, Shutdown, Type, ['AModule']},

   {ok, {SupFlags, []}}.
%% ====================================================================
%% Internal functions
%% ====================================================================
