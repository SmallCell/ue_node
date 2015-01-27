-module(ue_srb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_srb_handler/1, stop_srb_handler/1]).

%% Supervisor callbacks
-export([init/1]).


-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).


%% ===================================================================
%% API functions
%% ===================================================================
start_srb_handler(LCID) ->
    supervisor:start_child (?MODULE,
                            {LCID,           % Id  = internal id
                             {ue_srb,start_link,[LCID]}, % StartFun = {M, F, A}
                             temporary,       % Restart  = permanent | transient | temporary
                             2000,            % Shutdown = brutal_kill | int() >= 0 | infinity
                             worker,          % Type     = worker | supervisor
                             [ue_srb]         % Modules  = [Module] | dynamic
    }).

stop_srb_handler(LCID) ->
    supervisor:terminate_child (?MODULE, LCID),
    supervisor:delete_child (?MODULE, LCID).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->    
    {ok,
     {{one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
      ]
     }
    }.

