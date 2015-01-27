-module(ue_drb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_drb_handler/1, stop_drb_handler/1]).

%% Supervisor callbacks
-export([init/1]).


-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).


%% ===================================================================
%% API functions
%% ===================================================================
start_drb_handler(LCID) ->
    supervisor:start_child (?MODULE,
                            {LCID,           % Id  = internal id
                             {ue_drb,start_link,[LCID]}, % StartFun = {M, F, A}
                             temporary,       % Restart  = permanent | transient | temporary
                             2000,            % Shutdown = brutal_kill | int() >= 0 | infinity
                             worker,          % Type     = worker | supervisor
                             [ue_drb]         % Modules  = [Module] | dynamic
    }).

stop_drb_handler(LCID) ->
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

