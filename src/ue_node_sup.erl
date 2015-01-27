
-module(ue_node_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok,
     {{one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% Logical SRB CH supervisor
       {   ue_srb_sup,
           {ue_srb_sup,start_link, []},
           permanent,                               % Restart  = permanent | transient | temporary
           infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
           supervisor,                              % Type     = worker | supervisor
           []                                       % Modules  = [Module] | dynamic
       },
       %% Logical DRB CH supervisor
       {   ue_drb_sup,
           {ue_drb_sup,start_link, []},
           permanent,                               % Restart  = permanent | transient | temporary
           infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
           supervisor,                              % Type     = worker | supervisor
           []                                       % Modules  = [Module] | dynamic
       },
       %% RRC instance
       {   ue_node_sup,                          % Id       = internal id
           {ue_rrc,start_link,[]}, % StartFun = {M, F, A}
           permanent,                               % Restart  = permanent | transient | temporary
           2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
           worker,                                  % Type     = worker | supervisor
           [ue_rrc]                           % Modules  = [Module] | dynamic
       }
      ]
     }
    }.
