
-module(ue_node_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(UeId) ->
    supervisor:start_link({local, name(?MODULE,UeId)}, ?MODULE, [UeId]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([UeId]) ->
    ue_node:create(UeId),
    ue_node:register(UeId, ue_node_sup, self()),

    {ok,
     {{one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% Logical Channel supervisor
       {   ue_ch_sup,
           {ue_ch_sup,start_link, [UeId]},
           permanent,                               % Restart  = permanent | transient | temporary
           infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
           supervisor,                              % Type     = worker | supervisor
           []                                       % Modules  = [Module] | dynamic
       },
       %% RRC FSM instance
       {   ue_rrc,                          % Id       = internal id
           {ue_rrc,start_link,[UeId]}, % StartFun = {M, F, A}
           permanent,                               % Restart  = permanent | transient | temporary
           2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
           worker,                                  % Type     = worker | supervisor
           [ue_rrc]                           % Modules  = [Module] | dynamic
       }
      ]
     }
    }.

%%------------------------------------------------------------------------------
%% Local helpers
%%------------------------------------------------------------------------------

name(Module, Rb) ->
    list_to_atom(atom_to_list(Module) ++ "_" ++ integer_to_list(Rb)).
