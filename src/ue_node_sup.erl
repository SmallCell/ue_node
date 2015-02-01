
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
       %% Ue Logic
       {   ue_logic,                          % Id       = internal id
           {ue_logic,start_link,[UeId]}, % StartFun = {M, F, A}
           permanent,                               % Restart  = permanent | transient | temporary
           2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
           worker,                                  % Type     = worker | supervisor
           [ue_logic]                           % Modules  = [Module] | dynamic
       },
       %% RRC FSM instance
       {   ue_rrc_fsm,                          % Id       = internal id
           {ue_rrc_fsm,start_link,[UeId]}, % StartFun = {M, F, A}
           permanent,                               % Restart  = permanent | transient | temporary
           2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
           worker,                                  % Type     = worker | supervisor
           [ue_rrc_fsm]                           % Modules  = [Module] | dynamic
       },
       %% ECM FSM instance
       {   ue_ecm_fsm,                          % Id       = internal id
           {ue_ecm_fsm,start_link,[UeId]}, % StartFun = {M, F, A}
           permanent,                               % Restart  = permanent | transient | temporary
           2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
           worker,                                  % Type     = worker | supervisor
           [ue_ecm_fsm]                           % Modules  = [Module] | dynamic
       },
       %% EMM FSM instance
       {   ue_emm_fsm,                          % Id       = internal id
           {ue_emm_fsm,start_link,[UeId]}, % StartFun = {M, F, A}
           permanent,                               % Restart  = permanent | transient | temporary
           2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
           worker,                                  % Type     = worker | supervisor
           [ue_emm_fsm]                           % Modules  = [Module] | dynamic
       }
      ]
     }
    }.

%%------------------------------------------------------------------------------
%% Local helpers
%%------------------------------------------------------------------------------

name(Module, Rb) ->
    list_to_atom(atom_to_list(Module) ++ "_" ++ integer_to_list(Rb)).
