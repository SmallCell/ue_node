-module(ue_ch_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, 
         start_srb_handler/2, 
         stop_srb_handler/1, 
         start_drb_handler/2, 
         stop_drb_handler/1]).

%% Supervisor callbacks
-export([init/1]).


-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).


%% ===================================================================
%% API functions
%% ===================================================================
start_srb_handler(UeId,RB) ->
    supervisor:start_child (?MODULE,
                            {name("srb", RB),           % Id  = internal id
                             {ue_srb,start_link,[UeId, RB]}, % StartFun = {M, F, A}
                             temporary,       % Restart  = permanent | transient | temporary
                             2000,            % Shutdown = brutal_kill | int() >= 0 | infinity
                             worker,          % Type     = worker | supervisor
                             [ue_srb]         % Modules  = [Module] | dynamic
    }).

stop_srb_handler(RB) ->
    Child = name("srb", RB),
    supervisor:terminate_child (?MODULE, Child),
    supervisor:delete_child (?MODULE, Child).

start_drb_handler(UeId,RB) ->
    supervisor:start_child (?MODULE,
                            {name("srb", RB), % Id  = internal id
                             {ue_drb,start_link,[UeId, RB]}, % StartFun = {M, F, A}
                             temporary,       % Restart  = permanent | transient | temporary
                             2000,            % Shutdown = brutal_kill | int() >= 0 | infinity
                             worker,          % Type     = worker | supervisor
                             [ue_drb]         % Modules  = [Module] | dynamic
    }).

stop_drb_handler(RB) ->
    Child = name("drb", RB),
    supervisor:terminate_child (?MODULE, Child),
    supervisor:delete_child (?MODULE, Child).

start_link(UeId) ->
    supervisor:start_link({local, name(atom_to_list(?MODULE),UeId)}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->    
    {ok,{{one_for_one, ?MAX_RESTART, ?MAX_TIME},[]}}.

%%------------------------------------------------------------------------------
%% Local helpers
%%------------------------------------------------------------------------------

name(Ch, Rb) ->
    list_to_atom(Ch ++ "_" ++ integer_to_list(Rb)).
