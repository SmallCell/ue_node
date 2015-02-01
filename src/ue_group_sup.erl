-module(ue_group_sup).
 
-behaviour(supervisor).

%% API
-export([start_link/0,
         start_ue_node/1,
         stop_ue_node/1]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("lte_model/include/node_logger.hrl").

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    {ok, UeGroup} = application:get_env(ue_node, ue_group),

    ?INFO("sys.config: ~p", [UeGroup]),
    [start_ue_node(Id) || {ue, Id} <- UeGroup],
    {ok, Pid}.


start_ue_node(UeId) ->
    supervisor:start_child (?MODULE,
                            {UeId,           % Id  = internal id
                             {ue_node_sup,start_link,[UeId]}, % StartFun = {M, F, A}
                             temporary,       % Restart  = permanent | transient | temporary
                             2000,            % Shutdown = brutal_kill | int() >= 0 | infinity
                             worker,          % Type     = worker | supervisor
                             [ue_drb]         % Modules  = [Module] | dynamic
    }).

stop_ue_node(UeId) ->
    supervisor:terminate_child (?MODULE, UeId),
    supervisor:delete_child (?MODULE, UeId).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->    
    {ok,{{one_for_one, ?MAX_RESTART, ?MAX_TIME},[]}}.

