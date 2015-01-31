-module(ue_node).

-behaviour(application).



%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([create/1,
         delete/1,
         register/3,
         lookup/2
        ]).

-include_lib("lte_model/include/x-small-cell-ue-1-0-dt.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ue_node_sup:start_link(0).                  %FIXME: single UE

stop(_State) ->
    ok.
 

%%------------------------------------------------------------------------------
%% Common UE helper functions
%%------------------------------------------------------------------------------
-spec create(integer()) -> ets:tid().
create(UeId) ->
    ets:new(name(UeId), [named_table, public,
                             {read_concurrency, true}]).

-spec delete(integer()) -> true.
delete(UeId) ->
    true = ets:delete(name(UeId)).

-spec register(integer(), atom(), pid() | ets:tid()) -> true.
register(UeId, Name, Pid) ->
    true = ets:insert(name(UeId), {Name, Pid}).

-spec lookup(integer(), atom()) -> term().
lookup(UeId, Name) ->
    case ets:lookup(name(UeId), Name) of
        [{Name, Pid}] ->
            Pid;
        [] ->
            undefined
    end.

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Local helpers
%%------------------------------------------------------------------------------

name(UeId) ->
    list_to_atom("ue_" ++ integer_to_list(UeId)).
