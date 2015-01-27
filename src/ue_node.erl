-module(ue_node).

-behaviour(application).

%% API
-export([register_ue/2]).


%% Application callbacks
-export([start/2, stop/1]).


-include_lib("lte_model/include/x-small-cell-ue-1-0-dt.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ue_node_sup:start_link().

stop(_State) ->
    ok.
 

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% @doc Send message out to controllers.

register_ue(Imsi, RaRnti) ->
    self().
 
