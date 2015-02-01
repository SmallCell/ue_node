-module(ue_logic).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         attach/2
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("lte_model/include/node_logger.hrl").

-record(state, {
          ue_id :: integer()
         }).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec attach(integer(), integer()) -> any().
attach(UeId, EnbId) ->
    gen_server:cast(ue_node:lookup(UeId, ue_logic),
                    {attach, EnbId}).


-spec start_link(non_neg_integer()) -> {ok, pid()} |
                                       {error, any()}.
start_link(UeId) ->
    gen_server:start_link({local, name(?SERVER,UeId)}, ?MODULE, [UeId], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([UeId]) ->
    ue_node:register(UeId, ue_logic, self()),
    {ok, #state{ue_id=UeId}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({attach, EnbId}, #state{ue_id=UeId} = State) ->
    EMM = ue_node:lookup(UeId, ue_emm_fsm),
    ue_emm_fsm:attach(EMM, EnbId),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Local helpers
%%------------------------------------------------------------------------------

name(Server, UeId) ->
    list_to_atom(atom_to_list(Server) ++ "_" ++ integer_to_list(UeId)).

