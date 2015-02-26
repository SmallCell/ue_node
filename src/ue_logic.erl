-module(ue_logic).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         emm_attach/2,
         rrc_connect/2,
         emc_connect/2
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

-spec emm_attach(integer(), integer()) -> any().
emm_attach(UeId, EnbId) ->
    gen_server:call(ue_node:lookup(UeId, ue_logic), {emm_attach, EnbId}).

-spec rrc_connect(integer(), integer()) -> any().
rrc_connect(UeId, EnbId) ->
    gen_server:call(ue_node:lookup(UeId, ue_logic), {rrc_connect, EnbId}).

-spec emc_connect(integer(), integer()) -> any().
emc_connect(UeId, EnbId) ->
    gen_server:call(ue_node:lookup(UeId, ue_logic), {emc_connect, EnbId}).


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

handle_call({rrc_connect, EnbId},  _From, #state{ue_id=UeId} = State) ->
    Pid = ue_node:lookup(UeId, ue_rrc_fsm),
    {ok, NASMessage, ESMMessage} = ue_rrc_fsm:connect(Pid, EnbId),
    {reply, {ok, NASMessage, ESMMessage}, State}; % FIXME: update state with parameters
handle_call({emc_connect, EnbId},  _From, #state{ue_id=UeId} = State) ->
    Pid = ue_node:lookup(UeId, ue_emc_fsm),
    {ok, NASMessage, ESMMessage} = ue_rrc_fsm:connect(Pid, EnbId),
    {reply, {ok, NASMessage, ESMMessage}, State}; % FIXME: update state with parameters
handle_call({mme_attach, EnbId},  _From, #state{ue_id=UeId} = State) ->
    Pid = ue_node:lookup(UeId, ue_emm_fsm),
    {ok, IP} = ue_emm_fsm:attach(Pid, EnbId),
    {reply, {ok, IP}, State}; % FIXME: update state with parameters
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

