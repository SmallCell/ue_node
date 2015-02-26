-module(ue_rrc_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         connect/2
        ]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

-include_lib("lte_model/include/node_logger.hrl").

%% FSM States
-export([
    'RRC_IDLE'/3,
    'RRC_Connected'/3
]).

-record(state, {
          ue_id :: integer(), % client socket
          enb_id :: integer(), % client socket
          srb_0 :: pid(),
          connect_act :: pid()
         }).

%% RRC connection supervision (RCS)
-define(T310, 1000).
-define(T311, 1000).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

connect(Pid, EnbId) when is_pid(Pid), is_integer(EnbId) ->
    gen_fsm:sync_send_event(Pid, {conect, EnbId}).

start_link(UeId) ->
    gen_fsm:start_link({local, name(?SERVER,UeId)}, ?MODULE, [UeId], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([UeId]) ->
    ue_node:register(UeId, ue_rrc_fsm, self()),
    {ok, 'RRC_IDLE', #state{ue_id=UeId}}.

'RRC_IDLE'({conect, EnbId}, From, #state{ue_id=UeId} = State) ->
    ?INFO("RRC_IDLE:connect ~p", [EnbId]),
    {ok, Pid} = ue_ch_sup:start_srb_handler(UeId,0),
    
    {next_state, 'RRC_IDLE', State#state{connect_act = From}};
'RRC_IDLE'(_Event, _From, State) ->
    ?ERROR("RRC_IDLE ~p", [_Event]),
    {next_state, 'RRC_IDLE', State}.

'RRC_Connected'(_Event, _From, State) ->
    ?ERROR("RRC_Connected ~p", [_Event]),
    {next_state, 'RRC_Connected', State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

name(Server, UeId) ->
    list_to_atom(atom_to_list(Server) ++ "_" ++ integer_to_list(UeId)).
