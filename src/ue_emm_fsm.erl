-module(ue_emm_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         attach/2
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
    'EMM-DEREGISTERED'/2,
    'EMM-REGISTERED'/2
]).

-record(state, {
          ue_id :: integer(), % client socket
          enb_id :: integer() % client socket
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

attach(Pid, EnbId) when is_pid(Pid), is_integer(EnbId) ->
    gen_fsm:send_event(Pid, {attach, EnbId}).

start_link(UeId) ->
    gen_fsm:start_link({local, name(?SERVER,UeId)}, ?MODULE, [UeId], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([UeId]) ->
    ue_node:register(UeId, ue_emm_fsm, self()),
    {ok, 'EMM-DEREGISTERED', #state{ue_id=UeId}}.

'EMM-DEREGISTERED'(_Event, State) ->
    ?ERROR("EMM-DEREGISTERED ~p", [_Event]),
    {next_state, 'EMM-DEREGISTERED', State}.

'EMM-REGISTERED'(_Event, State) ->
    ?ERROR("EMM-REGISTERED ~p", [_Event]),
    {next_state, 'EMM-REGISTERED', State}.

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
