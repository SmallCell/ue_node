-module(ue_srb).
-behaviour(gen_server).

-define(CLIENT, ?MODULE).

-define(SIB_PORT_BASE, 8000).
-define(UE_HOST, {127,0,0,1}).
-define(ENB_HOST, {127,0,0,1}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3, 
         send/3
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include_lib("kernel/include/inet.hrl").
-include_lib("lte_model/include/node_logger.hrl").


-record(state, {
          ueid  :: non_neg_integer(),
          owner :: pid(),
          rb   :: non_neg_integer(),
          socket :: gen_udp:sctp_socket()       % Listening socket
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link(integer(), integer(), pid()) -> {ok, pid()} |
                                                 ignore |
                                                 {error, term()}.
start_link(RB, UeId, Owner) when RB >= 0, RB < 4 ->
    Id = list_to_atom("ue_srb_" ++ integer_to_list(RB)),
    gen_server:start_link({local, Id}, ?MODULE, [UeId, RB, Owner], []).

-spec send(integer(), integer(), binary()) -> any().
send(Pid, EnbId, Data) when is_pid(Pid), is_number(EnbId)->
    gen_server:cast(Pid, {send, EnbId, Data}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([UeId, RB, Owner]) ->
    %%W/O process_flag(trap_exit, true),
    UePort = ?SIB_PORT_BASE + 100 * UeId + RB,
    ?INFO("UE ~p Allocate SRB ~p on port: ~p", [UeId,RB,UePort]),
    Opts = [{active, once}, {mode, binary}],
    case gen_udp:open(UePort, Opts) of
        {ok, Socket} ->
            ok = gen_udp:controlling_process(Socket, self()),
            {ok, #state{ueid=UeId,owner=Owner,rb=RB,
                        socket=Socket}};
        {error, Reason} ->
            {stop, Reason}               
    end.


%% handle_call({set_data_hnd, _Pid}, _From, State) ->
%%     io:format(user, ">> state ~p", [State]),
%%     {reply, {error, already_handled}, State};

% fall trough
handle_call(Request, _From, State) ->
    ?ERROR(">> ue_srb_ ~p, unknown req: ~p", [self(), Request]),
    {reply, ok, State}.

handle_cast({send, _EnbId, Data}, State=#state{socket=S,rb=RB}) ->
    EnbPort = ?SIB_PORT_BASE + RB,
    ?INFO(">> UE sending on: ~p",[EnbPort]),
    gen_udp:send(S, ?ENB_HOST, EnbPort, Data),
    {noreply, State};

handle_cast(Msg, State) ->
    ?ERROR("ue_srb:handle_cast '~p'", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?ERROR("ue_srb:handle_info '~p'", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

 
