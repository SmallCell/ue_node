-module(ue_srb).
-behaviour(gen_server).

-define(CLIENT, ?MODULE).

-define(SIB_PORT_BASE, 8000).
-define(UE_HOST, {127,0,0,1}).
-define(ENB_HOST, {127,0,0,1}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, send/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include_lib("kernel/include/inet.hrl").
-include_lib("lte_model/include/node_logger.hrl").


-record(state, {
          socket :: gen_udp:sctp_socket(),       % Listening socket
          port   :: inet:port(),
          rb   :: non_neg_integer()       % FSM handling module
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(RB) when RB >= 0, RB < 4 ->
    Id = list_to_atom("ue_srb_" ++ integer_to_list(RB)),
    gen_server:start_link({local, Id}, ?MODULE, [RB], []).

send(Ch, Data) ->
    gen_server:cast(Ch, {send, Data}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([RB]) ->
    %%W/O process_flag(trap_exit, true),
    ?INFO("UE Allocate SRB~p", [RB]),
    Opts = [{active, once}, {mode, binary}],
    UePort = ?SIB_PORT_BASE + RB,
    case gen_udp:open(0, Opts) of
        {ok, Socket} ->
            ok = gen_udp:controlling_process(Socket, self()),
            {ok, #state{socket=Socket,
                        port = UePort,
                        rb=RB}};
        {error, Reason} ->
            {stop, Reason}               
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({send, Data}, State=#state{socket=S,port=P}) ->
    gen_udp:send(S, ?ENB_HOST, P, Data),
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

 
