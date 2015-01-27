-module(ue_node_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit_fsm/include/eunit_fsm.hrl").

%% fsm_tradepost_test_() ->
%%     {setup,
%%      fun ()  -> start_tp()end,
%%      fun (_) -> stop_tp() end,

-define('RA-RNTI', 42).
-define('IMSI', 208640000000001).

ue_node_test_() ->
    {timeout, 30,
     {setup, 
      fun setup/0,
      fun teardown/1,
      [{"UE startup", fun logic/0}]}}.

logic() ->
    ok = application:start(ue_node),
    ?assertEqual(ok, ok),
    UeId = ue_node:register_ue(?'IMSI', ?'RA-RNTI'),
    ok.

setup() ->
    error_logger:tty(false),
    {ok, Deps} = application:ensure_all_started(lager),
    Deps.


teardown(_) ->  
    application:stop(ue_node),
    ok = application:stop(lager).
