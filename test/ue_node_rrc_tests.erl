-module(ue_node_rrc_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit_fsm/include/eunit_fsm.hrl").

rrc_setup_test_() ->
    {timeout, 30,
     {setup, 
      fun setup/0,
      fun teardown/1,
      [{"LTE attach sequence", fun logic/0}]}}.

logic() ->
    ?assertEqual(ok, nok),

    ok.

setup() ->
    error_logger:tty(false),
    ok = application:start(asn1).


teardown(_) ->
    ok = application:stop(asn1).

