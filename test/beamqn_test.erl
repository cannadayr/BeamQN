-module(beamqn_test).
-include_lib("eunit/include/eunit.hrl").
-define(MAX_TSDIFF,1000).

-define(BEAMQN_TEST_READ_INPUT, [0, 0.0, 1 bsl 59, -(1 bsl 59)-1, 2.0e53, -2.0e53, [], [0,1,2], [0.0,1.0,2.0], [[1,2],3], [[1.0,2.0],3.0]]).
-define(BEAMQN_TEST_EVAL_INPUT, [<<"⊢"/utf8>>]).

make_helper(Arg0) ->
    {Msg0,Ref0,Stats0} = beamqn:make(Arg0,[{tsdiff,true}]),
    #{ tsdiff := TsDiff0 } = Stats0,
    ?assertEqual({ok,true,true},{Msg0,erlang:is_reference(Ref0),TsDiff0<?MAX_TSDIFF}).
make_test() ->
    lists:map(fun make_helper/1,?BEAMQN_TEST_READ_INPUT).

norm(N) when is_integer(N) ->
    float(N);
norm(N) when is_float(N) ->
    N;
norm(L) when is_list(L) ->
    lists:map(fun norm/1,L).
make_and_read_helper(Arg0) ->
    {Msg0,Ref0,Stats0} = beamqn:make(Arg0,[{tsdiff,true}]),
    #{ tsdiff := TsDiff0 } = Stats0,
    ?assertEqual({ok,true,true},{Msg0,erlang:is_reference(Ref0),TsDiff0<?MAX_TSDIFF}),
    {Msg1,Rtn,Stats1} = beamqn:read(Ref0,[{tsdiff,true}]),
    #{ tsdiff := TsDiff1 } = Stats1,
    ?assertEqual({ok,true,true},{Msg1,norm(Arg0) =:= Rtn,TsDiff1<?MAX_TSDIFF}).
make_and_read_test() ->
    lists:map(fun make_and_read_helper/1,?BEAMQN_TEST_READ_INPUT).

eval_helper(Arg0) ->
    {Msg,Ref,Stats} = beamqn:eval(Arg0,[{tsdiff,true}]),
    #{ tsdiff := TsDiff0 } = Stats,
    ?assertEqual({ok,true,true},{Msg,erlang:is_reference(Ref),TsDiff0<?MAX_TSDIFF}).
eval_test() ->
    lists:map(fun eval_helper/1,?BEAMQN_TEST_EVAL_INPUT).

eval_make_call_read_helper({Arg0, Arg1}) ->
    {Msg0,Ref0,Stats0} = beamqn:eval(Arg0,[{tsdiff,true}]),
    #{ tsdiff := TsDiff0 } = Stats0,
    ?assertEqual({ok,true,true},{Msg0,erlang:is_reference(Ref0),TsDiff0<?MAX_TSDIFF}),
    {Msg1,Ref1,Stats1} = beamqn:make(Arg1,[{tsdiff,true}]),
    #{ tsdiff := TsDiff1 } = Stats1,
    ?assertEqual({ok,true,true},{Msg1,erlang:is_reference(Ref1),TsDiff1<?MAX_TSDIFF}),
    {Msg2,Ref2,Stats2} = beamqn:call(Ref0,Ref1,[{tsdiff,true}]),
    #{ tsdiff := TsDiff2 } = Stats2,
    ?assertEqual({ok,true,true},{Msg2,erlang:is_reference(Ref2),TsDiff2<?MAX_TSDIFF}),
    {Msg3,Rtn,Stats3} = beamqn:read(Ref2,[{tsdiff,true}]),
    #{ tsdiff := TsDiff3 } = Stats3,
    ?assertEqual({ok,true,true},{Msg3,norm(Arg1) =:= Rtn,TsDiff3<?MAX_TSDIFF}).
eval_make_call_read_test() ->
    lists:map(fun eval_make_call_read_helper/1,[{Arg0, Arg1} || Arg0 <- ?BEAMQN_TEST_EVAL_INPUT, Arg1 <- ?BEAMQN_TEST_READ_INPUT]).
