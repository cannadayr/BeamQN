-module(beamqn_test).
-include_lib("eunit/include/eunit.hrl").
-export([
    make_tests/0,
    make_and_read_tests/0
]).

-define(MAX_TSDIFF,1000).

-define(INPUTS, [0, 0.0, 1 bsl 59, -(1 bsl 59)-1, 2.0e53, -2.0e53, [], [0,1,2], [0.0,1.0,2.0]]).

make_test(Arg0) ->
    {Msg,Ref,Stats} = beamqn:make(Arg0,[{tsdiff,true}]),
    #{ tsdiff := TsDiff } = Stats,
    ?assertEqual({ok,true,true},{Msg,erlang:is_reference(Ref),TsDiff<?MAX_TSDIFF}).
make_tests() ->
    lists:map(fun make_test/1,?INPUTS).

norm(N) when is_integer(N) ->
    float(N);
norm(N) when is_float(N) ->
    N;
norm(L) when is_list(L) ->
    lists:map(fun norm/1,L).
make_and_read_test(Arg0) ->
    {Msg0,Ref,Stats0} = beamqn:make(Arg0,[{tsdiff,true}]),
    #{ tsdiff := TsDiff0 } = Stats0,
    ?assertEqual({ok,true,true},{Msg0,erlang:is_reference(Ref),TsDiff0<?MAX_TSDIFF}),
    {Msg1,Rtn,Stats1} = beamqn:read(Ref,[{tsdiff,true}]),
    #{ tsdiff := TsDiff1 } = Stats1,
    ?assertEqual({ok,true,true},{Msg1,norm(Arg0) =:= Rtn,TsDiff1<?MAX_TSDIFF}).
make_and_read_tests() ->
    lists:map(fun make_and_read_test/1,?INPUTS).
