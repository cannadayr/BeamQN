-module(beamqn_test).
-include_lib("eunit/include/eunit.hrl").
-export([make_tests/0]).

-define(MAX_TSDIFF,1000).

make_test(Arg0) ->
    {Msg,Ref,Stats} = beamqn:make(Arg0,[{tsdiff,true}]),
    #{ tsdiff := TsDiff } = Stats,
    ?assertEqual({ok,true,true},{Msg,erlang:is_reference(Ref),TsDiff<?MAX_TSDIFF}).
make_tests() ->
    make_test(0.0),
    make_test([]).
