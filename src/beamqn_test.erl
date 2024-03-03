-module(beamqn_test).
-include_lib("eunit/include/eunit.hrl").
-export([beamqn_bqn_makeF64_tests/0]).

-define(MAX_TSDIFF,1000).

beamqn_bqn_makeF64_test(Arg0) ->
    {Msg,Ref,TsDiff} = beamqn:makeF64(Arg0,true),
    ?assertEqual({ok,true,true},{Msg,erlang:is_reference(Ref),TsDiff<?MAX_TSDIFF}).
beamqn_bqn_makeF64_tests() ->
    beamqn_bqn_makeF64_test(0.0).
