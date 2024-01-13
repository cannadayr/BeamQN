-module(beamqn_test).
-include_lib("eunit/include/eunit.hrl").
-export([beamqn_bqn_makeF64_tests/0]).

-define(MAX_TSDIFF,1000).

beamqn_bqn_makeF64_test(Arg0) ->
    {Msg,TsDiff,Ref} = beamqn:makeF64(Arg0),
    ?assertEqual({ok,true,true},{Msg,TsDiff<?MAX_TSDIFF,erlang:is_reference(Ref)}).
beamqn_bqn_makeF64_tests() ->
    beamqn_bqn_makeF64_test(0.0).
