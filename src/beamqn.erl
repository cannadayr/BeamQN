-module(beamqn).
-export([makeF64/1,makeF64Vec/1,readF64/1,readF64Vec/1]).
-on_load(init/0).

-define(APPNAME, beamqn).
-define(LIBNAME, 'BeamQN').

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

makeF64(_N) ->
    not_loaded(?LINE).
makeF64Vec(_N) ->
    not_loaded(?LINE).
readF64(_N) ->
    not_loaded(?LINE).
readF64Vec(_N) ->
    not_loaded(?LINE).
