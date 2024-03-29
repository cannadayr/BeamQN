-module(beamqn).
-export([
    call1/2, call1/3,
    %call2/3, call2/4,
    eval/1,  eval/2,
    make/1,  make/2,
    read/1,  read/2
]).
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

call1(_N,_X) ->
    not_loaded(?LINE).
call1(_N,_X,_OPT) ->
    not_loaded(?LINE).
%call2(_N,_X,_W) ->
%    not_loaded(?LINE).
%call2(_N,_X,_W,_OPT) ->
%    not_loaded(?LINE).
eval(_N) ->
    not_loaded(?LINE).
eval(_N,_OPT) ->
    not_loaded(?LINE).
make(_N) ->
    not_loaded(?LINE).
make(_N,_OPT) ->
    not_loaded(?LINE).
read(_N) ->
    not_loaded(?LINE).
read(_N,_OPT) ->
    not_loaded(?LINE).
