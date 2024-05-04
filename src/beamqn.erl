-module(beamqn).
-export([
    call/2, call/3,
    eval/1, eval/2,
    make/1, make/2,
    read/1, read/2
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

-spec call(F :: reference(), X :: reference() | {reference(), reference()}) -> {atom(), reference()}.
%% @doc Invoke BQN function; F is a reference to a BQN function; X is either a reference or a two-tuple of references.
call(_F,_X) ->
    not_loaded(?LINE).
-spec call(F :: reference(), X :: reference() | {reference(), reference()}, OPT :: [] | [{atom(), atom()}]) -> {atom(), reference()}.
%% @doc Invoke BQN function; F is a reference to a BQN function; X is either a reference or a two-tuple of references; OPT is a list of options.
call(_F,_X,_OPT) ->
    not_loaded(?LINE).
-spec eval(S :: bitstring()) -> {atom(), reference()}.
%% @doc Evaluate BQN code in a fresh environment; S is a utf8 encoded bitstring.
eval(_S) ->
    not_loaded(?LINE).
-spec eval(S :: bitstring(), OPT :: [] | [{atom(), atom()}]) -> {atom(), reference()}.
%% @doc Evaluate BQN code in a fresh environment; S is a utf8 encoded bitstring; OPT is a list of options.
eval(_S,_OPT) ->
    not_loaded(?LINE).
-spec make(X :: integer() | float() | [] | [integer()] | [float()]) -> {atom(), reference()}.
%% @doc Create new BQN objects.
make(_X) ->
    not_loaded(?LINE).
-spec make(X :: integer() | float() | [] | [integer()] | [float()], OPT :: [] | [{atom(), atom()}]) -> {atom(), reference()}.
%% @doc Create new BQN objects; OPT is a list of options.
make(_X,_OPT) ->
    not_loaded(?LINE).
-spec read(R :: reference()) -> {atom(), integer() | float() | [] | [integer()] | [float()]}.
%% @doc Read BQN objects; R is a reference to a BQN object.
read(_R) ->
    not_loaded(?LINE).
-spec read(N :: reference(), OPT :: [] | [{atom(), atom()}]) -> {atom(), integer() | float() | [] | [integer()] | [float()]}.
%% @doc Read BQN objects; R is a reference to a BQN object; OPT is a list of options.
read(_N,_OPT) ->
    not_loaded(?LINE).
