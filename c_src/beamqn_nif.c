#include <stdlib.h>
#include <stdio.h>
#include <erl_nif.h>
#include <bqnffi.h>

static ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

static ERL_NIF_TERM makeF64(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    double x;
    if (!enif_get_double(env, argv[0], &x)) {
	    return enif_make_badarg(env);
    }

    double res;
    BQNV y;
    y = bqn_makeF64(x+1);
    res = bqn_readF64(y);

    //ret = mk_atom(env, "ok");
    ret = enif_make_tuple2(env, mk_atom(env, "ok"), enif_make_double(env, res));
    return ret;
}

static ErlNifFunc nif_funcs[] = {
    {"makeF64", 1, makeF64}
};

ERL_NIF_INIT(beamqn, nif_funcs, NULL, NULL, NULL, NULL)
