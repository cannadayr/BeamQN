#include <erl_nif.h>

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
    int x, ret;
    if (!enif_get_int(env, argv[0], &x)) {
	return enif_make_badarg(env);
    }
    ret = mk_atom(env, "ok");
    return ret;
}

static ErlNifFunc nif_funcs[] = {
    {"makeF64", 1, makeF64}
};

ERL_NIF_INIT(beamqn, nif_funcs, NULL, NULL, NULL, NULL)
