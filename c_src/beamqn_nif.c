#include <stdlib.h>
#include <stdio.h>
#include <erl_nif.h>
#include <bqnffi.h>

ERL_NIF_TERM ok_atom;
ErlNifResourceType* BEAMQN_BQNV;

static ERL_NIF_TERM beamqn_make_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

static void
beamqn_free_bqnv(ErlNifEnv* env, void* ptr)
{
    BQNV* x = (BQNV*) ptr;
    bqn_free(*x);
    enif_free(x);
}


static int beamqn_init(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ok_atom = beamqn_make_atom(env, "ok");
    BEAMQN_BQNV = enif_open_resource_type(env, NULL, "BQNV", beamqn_free_bqnv, ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);
    bqn_init();
    return 0;
}

static ERL_NIF_TERM beamqn_bqn_makeF64(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double x;
    if (!enif_get_double(env, argv[0], &x)) {
	    return enif_make_badarg(env);
    }

    BQNV* bqn_f64 = enif_alloc_resource(BEAMQN_BQNV, sizeof(BQNV));
    *bqn_f64 = bqn_makeF64(x);
    ERL_NIF_TERM term = enif_make_resource(env, bqn_f64);

    enif_release_resource(bqn_f64);

    return enif_make_tuple2(env, ok_atom, term);
}

static ERL_NIF_TERM beamqn_bqn_readF64(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    BQNV* x;
    if (!enif_get_resource(env, argv[0], BEAMQN_BQNV, (void**) &x)) {
        return enif_make_badarg(env);
    }

    double f64 = bqn_readF64(*x);
    ERL_NIF_TERM term = enif_make_double(env, f64);
    return enif_make_tuple2(env, ok_atom, term);
}

static ErlNifFunc nif_funcs[] = {
    {"makeF64", 1, beamqn_bqn_makeF64},
    {"readF64", 1, beamqn_bqn_readF64}
};

ERL_NIF_INIT(beamqn, nif_funcs, &beamqn_init, NULL, NULL, NULL)
