#include <stdlib.h>
#include <stdio.h>
#include <erl_nif.h>
#include <bqnffi.h>

ERL_NIF_TERM ok_atom;
ErlNifResourceType* BEAMQN_BQNV;

static ERL_NIF_TERM beamqn_make_atom(ErlNifEnv* env, const char* atom) {
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
        return enif_make_atom(env, atom);
    }

    return ret;
}

static ERL_NIF_TERM beamqn_make_error(ErlNifEnv* env, const char* mesg) {
    return enif_make_tuple2(env, beamqn_make_atom(env, "error"), beamqn_make_atom(env, mesg));
}

static void beamqn_free_bqnv(ErlNifEnv* env, void* ptr) {
    BQNV* x = (BQNV*) ptr;
    bqn_free(*x);
    enif_free(x);
}

static int beamqn_init(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    ok_atom = beamqn_make_atom(env, "ok");
    BEAMQN_BQNV = enif_open_resource_type(env, NULL, "BQNV", beamqn_free_bqnv, ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);
    bqn_init();
    return 0;
}

static ERL_NIF_TERM beamqn_bqn_makeF64(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    double x, ts0, ts1;
    ERL_NIF_TERM term, tsdiff;
    BQNV* bqn_f64;

    ts0 = enif_monotonic_time(ERL_NIF_USEC);

    if (!enif_get_double(env, argv[0], &x)) {
	    return enif_make_badarg(env);
    }

    bqn_f64 = enif_alloc_resource(BEAMQN_BQNV, sizeof(BQNV));
    *bqn_f64 = bqn_makeF64(x);

    term = enif_make_resource(env, bqn_f64);
    enif_release_resource(bqn_f64);

    ts1 = enif_monotonic_time(ERL_NIF_USEC);
    tsdiff = enif_make_int64(env, ts1-ts0);

    return enif_make_tuple3(env, ok_atom, tsdiff, term);

}

static ERL_NIF_TERM beamqn_bqn_makeF64Vec(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    unsigned x_len;
    ERL_NIF_TERM x, x_hd, x_tl;
    double x_cur;
    double ts0;
    ERL_NIF_TERM term, tsdiff;
    BQNV* bqn_f64Vec;

    ts0 = enif_monotonic_time(ERL_NIF_USEC);

    x = argv[0];

    if(!enif_is_list(env, x)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_list_length(env, x, &x_len)) {
        return enif_make_badarg(env);
    }

    double* bqn_arr = enif_alloc(x_len * sizeof(double));
    bqn_f64Vec = enif_alloc_resource(BEAMQN_BQNV, sizeof(BQNV));

    for (int i = 0; enif_get_list_cell(env,x,&x_hd,(ERL_NIF_TERM*) &x); i++) {
        if (!enif_get_double(env, x_hd, &x_cur)) {
            return enif_make_badarg(env);
        }
        bqn_arr[i] = x_cur;
    }
    *bqn_f64Vec = bqn_makeF64Vec(x_len, bqn_arr);

    term = enif_make_resource(env, bqn_f64Vec);
    enif_release_resource(bqn_f64Vec);

    tsdiff = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC)-ts0);

    return enif_make_tuple3(env, ok_atom, tsdiff, term);

}

static ERL_NIF_TERM beamqn_bqn_readF64(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    BQNV* x;
    double ts0;
    ERL_NIF_TERM term, tsdiff;

    ts0 = enif_monotonic_time(ERL_NIF_USEC);

    if (!enif_get_resource(env, argv[0], BEAMQN_BQNV, (void**) &x)) {
        return enif_make_badarg(env);
    }

    term = enif_make_double(env, bqn_readF64(*x));

    tsdiff = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC)-ts0);

    return enif_make_tuple3(env, ok_atom, tsdiff, term);
}

static ERL_NIF_TERM beamqn_bqn_readF64Vec(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    BQNV* x;
    double ts0, ts1;
    ERL_NIF_TERM term, tsdiff;

    ts0 = enif_monotonic_time(ERL_NIF_USEC);

    if (!enif_get_resource(env, argv[0], BEAMQN_BQNV, (void**) &x)) {
        return enif_make_badarg(env);
    }

    size_t len = bqn_bound(*x);
    if (len == 0) {
        term = enif_make_list(env,0);
    }
    else {
        double* buf = enif_alloc(len * sizeof(double));
        bqn_readF64Arr(*x, buf);

        ERL_NIF_TERM* ebuf = enif_alloc(len * sizeof(ERL_NIF_TERM));
        for (int i = 0; i < len; i++) {
            ebuf[i] = enif_make_double(env,buf[i]);
        }

        term = enif_make_list_from_array(env,ebuf,len);
    }

    tsdiff = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC)-ts0);

    return enif_make_tuple3(env, ok_atom, tsdiff, term);
}

static ErlNifFunc nif_funcs[] = {
    {"makeF64",    1, beamqn_bqn_makeF64},
    {"makeF64Vec", 1, beamqn_bqn_makeF64Vec,ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"readF64",    1, beamqn_bqn_readF64},
    {"readF64Vec", 1, beamqn_bqn_readF64Vec,ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(beamqn, nif_funcs, &beamqn_init, NULL, NULL, NULL)
