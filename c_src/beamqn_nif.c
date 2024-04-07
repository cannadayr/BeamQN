#include <bqnffi.h>
#include <erl_nif.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ERL_NIF_TERM ok_atom;
ERL_NIF_TERM tsdiff_atom;
ErlNifResourceType* BEAMQN_BQNV;

static ERL_NIF_TERM beamqn_make_atom(ErlNifEnv* env, const char* atom) {
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
        return enif_make_atom(env, atom);
    }

    return ret;
}

bool beamqn_opt_get_bool(ErlNifEnv*, ERL_NIF_TERM, bool*);
bool beamqn_opt_get_bool(ErlNifEnv* env, ERL_NIF_TERM atom, bool* opt) {
    char buf[6]; // max char length of false + 1
    if (!enif_get_atom(env, atom, buf, 6, ERL_NIF_LATIN1)) {
        return false;
    }
    if (strcmp(buf, "true") == 0) {
        *opt = true;
    }
    else if (strcmp(buf, "false") == 0) {
        *opt = false;
    }
    else {
        return false;
    }
    return true;
}

static void beamqn_free_bqnv(ErlNifEnv* env, void* ptr) {
    BQNV* x = (BQNV*) ptr;
    // CBQN uses its own memory management system (see CBQN/src/opt/) and reads past the end
    // of allocations (see CBQN/src/opt/mm_malloc.c).
    // It is unknown if allocations made with enif_alloc allow reading past the end.
    // If we use the erts allocator, and it does not allow reading past the end, we may
    // need to allocate an additional 64 bytes on every enif_alloc.
    // To use the erts allocator, we will need to either add a new allocator to CBQN,
    // or overload the malloc/free calls with erts equivalents.
    bqn_free(*x);
}

static int beamqn_init(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    ok_atom = beamqn_make_atom(env, "ok");
    tsdiff_atom = beamqn_make_atom(env, "tsdiff");
    BEAMQN_BQNV = enif_open_resource_type(env, NULL, "BQNV", beamqn_free_bqnv, ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);
    bqn_init();
    return 0;
}

typedef struct BqnCallOpt { bool tsdiff; } BqnCallOpt;
#define BQN_CALL_OPT_N 1 // the number of option variants
#define BQN_CALL_OPT_S 7 // the maximum identifier size + 1
typedef struct BqnCallStat { size_t count; ERL_NIF_TERM keys[BQN_CALL_OPT_N]; ERL_NIF_TERM values[BQN_CALL_OPT_N]; } BqnCallStat;

static ERL_NIF_TERM beamqn_bqn_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    BqnCallOpt call_opt;
    BqnCallStat stat;

    stat.count = 0;
    call_opt.tsdiff = false;

    ERL_NIF_TERM arg, term;
    BQNV *prog, *x, *w, *bqnv;
    int arg_arity;
    const ERL_NIF_TERM *arg_cur;

    arg = argv[1]; // can be arity 1 or 2

    ErlNifTime ts0 = enif_monotonic_time(ERL_NIF_USEC);

    if (argc != 1 && argc != 2 && argc != 3) {
        return enif_make_badarg(env);
    }
    if (argc == 3) {
        unsigned opt_len;
        ERL_NIF_TERM opt, opt_hd;
        int opt_arity;
        const ERL_NIF_TERM *opt_cur;
        char buf[BQN_CALL_OPT_S];

        opt = argv[2];

        if(!enif_is_list(env, opt)) {
            return enif_make_badarg(env);
        }
        if (!enif_get_list_length(env, opt, &opt_len)) {
            return enif_make_badarg(env);
        }

        for (int i = 0; enif_get_list_cell(env, opt, &opt_hd, (ERL_NIF_TERM*) &opt); i++) {
            if (!enif_get_tuple(env, opt_hd, &opt_arity, &opt_cur)) {
                return enif_make_badarg(env);
            }
            if (opt_arity != 2) {
                return enif_make_badarg(env);
            }
            if (!enif_is_atom(env, opt_cur[0])) {
                return enif_make_badarg(env);
            }
            if (!enif_get_atom(env, opt_cur[0], buf, BQN_CALL_OPT_S, ERL_NIF_LATIN1)) {
                return enif_make_badarg(env);
            }
            if (strcmp(buf, "tsdiff") == 0) {
                if (!beamqn_opt_get_bool(env, opt_cur[1], &call_opt.tsdiff)) {
                    return enif_make_badarg(env);
                }
            }
            else {
                return enif_make_badarg(env);
            }
        }
    }

    if (!enif_get_resource(env, argv[0], BEAMQN_BQNV, (void**) &prog)) {
        return enif_make_badarg(env);
    }
    if (bqn_type(*prog) != 3) { // not a function
        return enif_make_badarg(env);
    }
    bqnv = enif_alloc_resource(BEAMQN_BQNV, sizeof(BQNV));
    if (enif_is_tuple(env,arg)) { // call2
        enif_get_tuple(env, arg, &arg_arity, &arg_cur);
        if (arg_arity != 2) {
            return enif_make_badarg(env);
        }
        if (!enif_get_resource(env, arg_cur[0], BEAMQN_BQNV, (void**) &x)) {
            return enif_make_badarg(env);
        }
        if (!enif_get_resource(env, arg_cur[1], BEAMQN_BQNV, (void**) &w)) {
            return enif_make_badarg(env);
        }
        *bqnv = bqn_call2(*prog, *x, *w);
    }
    else { // call1
        if (!enif_get_resource(env, argv[1], BEAMQN_BQNV, (void**) &x)) {
            return enif_make_badarg(env);
        }
        *bqnv = bqn_call1(*prog, *x);
    }
    term = enif_make_resource(env, bqnv);
    enif_release_resource(bqnv);

    if (call_opt.tsdiff) {
        stat.keys[stat.count] = tsdiff_atom;
        stat.values[stat.count] = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC)-ts0);
        stat.count++;
    }

    if (argc == 2) {
        return enif_make_tuple2(env, ok_atom, term);
    }
    else if (argc == 3) {
        ERL_NIF_TERM stat_out;
        if (!enif_make_map_from_arrays(env, stat.keys, stat.values, stat.count, &stat_out)) {
            return enif_make_badarg(env);
        }
        return enif_make_tuple3(env, ok_atom, term, stat_out);
    }
    else {
        return enif_make_badarg(env);
    }
}

typedef struct BqnEvalOpt { bool tsdiff; } BqnEvalOpt;
#define BQN_EVAL_OPT_N 1 // the number of option variants
#define BQN_EVAL_OPT_S 7 // the maximum identifier size + 1
typedef struct BqnEvalStat { size_t count; ERL_NIF_TERM keys[BQN_EVAL_OPT_N]; ERL_NIF_TERM values[BQN_EVAL_OPT_N]; } BqnEvalStat;

static ERL_NIF_TERM beamqn_bqn_eval(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    BqnEvalOpt eval_opt;
    BqnEvalStat stat;

    stat.count = 0;
    eval_opt.tsdiff = false;

    ErlNifBinary x;
    BQNV* prog;
    ERL_NIF_TERM term;

    ErlNifTime ts0 = enif_monotonic_time(ERL_NIF_USEC);

    if (argc != 1 && argc != 2) {
        return enif_make_badarg(env);
    }
    if (argc == 2) {
        unsigned opt_len;
        ERL_NIF_TERM opt, opt_hd;
        int opt_arity;
        const ERL_NIF_TERM* opt_cur;
        char buf[BQN_EVAL_OPT_S];

        opt = argv[1];

        if(!enif_is_list(env, opt)) {
            return enif_make_badarg(env);
        }
        if (!enif_get_list_length(env, opt, &opt_len)) {
            return enif_make_badarg(env);
        }

        for (int i = 0; enif_get_list_cell(env, opt, &opt_hd, (ERL_NIF_TERM*) &opt); i++) {
            if (!enif_get_tuple(env, opt_hd, &opt_arity, &opt_cur)) {
                return enif_make_badarg(env);
            }
            if (opt_arity != 2) {
                return enif_make_badarg(env);
            }
            if (!enif_is_atom(env, opt_cur[0])) {
                return enif_make_badarg(env);
            }
            if (!enif_get_atom(env, opt_cur[0], buf, BQN_EVAL_OPT_S, ERL_NIF_LATIN1)) {
                return enif_make_badarg(env);
            }
            if (strcmp(buf, "tsdiff") == 0) {
                if (!beamqn_opt_get_bool(env, opt_cur[1], &eval_opt.tsdiff)) {
                    return enif_make_badarg(env);
                }
            }
            else {
                return enif_make_badarg(env);
            }
        }
    }

    if (!enif_inspect_binary(env, argv[0], &x)) {
        return enif_make_badarg(env);
    }
    char* src = (char*)enif_alloc(x.size + 1);
    memcpy(src, x.data, x.size);
    src[x.size] = '\0';
    prog = enif_alloc_resource(BEAMQN_BQNV, sizeof(BQNV));
    *prog = bqn_evalCStr(src);
    term = enif_make_resource(env, prog);
    enif_release_resource(prog);
    if (bqn_type(*prog) != 3) { // not a function
        return enif_make_badarg(env);
    }

    enif_free(src);

    if (eval_opt.tsdiff) {
        stat.keys[stat.count] = tsdiff_atom;
        stat.values[stat.count] = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC)-ts0);
        stat.count++;
    }

    if (argc == 1) {
        return enif_make_tuple2(env, ok_atom, term);
    }
    else if (argc == 2) {
        ERL_NIF_TERM stat_out;
        if (!enif_make_map_from_arrays(env, stat.keys, stat.values, stat.count, &stat_out)) {
            return enif_make_badarg(env);
        }
        return enif_make_tuple3(env, ok_atom, term, stat_out);
    }
    else {
        return enif_make_badarg(env);
    }
}

typedef struct BqnMakeOpt { bool tsdiff; } BqnMakeOpt;
#define BQN_MAKE_OPT_N 1 // the number of option variants
#define BQN_MAKE_OPT_S 7 // the maximum identifier size + 1
typedef struct BqnMakeStat { size_t count; ERL_NIF_TERM keys[BQN_MAKE_OPT_N]; ERL_NIF_TERM values[BQN_MAKE_OPT_N]; } BqnMakeStat;

static ERL_NIF_TERM beamqn_bqn_make(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    BqnMakeOpt make_opt;
    BqnMakeStat stat;

    stat.count = 0;
    make_opt.tsdiff = false;

    ErlNifTime ts0 = enif_monotonic_time(ERL_NIF_USEC);

    if (argc != 1 && argc != 2) {
        return enif_make_badarg(env);
    }
    if (argc == 2) {
        unsigned opt_len;
        ERL_NIF_TERM opt, opt_hd;
        int opt_arity;
        const ERL_NIF_TERM* opt_cur;
        char buf[BQN_MAKE_OPT_S];

        opt = argv[1];

        if(!enif_is_list(env, opt)) {
            return enif_make_badarg(env);
        }
        if (!enif_get_list_length(env, opt, &opt_len)) {
            return enif_make_badarg(env);
        }

        for (int i = 0; enif_get_list_cell(env, opt, &opt_hd, (ERL_NIF_TERM*) &opt); i++) {
            if (!enif_get_tuple(env, opt_hd, &opt_arity, &opt_cur)) {
                return enif_make_badarg(env);
            }
            if (opt_arity != 2) {
                return enif_make_badarg(env);
            }
            if(!enif_is_atom(env, opt_cur[0])) {
                return enif_make_badarg(env);
            }
            if (!enif_get_atom(env, opt_cur[0], buf, BQN_MAKE_OPT_S, ERL_NIF_LATIN1)) {
                return enif_make_badarg(env);
            }
            if (strcmp(buf, "tsdiff") == 0) {
                if (!beamqn_opt_get_bool(env, opt_cur[1], &make_opt.tsdiff)) {
                    return enif_make_badarg(env);
                }
            }
            else {
                return enif_make_badarg(env);
            }
        }
    }

    BQNV* bqnv;
    ERL_NIF_TERM ref;
    switch (enif_term_type(env,argv[0])) {
        case ERL_NIF_TERM_TYPE_ATOM:
            return enif_make_badarg(env);
            break;
        case ERL_NIF_TERM_TYPE_BITSTRING:
            return enif_make_badarg(env);
            break;
        case ERL_NIF_TERM_TYPE_FLOAT:
            double f64_val;
            if (!enif_get_double(env, argv[0], &f64_val)) {
                return enif_make_badarg(env);
            }
            bqnv = enif_alloc_resource(BEAMQN_BQNV, sizeof(BQNV));
            *bqnv = bqn_makeF64(f64_val);
            ref = enif_make_resource(env, bqnv);
            enif_release_resource(bqnv);
            break;
        case ERL_NIF_TERM_TYPE_FUN:
            return enif_make_badarg(env);
            break;
        case ERL_NIF_TERM_TYPE_INTEGER:
            int64_t i64_val;
            if (!enif_get_int64(env, argv[0], &i64_val)) {
                return enif_make_badarg(env);
            }
            bqnv = enif_alloc_resource(BEAMQN_BQNV, sizeof(BQNV));
            *bqnv = bqn_makeF64((double)i64_val);
            ref = enif_make_resource(env, bqnv);
            enif_release_resource(bqnv);
            break;
        case ERL_NIF_TERM_TYPE_LIST:
            ERL_NIF_TERM x, x_hd;
            unsigned x_len;

            x = argv[0];
            if (!enif_get_list_length(env, argv[0], &x_len)) {
                return enif_make_badarg(env);
            }

            double* f64_vec_val = enif_alloc(x_len * sizeof(double));
            bqnv = enif_alloc_resource(BEAMQN_BQNV, sizeof(BQNV));

            for (int i = 0; enif_get_list_cell(env,x,&x_hd,(ERL_NIF_TERM*) &x); i++) {
                switch (enif_term_type(env, x_hd)) {
                    case ERL_NIF_TERM_TYPE_ATOM:
                        return enif_make_badarg(env);
                        break;
                    case ERL_NIF_TERM_TYPE_BITSTRING:
                        return enif_make_badarg(env);
                        break;
                    case ERL_NIF_TERM_TYPE_FLOAT:
                        double f64_val;
                        if (!enif_get_double(env, x_hd, &f64_val)) {
                            return enif_make_badarg(env);
                        }
                        f64_vec_val[i] = f64_val;
                        break;
                    case ERL_NIF_TERM_TYPE_FUN:
                        return enif_make_badarg(env);
                        break;
                    case ERL_NIF_TERM_TYPE_INTEGER:
                        int64_t i64_val;
                        if (!enif_get_int64(env, x_hd, &i64_val)) {
                            return enif_make_badarg(env);
                        }
                        f64_vec_val[i] = (double)i64_val;
                        break;
                    case ERL_NIF_TERM_TYPE_LIST:
                        return enif_make_badarg(env);
                        break;
                    case ERL_NIF_TERM_TYPE_MAP:
                        return enif_make_badarg(env);
                        break;
                    case ERL_NIF_TERM_TYPE_PID:
                        return enif_make_badarg(env);
                        break;
                    case ERL_NIF_TERM_TYPE_PORT:
                        return enif_make_badarg(env);
                        break;
                    case ERL_NIF_TERM_TYPE_REFERENCE:
                        return enif_make_badarg(env);
                        break;
                    case ERL_NIF_TERM_TYPE_TUPLE:
                        return enif_make_badarg(env);
                        break;
                    default:
                        return enif_make_badarg(env);
                        break;
                }
            }
            *bqnv = bqn_makeF64Vec(x_len, f64_vec_val);

            ref = enif_make_resource(env, bqnv);
            enif_release_resource(bqnv);
            break;
        case ERL_NIF_TERM_TYPE_MAP:
            return enif_make_badarg(env);
            break;
        case ERL_NIF_TERM_TYPE_PID:
            return enif_make_badarg(env);
            break;
        case ERL_NIF_TERM_TYPE_PORT:
            return enif_make_badarg(env);
            break;
        case ERL_NIF_TERM_TYPE_REFERENCE:
            return enif_make_badarg(env);
            break;
        case ERL_NIF_TERM_TYPE_TUPLE:
            return enif_make_badarg(env);
            break;
        default:
            return enif_make_badarg(env);
            break;
    }

    if (make_opt.tsdiff) {
        stat.keys[stat.count] = tsdiff_atom;
        stat.values[stat.count] = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC)-ts0);
        stat.count++;
    }

    if (argc == 1) {
        return enif_make_tuple2(env, ok_atom, ref);
    }
    else if (argc == 2) {
        ERL_NIF_TERM stat_out;
        if (!enif_make_map_from_arrays(env, stat.keys, stat.values, stat.count, &stat_out)) {
            return enif_make_badarg(env);
        }
        return enif_make_tuple3(env, ok_atom, ref, stat_out);
    }
    else {
        return enif_make_badarg(env);
    }
}

typedef struct BqnReadOpt { bool tsdiff; } BqnReadOpt;
#define BQN_READ_OPT_N 1 // the number of option variants
#define BQN_READ_OPT_S 7 // the maximum identifier size + 1
typedef struct BqnReadStat { size_t count; ERL_NIF_TERM keys[BQN_READ_OPT_N]; ERL_NIF_TERM values[BQN_READ_OPT_N]; } BqnReadStat;

static ERL_NIF_TERM beamqn_bqn_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    BQNV* bqnv;
    ERL_NIF_TERM term;
    BqnReadOpt read_opt;
    BqnReadStat stat;

    stat.count = 0;
    read_opt.tsdiff = false;

    ErlNifTime ts0 = enif_monotonic_time(ERL_NIF_USEC);

    if (argc != 1 && argc != 2) {
        return enif_make_badarg(env);
    }
    if (argc == 2) {
        unsigned opt_len;
        ERL_NIF_TERM opt, opt_hd;
        int opt_arity;
        const ERL_NIF_TERM* opt_cur;
        char buf[BQN_READ_OPT_S];

        opt = argv[1];

        if(!enif_is_list(env, opt)) {
            return enif_make_badarg(env);
        }
        if (!enif_get_list_length(env, opt, &opt_len)) {
            return enif_make_badarg(env);
        }

        for (int i = 0; enif_get_list_cell(env, opt, &opt_hd, (ERL_NIF_TERM*) &opt); i++) {
            if (!enif_get_tuple(env, opt_hd, &opt_arity, &opt_cur)) {
                return enif_make_badarg(env);
            }
            if (opt_arity != 2) {
                return enif_make_badarg(env);
            }
            if (!enif_is_atom(env, opt_cur[0])) {
                return enif_make_badarg(env);
            }
            if (!enif_get_atom(env, opt_cur[0], buf, BQN_READ_OPT_S, ERL_NIF_LATIN1)) {
                return enif_make_badarg(env);
            }
            if (strcmp(buf, "tsdiff") == 0) {
                if (!beamqn_opt_get_bool(env, opt_cur[1], &read_opt.tsdiff)) {
                    return enif_make_badarg(env);
                }
            }
            else {
                return enif_make_badarg(env);
            }
        }
    }

    if (!enif_get_resource(env, argv[0], BEAMQN_BQNV, (void**) &bqnv)) {
        return enif_make_badarg(env);
    }

    switch (bqn_type(*bqnv)) {
        case 0: // array
            size_t len = bqn_bound(*bqnv);
            if (len == 0) {
                term = enif_make_list(env,0);
            }
            else {
                double* buf = enif_alloc(len * sizeof(double));
                bqn_readF64Arr(*bqnv, buf);

                ERL_NIF_TERM* ebuf = enif_alloc(len * sizeof(ERL_NIF_TERM));
                for (int i = 0; i < len; i++) {
                    ebuf[i] = enif_make_double(env,buf[i]);
                }

                term = enif_make_list_from_array(env,ebuf,len);

                enif_free(buf);
                enif_free(ebuf);
            }
            break;
        case 1: // number
            term = enif_make_double(env, bqn_readF64(*bqnv));
            break;
        default:
            return enif_make_badarg(env);
            break;
    }

    if (read_opt.tsdiff) {
        stat.keys[stat.count] = tsdiff_atom;
        stat.values[stat.count] = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC)-ts0);
        stat.count++;
    }

    if (argc == 1) {
        return enif_make_tuple2(env, ok_atom, term);
    }
    else if (argc == 2) {
        ERL_NIF_TERM stat_out;
        if (!enif_make_map_from_arrays(env, stat.keys, stat.values, stat.count, &stat_out)) {
            return enif_make_badarg(env);
        }
        return enif_make_tuple3(env, ok_atom, term, stat_out);
    }
    else {
        return enif_make_badarg(env);
    }
}

static ErlNifFunc nif_funcs[] = {
    {"call", 2, beamqn_bqn_call,ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"call", 3, beamqn_bqn_call,ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"eval", 1, beamqn_bqn_eval,ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"eval", 2, beamqn_bqn_eval,ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"make", 1, beamqn_bqn_make,ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"make", 2, beamqn_bqn_make,ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"read", 1, beamqn_bqn_read,ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"read", 2, beamqn_bqn_read,ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(beamqn, nif_funcs, &beamqn_init, NULL, NULL, NULL)
