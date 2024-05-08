#include <bqnffi.h>
#include <erl_nif.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ERL_NIF_TERM beamqn_atom_core_ok;
ERL_NIF_TERM beamqn_atom_core_err;
ERL_NIF_TERM beamqn_atom_opt_tsdiff;
ERL_NIF_TERM beamqn_atom_err_badtype;
ERL_NIF_TERM beamqn_atom_err_oom;
ERL_NIF_TERM beamqn_atom_typ_elt_unk;
ERL_NIF_TERM beamqn_atom_typ_elt_f64;
ERL_NIF_TERM beamqn_atom_typ_elt_i8;
ERL_NIF_TERM beamqn_atom_typ_elt_i16;
ERL_NIF_TERM beamqn_atom_typ_elt_i32;
ERL_NIF_TERM beamqn_atom_typ_elt_c8;
ERL_NIF_TERM beamqn_atom_typ_elt_c16;
ERL_NIF_TERM beamqn_atom_typ_elt_c32;
ERL_NIF_TERM beamqn_atom_typ_elt_undef;
ERL_NIF_TERM beamqn_atom_typ_bqn_arr;
ERL_NIF_TERM beamqn_atom_typ_bqn_num;
ERL_NIF_TERM beamqn_atom_typ_bqn_char;
ERL_NIF_TERM beamqn_atom_typ_bqn_func;
ERL_NIF_TERM beamqn_atom_typ_bqn_mod1;
ERL_NIF_TERM beamqn_atom_typ_bqn_mod2;
ERL_NIF_TERM beamqn_atom_typ_bqn_ns;
ERL_NIF_TERM beamqn_atom_typ_bqn_undef;
ERL_NIF_TERM beamqn_atom_typ_nif_atom;
ERL_NIF_TERM beamqn_atom_typ_nif_bitstring;
ERL_NIF_TERM beamqn_atom_typ_nif_float;
ERL_NIF_TERM beamqn_atom_typ_nif_fun;
ERL_NIF_TERM beamqn_atom_typ_nif_integer;
ERL_NIF_TERM beamqn_atom_typ_nif_list;
ERL_NIF_TERM beamqn_atom_typ_nif_map;
ERL_NIF_TERM beamqn_atom_typ_nif_pid;
ERL_NIF_TERM beamqn_atom_typ_nif_port;
ERL_NIF_TERM beamqn_atom_typ_nif_reference;
ERL_NIF_TERM beamqn_atom_typ_nif_tuple;
ERL_NIF_TERM beamqn_atom_typ_nif_undef;

ErlNifResourceType *BqnvResource;

static BQNV *beamqn_safe_eval;

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

bool beamqn_decode_c8(ErlNifEnv*, size_t, BQNV*, ERL_NIF_TERM*, ERL_NIF_TERM*);
bool beamqn_decode_c8(ErlNifEnv *env, size_t len, BQNV *bqnv, ERL_NIF_TERM *term, ERL_NIF_TERM *err) {
    ErlNifBinary ebin;
    if (!enif_alloc_binary(len * sizeof(uint8_t), &ebin)) {
        *err = beamqn_atom_err_oom;
        return false;
    }
    bqn_readC8Arr(*bqnv, ebin.data);
    *term = enif_make_binary(env, &ebin);
    return true;
}

static void beamqn_free_bqnv(ErlNifEnv* env, void* ptr) {
    BQNV *x = (BQNV*) ptr;
    // CBQN uses its own memory management system (see CBQN/src/opt/) and reads past the end
    // of allocations (see CBQN/src/opt/mm_malloc.c and VERIFY_TAIL).
    // It is unknown if allocations made with enif_alloc allow reading past the end.
    // If we use the erts allocator, and it does not allow reading past the end, we may
    // need to allocate an additional 64 bytes on every enif_alloc.
    // To use the erts allocator, we will need to either add a new allocator to CBQN,
    // or overload the malloc/free calls with erts equivalents.
    bqn_free(*x);
}

static int beamqn_init(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    beamqn_atom_core_ok             = beamqn_make_atom(env, "ok");
    beamqn_atom_core_err            = beamqn_make_atom(env, "err");
    beamqn_atom_opt_tsdiff          = beamqn_make_atom(env, "tsdiff");
    beamqn_atom_err_badtype         = beamqn_make_atom(env, "badtype");
    beamqn_atom_err_oom             = beamqn_make_atom(env, "oom");
    beamqn_atom_typ_elt_unk         = beamqn_make_atom(env, "elt_unk");
    beamqn_atom_typ_elt_f64         = beamqn_make_atom(env, "elt_f64");
    beamqn_atom_typ_elt_i8          = beamqn_make_atom(env, "elt_i8");
    beamqn_atom_typ_elt_i16         = beamqn_make_atom(env, "elt_i16");
    beamqn_atom_typ_elt_i32         = beamqn_make_atom(env, "elt_i32");
    beamqn_atom_typ_elt_c8          = beamqn_make_atom(env, "elt_c8");
    beamqn_atom_typ_elt_c16         = beamqn_make_atom(env, "elt_c16");
    beamqn_atom_typ_elt_c32         = beamqn_make_atom(env, "elt_c32");
    beamqn_atom_typ_elt_undef       = beamqn_make_atom(env, "elt_undef");
    beamqn_atom_typ_bqn_arr         = beamqn_make_atom(env, "bqn_arr");
    beamqn_atom_typ_bqn_num         = beamqn_make_atom(env, "bqn_num");
    beamqn_atom_typ_bqn_char        = beamqn_make_atom(env, "bqn_char");
    beamqn_atom_typ_bqn_func        = beamqn_make_atom(env, "bqn_func");
    beamqn_atom_typ_bqn_mod1        = beamqn_make_atom(env, "bqn_mod1");
    beamqn_atom_typ_bqn_mod2        = beamqn_make_atom(env, "bqn_mod2");
    beamqn_atom_typ_bqn_ns          = beamqn_make_atom(env, "bqn_ns");
    beamqn_atom_typ_bqn_undef       = beamqn_make_atom(env, "bqn_undef");
    beamqn_atom_typ_nif_atom        = beamqn_make_atom(env, "nif_atom");
    beamqn_atom_typ_nif_bitstring   = beamqn_make_atom(env, "nif_bitstring");
    beamqn_atom_typ_nif_float       = beamqn_make_atom(env, "nif_float");
    beamqn_atom_typ_nif_fun         = beamqn_make_atom(env, "nif_fun");
    beamqn_atom_typ_nif_integer     = beamqn_make_atom(env, "nif_integer");
    beamqn_atom_typ_nif_list        = beamqn_make_atom(env, "nif_list");
    beamqn_atom_typ_nif_map         = beamqn_make_atom(env, "nif_map");
    beamqn_atom_typ_nif_pid         = beamqn_make_atom(env, "nif_pid");
    beamqn_atom_typ_nif_port        = beamqn_make_atom(env, "nif_port");
    beamqn_atom_typ_nif_reference   = beamqn_make_atom(env, "nif_reference");
    beamqn_atom_typ_nif_tuple       = beamqn_make_atom(env, "nif_tuple");
    beamqn_atom_typ_nif_undef       = beamqn_make_atom(env, "nif_undef");

    BqnvResource = enif_open_resource_type(env, NULL, "BQNV", beamqn_free_bqnv, ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);
    bqn_init();

    // Throwing and catching errors in CBQN leaks memory!
    // See https://github.com/dzaima/CBQN/#limitations
    // This is currently necessary as crashing the BEAM due to an invalid source input is extremely annoying.
    beamqn_safe_eval = enif_alloc(sizeof(BQNV));
    *beamqn_safe_eval = bqn_evalCStr("({𝕊:⟨0,•BQN 𝕩⟩}⎊{𝕊:⟨1,•ToUTF8 •CurrentError@⟩})");

    return 0;
}

static void beamqn_unload(ErlNifEnv* env, void* priv_data) {
    enif_free(beamqn_safe_eval);
}

typedef struct BqnCallOpt { bool tsdiff; } BqnCallOpt;
#define BQN_CALL_OPT_N 1 // the number of option variants
#define BQN_CALL_OPT_S 7 // the maximum identifier size + 1
typedef struct BqnCallStat { size_t count; ERL_NIF_TERM keys[BQN_CALL_OPT_N]; ERL_NIF_TERM values[BQN_CALL_OPT_N]; } BqnCallStat;

static ERL_NIF_TERM beamqn_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

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

        while (enif_get_list_cell(env, opt, &opt_hd, (ERL_NIF_TERM*) &opt)) {
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

    if (!enif_get_resource(env, argv[0], BqnvResource, (void**) &prog)) {
        return enif_make_badarg(env);
    }
    if (3 != bqn_type(*prog)) { // not a function
        return enif_make_badarg(env);
    }
    bqnv = enif_alloc_resource(BqnvResource, sizeof(BQNV));
    if (enif_is_tuple(env,arg)) { // call2
        enif_get_tuple(env, arg, &arg_arity, &arg_cur);
        if (arg_arity != 2) {
            return enif_make_badarg(env);
        }
        if (!enif_get_resource(env, arg_cur[0], BqnvResource, (void**) &x)) {
            return enif_make_badarg(env);
        }
        if (!enif_get_resource(env, arg_cur[1], BqnvResource, (void**) &w)) {
            return enif_make_badarg(env);
        }
        *bqnv = bqn_call2(*prog, *x, *w);
    }
    else { // call1
        if (!enif_get_resource(env, argv[1], BqnvResource, (void**) &x)) {
            return enif_make_badarg(env);
        }
        *bqnv = bqn_call1(*prog, *x);
    }
    term = enif_make_resource(env, bqnv);
    enif_release_resource(bqnv);

    if (call_opt.tsdiff) {
        stat.keys[stat.count] = beamqn_atom_opt_tsdiff;
        stat.values[stat.count] = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC)-ts0);
        stat.count++;
    }

    if (argc == 2) {
        return enif_make_tuple2(env, beamqn_atom_core_ok, term);
    }
    else if (argc == 3) {
        ERL_NIF_TERM stat_out;
        if (!enif_make_map_from_arrays(env, stat.keys, stat.values, stat.count, &stat_out)) {
            return enif_make_badarg(env);
        }
        return enif_make_tuple3(env, beamqn_atom_core_ok, term, stat_out);
    }
    else {
        return enif_make_badarg(env);
    }
}

typedef struct BqnEvalOpt { bool tsdiff; } BqnEvalOpt;
#define BQN_EVAL_OPT_N 1 // the number of option variants
#define BQN_EVAL_OPT_S 7 // the maximum identifier size + 1
typedef struct BqnEvalStat { size_t count; ERL_NIF_TERM keys[BQN_EVAL_OPT_N]; ERL_NIF_TERM values[BQN_EVAL_OPT_N]; } BqnEvalStat;

static ERL_NIF_TERM beamqn_eval(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    BqnEvalOpt eval_opt;
    BqnEvalStat stat;

    stat.count = 0;
    eval_opt.tsdiff = false;

    ErlNifBinary x;
    BQNV *func;
    BQNV prog, bqn_err;
    ERL_NIF_TERM term, err, atom;

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

        while (enif_get_list_cell(env, opt, &opt_hd, (ERL_NIF_TERM*) &opt)) {
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

    prog = bqn_call1(*beamqn_safe_eval, bqn_makeUTF8Str(x.size, (const char*)x.data));

    // There are dangers in casting to integers in C.
    // This seems safe since we are guaranteeing 0 or 1 returned from beamqn_safe_eval.
    // That may not be true with different compilers or compiler versions.
    // See https://www.cs.cmu.edu/~rbd/papers/cmj-float-to-int.html
    if (1 == (int)bqn_toF64(bqn_pick(prog,0))) {
        bqn_err = bqn_pick(prog,1);
        size_t len = bqn_bound(bqn_err);
        if (0 != bqn_type(bqn_err)) { // not an array
            return enif_make_badarg(env);
        }
        switch (bqn_directArrType(bqn_err)) {
            case elt_c8:
                if (!beamqn_decode_c8(env, len, &bqn_err, &term, &err)) {
                    return enif_raise_exception(env, err);
                }
                atom = beamqn_atom_core_err;
                break;
            default:
                return enif_raise_exception(env, enif_make_tuple2(env, beamqn_atom_core_err, beamqn_atom_typ_elt_undef));
                break;
        }
    }
    else {
        func = enif_alloc_resource(BqnvResource, sizeof(BQNV));
        *func = bqn_pick(prog,1);

        if (3 != bqn_type(*func)) { // not a function
            return enif_make_badarg(env);
        }

        atom = beamqn_atom_core_ok;
        term = enif_make_resource(env, func);
        enif_release_resource(func);
    }

    if (eval_opt.tsdiff) {
        stat.keys[stat.count] = beamqn_atom_opt_tsdiff;
        stat.values[stat.count] = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC)-ts0);
        stat.count++;
    }

    if (argc == 1) {
        return enif_make_tuple2(env, atom, term);
    }
    else if (argc == 2) {
        ERL_NIF_TERM stat_out;
        if (!enif_make_map_from_arrays(env, stat.keys, stat.values, stat.count, &stat_out)) {
            return enif_make_badarg(env);
        }
        return enif_make_tuple3(env, atom, term, stat_out);
    }
    else {
        return enif_make_badarg(env);
    }
}

bool beamqn_make_bqnv_terminal(ErlNifEnv*, ERL_NIF_TERM, BQNV*, ERL_NIF_TERM*);
bool beamqn_make_bqnv_terminal(ErlNifEnv* env, ERL_NIF_TERM term, BQNV* bqnv, ERL_NIF_TERM *err) {
    switch (enif_term_type(env, term)) {
        case ERL_NIF_TERM_TYPE_ATOM:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_atom);
            return false;
            break;
        case ERL_NIF_TERM_TYPE_BITSTRING:
            ErlNifBinary ebin;
            if (!enif_inspect_binary(env, term, &ebin)) {
                *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_bitstring);
                return false;
            }
            // https://stackoverflow.com/questions/14746889/casting-from-unsigned-into-signed-char-in-c/14746982#14746982
            *bqnv = bqn_makeUTF8Str(ebin.size, (const char*)ebin.data);
            return true;
            break;
        case ERL_NIF_TERM_TYPE_FLOAT:
            double f64_val;
            if (!enif_get_double(env, term, &f64_val)) {
                *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_float);
                return false;
            }
            *bqnv = bqn_makeF64(f64_val);
            return true;
            break;
        case ERL_NIF_TERM_TYPE_FUN:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_fun);
            return false;
            break;
        case ERL_NIF_TERM_TYPE_INTEGER:
            int64_t i64_val;
            if (!enif_get_int64(env, term, &i64_val)) {
                *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_integer);
                return false;
            }
            *bqnv = bqn_makeF64((double)i64_val);
            return true;
            break;
        case ERL_NIF_TERM_TYPE_PID:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_pid);
            return false;
            break;
        case ERL_NIF_TERM_TYPE_PORT:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_port);
            return false;
            break;
        case ERL_NIF_TERM_TYPE_REFERENCE:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_reference);
            return false;
            break;
        default:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_undef);
            return false;
            break;
    }
}

bool beamqn_make_bqnv(ErlNifEnv*, ERL_NIF_TERM, BQNV*, ERL_NIF_TERM*);
bool beamqn_make_bqnv(ErlNifEnv* env, ERL_NIF_TERM term, BQNV* bqnv, ERL_NIF_TERM *err) {
    if (enif_is_map(env, term) || enif_is_tuple(env, term)) {
        // only allow lists as nonterminals.
        *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list);
        return false;
    }
    else if (!(enif_is_list(env, term) || enif_is_map(env, term) || enif_is_tuple(env, term))) {
        if (!beamqn_make_bqnv_terminal(env, term, bqnv, err)) {
            return false;
        }
    }
    else {
        unsigned len;
        if (!enif_get_list_length(env, term, &len)) {
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list);
            return false;
        }
        BQNV *arr = enif_alloc(len * sizeof(BQNV));
        if (0 == len) {
            *bqnv = bqn_makeObjVec(len, arr);
        }
        else {
            ERL_NIF_TERM hd;
            for (int i = 0; enif_get_list_cell(env, term, &hd, (ERL_NIF_TERM*) &term); i++) {
                // This is not tail call optimized, and vulnerable to stack overflows!
                beamqn_make_bqnv(env, hd, &arr[i], err);
            }
            *bqnv = bqn_makeObjVec(len, arr);
        }
    }
    return true;
}

typedef struct BqnMakeOpt { bool tsdiff; } BqnMakeOpt;
#define BQN_MAKE_OPT_N 1 // the number of option variants
#define BQN_MAKE_OPT_S 7 // the maximum identifier size + 1
typedef struct BqnMakeStat { size_t count; ERL_NIF_TERM keys[BQN_MAKE_OPT_N]; ERL_NIF_TERM values[BQN_MAKE_OPT_N]; } BqnMakeStat;

static ERL_NIF_TERM beamqn_make(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

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

        while (enif_get_list_cell(env, opt, &opt_hd, (ERL_NIF_TERM*) &opt)) {
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

    BQNV *bqnv;
    ERL_NIF_TERM err, resource;

    bqnv = enif_alloc_resource(BqnvResource, sizeof(BQNV));
    if (!beamqn_make_bqnv(env, argv[0], bqnv, &err)) {
        return enif_raise_exception(env, err);
    }
    resource = enif_make_resource(env, bqnv);
    enif_release_resource(bqnv);

    if (make_opt.tsdiff) {
        stat.keys[stat.count] = beamqn_atom_opt_tsdiff;
        stat.values[stat.count] = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC)-ts0);
        stat.count++;
    }

    if (argc == 1) {
        return enif_make_tuple2(env, beamqn_atom_core_ok, resource);
    }
    else if (argc == 2) {
        ERL_NIF_TERM stat_out;
        if (!enif_make_map_from_arrays(env, stat.keys, stat.values, stat.count, &stat_out)) {
            return enif_make_badarg(env);
        }
        return enif_make_tuple3(env, beamqn_atom_core_ok, resource, stat_out);
    }
    else {
        return enif_make_badarg(env);
    }
}

enum BQNV_TYPE { BQN_ARRAY, BQN_NUMBER, BQN_CHAR, BQN_FUNC, BQN_MOD1, BQN_MOD2, BQN_NS};
bool beamqn_read_bqnv_terminal(ErlNifEnv*, enum BQNV_TYPE, BQNV*, ERL_NIF_TERM*, ERL_NIF_TERM*);
bool beamqn_read_bqnv_terminal(ErlNifEnv* env, enum BQNV_TYPE bqnv_type, BQNV* bqnv, ERL_NIF_TERM *term, ERL_NIF_TERM *err) {
    switch (bqnv_type) {
        case BQN_NUMBER:
            *term = enif_make_double(env, bqn_readF64(*bqnv));
            return true;
            break;
        case BQN_CHAR:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_bqn_char);
            return false;
            break;
        default:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_bqn_undef);
            return false;
            break;
    }
}

bool beamqn_read_bqnv_elt_terminal(ErlNifEnv*, BQNElType, size_t, BQNV*, ERL_NIF_TERM*, ERL_NIF_TERM*);
bool beamqn_read_bqnv_elt_terminal(ErlNifEnv *env, BQNElType elt_type, size_t len, BQNV* bqnv, ERL_NIF_TERM *term, ERL_NIF_TERM *err) {
    struct EltBuf {
        union { double *f64; ErlNifBinary bin; } b;
    } elt_buf;
    switch (elt_type) {
        case elt_f64:
            ERL_NIF_TERM *ebuf;
            elt_buf.b.f64 = enif_alloc(len * sizeof(double));
            bqn_readF64Arr(*bqnv, elt_buf.b.f64);

            ebuf = enif_alloc(len * sizeof(ERL_NIF_TERM));
            for (int i = 0; i < len; i++) {
                ebuf[i] = enif_make_double(env, elt_buf.b.f64[i]);
            }

            *term = enif_make_list_from_array(env, ebuf, len);

            enif_free(elt_buf.b.f64);
            enif_free(ebuf);
            return true;
            break;
        case elt_i8:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_elt_i8);
            return false;
            break;
        case elt_i16:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_elt_i16);
            return false;
            break;
        case elt_i32:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_elt_i32);
            return false;
            break;
        case elt_c8:
            if (!beamqn_decode_c8(env, len, bqnv, term, err)) {
                return false;
            }
            else {
                return true;
            }
            break;
        case elt_c16:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_elt_c16);
            return false;
            break;
        case elt_c32:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_elt_c32);
            return false;
            break;
        default:
            *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_elt_undef);
            return false;
            break;
    }
}

bool beamqn_read_bqnv(ErlNifEnv*, BQNV*, ERL_NIF_TERM*, ERL_NIF_TERM*);
bool beamqn_read_bqnv(ErlNifEnv* env, BQNV* bqnv, ERL_NIF_TERM *term, ERL_NIF_TERM *err) {
    enum BQNV_TYPE type = bqn_type(*bqnv);
    if (type == BQN_NS) {
        // only allow arrays as nonterminals.
        *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_bqn_ns);
        return false;
    }
    else if ((type == BQN_FUNC) || (type == BQN_MOD1) || (type == BQN_MOD2)) {
        // don't allow these terminals.
        *err = enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_bqn_func);
        return false;
    }
    else if ((type == BQN_NUMBER) || (type == BQN_CHAR)) {
        if (!beamqn_read_bqnv_terminal(env, type, bqnv, term, err)) {
            return false;
        }
    }
    else if (type == BQN_ARRAY) {
        size_t len = bqn_bound(*bqnv);
        if (len == 0) {
            *term = enif_make_list(env, 0);
        }
        else {
            BQNElType elt_type = bqn_directArrType(*bqnv);
            // only elt_unk arrays are non-terminating.
            if (elt_type != elt_unk) {
                if (!beamqn_read_bqnv_elt_terminal(env, elt_type, len, bqnv, term, err)) {
                    return false;
                }
            }
            else {
                ERL_NIF_TERM *ebuf;
                ebuf = enif_alloc(len * sizeof(ERL_NIF_TERM));
                for (size_t i = 0; i < len; i++) {
                    BQNV elem = bqn_pick(*bqnv, i);
                    // This is not tail call optimized, and vulnerable to stack overflows!
                    beamqn_read_bqnv(env, &elem, &ebuf[i], err);
                }
                *term = enif_make_list_from_array(env, ebuf, len);
                enif_free(ebuf);
            }
        }
    }
    return true;
}

typedef struct BqnReadOpt { bool tsdiff; } BqnReadOpt;
#define BQN_READ_OPT_N 1 // the number of option variants
#define BQN_READ_OPT_S 7 // the maximum identifier size + 1
typedef struct BqnReadStat { size_t count; ERL_NIF_TERM keys[BQN_READ_OPT_N]; ERL_NIF_TERM values[BQN_READ_OPT_N]; } BqnReadStat;

static ERL_NIF_TERM beamqn_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    BQNV *bqnv;
    ERL_NIF_TERM term, err;
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

        while (enif_get_list_cell(env, opt, &opt_hd, (ERL_NIF_TERM*) &opt)) {
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

    if (!enif_get_resource(env, argv[0], BqnvResource, (void**) &bqnv)) {
        return enif_make_badarg(env);
    }

    if (!beamqn_read_bqnv(env, bqnv, &term, &err)) {
        return enif_raise_exception(env, err);
    }

    if (read_opt.tsdiff) {
        stat.keys[stat.count] = beamqn_atom_opt_tsdiff;
        stat.values[stat.count] = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC)-ts0);
        stat.count++;
    }

    if (argc == 1) {
        return enif_make_tuple2(env, beamqn_atom_core_ok, term);
    }
    else if (argc == 2) {
        ERL_NIF_TERM stat_out;
        if (!enif_make_map_from_arrays(env, stat.keys, stat.values, stat.count, &stat_out)) {
            return enif_make_badarg(env);
        }
        return enif_make_tuple3(env, beamqn_atom_core_ok, term, stat_out);
    }
    else {
        return enif_make_badarg(env);
    }
}

static ErlNifFunc nif_funcs[] = {
    {"call", 2, beamqn_call, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"call", 3, beamqn_call, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"eval", 1, beamqn_eval, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"eval", 2, beamqn_eval, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"make", 1, beamqn_make, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"make", 2, beamqn_make, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"read", 1, beamqn_read, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"read", 2, beamqn_read, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(beamqn, nif_funcs, &beamqn_init, NULL, NULL, &beamqn_unload)
