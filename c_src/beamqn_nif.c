#include <bqnffi.h>
#include <erl_nif.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ERL_NIF_TERM beamqn_atom_core_ok;
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
    beamqn_atom_core_ok             = beamqn_make_atom(env, "ok");
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

static ERL_NIF_TERM beamqn_bqn_eval(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    BqnEvalOpt eval_opt;
    BqnEvalStat stat;

    stat.count = 0;
    eval_opt.tsdiff = false;

    ErlNifBinary x;
    BQNV *func, *safe_eval;
    BQNV prog;
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

    // Requires CATCH_ERRORS=1
    safe_eval = enif_alloc(sizeof(BQNV));
    *safe_eval = bqn_evalCStr("({𝕊:⟨0,•BQN 𝕩⟩}⎊{𝕊:⟨1,•CurrentError@⟩})");

    prog = bqn_call1(*safe_eval, bqn_makeUTF8Str(x.size, (const char*)x.data));

    if (1 == (int)bqn_toF64(bqn_pick(prog,0))) {
        return enif_make_badarg(env);
    }

    func = enif_alloc_resource(BEAMQN_BQNV, sizeof(BQNV));
    *func = bqn_pick(prog,1);

    if (3 != bqn_type(*func)) { // not a function
        return enif_make_badarg(env);
    }

    term = enif_make_resource(env, func);
    enif_release_resource(func);
    enif_free(safe_eval);

    if (eval_opt.tsdiff) {
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
    ErlNifBinary binstr;
    // encode
    switch (enif_term_type(env, argv[0])) {
        case ERL_NIF_TERM_TYPE_ATOM:
            return enif_raise_exception(env, enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_atom));
            break;
        case ERL_NIF_TERM_TYPE_BITSTRING:
            if (!enif_inspect_binary(env, argv[0], &binstr)) {
                return enif_raise_exception(env, enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_bitstring));
            }
            bqnv = enif_alloc_resource(BEAMQN_BQNV, sizeof(BQNV));
            // https://stackoverflow.com/questions/14746889/casting-from-unsigned-into-signed-char-in-c/14746982#14746982
            *bqnv = bqn_makeUTF8Str(binstr.size, (const char*)binstr.data);
            ref = enif_make_resource(env, bqnv);
            enif_release_resource(bqnv);
            break;
        case ERL_NIF_TERM_TYPE_FLOAT:
            double f64_val;
            if (!enif_get_double(env, argv[0], &f64_val)) {
                return enif_raise_exception(env, enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_float));
            }
            bqnv = enif_alloc_resource(BEAMQN_BQNV, sizeof(BQNV));
            *bqnv = bqn_makeF64(f64_val);
            ref = enif_make_resource(env, bqnv);
            enif_release_resource(bqnv);
            break;
        case ERL_NIF_TERM_TYPE_FUN:
            return enif_raise_exception(env, enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_fun));
            break;
        case ERL_NIF_TERM_TYPE_INTEGER:
            int64_t i64_val;
            if (!enif_get_int64(env, argv[0], &i64_val)) {
                return enif_raise_exception(env, enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_integer));
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
                return enif_raise_exception(env, enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list));
            }

            BQNV *obj_vec = enif_alloc(x_len * sizeof(BQNV));
            bqnv = enif_alloc_resource(BEAMQN_BQNV, sizeof(BQNV));

            for (int i = 0; enif_get_list_cell(env,x,&x_hd,(ERL_NIF_TERM*) &x); i++) {
                switch (enif_term_type(env, x_hd)) {
                    case ERL_NIF_TERM_TYPE_ATOM:
                        return enif_raise_exception(env, enif_make_tuple3(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list, beamqn_atom_typ_nif_atom));
                        break;
                    case ERL_NIF_TERM_TYPE_BITSTRING:
                        if (!enif_inspect_binary(env, x_hd, &binstr)) {
                            return enif_raise_exception(env, enif_make_tuple3(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list, beamqn_atom_typ_nif_bitstring));
                        }
                        bqnv = enif_alloc_resource(BEAMQN_BQNV, sizeof(BQNV));
                        *bqnv = bqn_makeUTF8Str(binstr.size, (const char*)binstr.data);
                        obj_vec[i] = *bqnv;
                        break;
                    case ERL_NIF_TERM_TYPE_FLOAT:
                        double f64_val;
                        if (!enif_get_double(env, x_hd, &f64_val)) {
                            return enif_raise_exception(env, enif_make_tuple3(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list, beamqn_atom_typ_nif_float));
                        }
                        obj_vec[i] = bqn_makeF64(f64_val);
                        break;
                    case ERL_NIF_TERM_TYPE_FUN:
                        return enif_raise_exception(env, enif_make_tuple3(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list, beamqn_atom_typ_nif_fun));
                        break;
                    case ERL_NIF_TERM_TYPE_INTEGER:
                        int64_t i64_val;
                        if (!enif_get_int64(env, x_hd, &i64_val)) {
                            return enif_raise_exception(env, enif_make_tuple3(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list, beamqn_atom_typ_nif_integer));
                        }
                        obj_vec[i] = bqn_makeF64((double)i64_val);
                        break;
                    case ERL_NIF_TERM_TYPE_LIST:
                        return enif_raise_exception(env, enif_make_tuple3(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list, beamqn_atom_typ_nif_list));
                        break;
                    case ERL_NIF_TERM_TYPE_MAP:
                        return enif_raise_exception(env, enif_make_tuple3(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list, beamqn_atom_typ_nif_map));
                        break;
                    case ERL_NIF_TERM_TYPE_PID:
                        return enif_raise_exception(env, enif_make_tuple3(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list, beamqn_atom_typ_nif_pid));
                        break;
                    case ERL_NIF_TERM_TYPE_PORT:
                        return enif_raise_exception(env, enif_make_tuple3(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list, beamqn_atom_typ_nif_port));
                        break;
                    case ERL_NIF_TERM_TYPE_REFERENCE:
                        return enif_raise_exception(env, enif_make_tuple3(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list, beamqn_atom_typ_nif_reference));
                        break;
                    case ERL_NIF_TERM_TYPE_TUPLE:
                        return enif_raise_exception(env, enif_make_tuple3(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list, beamqn_atom_typ_nif_tuple));
                        break;
                    default:
                        return enif_raise_exception(env, enif_make_tuple3(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_list, beamqn_atom_typ_nif_undef));
                        break;
                }
            }
            *bqnv = bqn_makeObjVec(x_len, obj_vec);

            ref = enif_make_resource(env, bqnv);
            enif_release_resource(bqnv);
            break;
        case ERL_NIF_TERM_TYPE_MAP:
            return enif_raise_exception(env, enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_map));
            break;
        case ERL_NIF_TERM_TYPE_PID:
            return enif_raise_exception(env, enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_pid));
            break;
        case ERL_NIF_TERM_TYPE_PORT:
            return enif_raise_exception(env, enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_port));
            break;
        case ERL_NIF_TERM_TYPE_REFERENCE:
            return enif_raise_exception(env, enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_reference));
            break;
        case ERL_NIF_TERM_TYPE_TUPLE:
            return enif_raise_exception(env, enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_tuple));
            break;
        default:
            return enif_raise_exception(env, enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_nif_undef));
            break;
    }

    if (make_opt.tsdiff) {
        stat.keys[stat.count] = beamqn_atom_opt_tsdiff;
        stat.values[stat.count] = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC)-ts0);
        stat.count++;
    }

    if (argc == 1) {
        return enif_make_tuple2(env, beamqn_atom_core_ok, ref);
    }
    else if (argc == 2) {
        ERL_NIF_TERM stat_out;
        if (!enif_make_map_from_arrays(env, stat.keys, stat.values, stat.count, &stat_out)) {
            return enif_make_badarg(env);
        }
        return enif_make_tuple3(env, beamqn_atom_core_ok, ref, stat_out);
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

    // decode
    switch (bqn_type(*bqnv)) {
        case 0: // array
            size_t len = bqn_bound(*bqnv);
            if (len == 0) {
                term = enif_make_list(env,0);
            }
            else {
                struct BqnArrBuf {
                    union { double *buf_f64; ErlNifBinary ebin; } b;
                } arr_buf;
                ERL_NIF_TERM *ebuf;

                switch (bqn_directArrType(*bqnv)) {
                    case elt_unk:
                        ebuf = enif_alloc(len * sizeof(ERL_NIF_TERM));
                        for (size_t i = 0; i < len; i++) {
                            BQNV elem = bqn_pick(*bqnv, i);
                            switch (bqn_type(elem)) {
                                case 0: // array
                                    switch (bqn_directArrType(elem)) {
                                        case elt_unk:
                                            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple4(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk)));
                                            break;
                                        case elt_f64:
                                            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple4(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_f64)));
                                            break;
                                        case elt_i8:
                                            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple4(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_i8)));
                                            break;
                                        case elt_i16:
                                            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple4(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_i16)));
                                            break;
                                        case elt_i32:
                                            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple4(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_i32)));
                                            break;
                                        case elt_c8:
                                            ErlNifBinary etmp;
                                            size_t strlen = bqn_bound(elem);
                                            if (!enif_alloc_binary(strlen * sizeof(uint8_t), &etmp)) {
                                                return enif_raise_exception(env,beamqn_atom_err_oom);
                                            }
                                            bqn_readC8Arr(elem, etmp.data);
                                            ebuf[i] = enif_make_binary(env, &etmp);
                                            break;
                                        case elt_c16:
                                            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple4(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_c16)));
                                            break;
                                        case elt_c32:
                                            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple4(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_c32)));
                                            break;
                                        default:
                                            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple4(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_undef)));
                                            break;
                                    }
                                    break;
                                case 1: // number
                                    ebuf[i] = enif_make_double(env, bqn_toF64(elem));
                                    break;
                                case 2: // char
                                    return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple3(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_char)));
                                    break;
                                case 3: // func
                                    return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple3(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_func)));
                                    break;
                                case 4: // mod1
                                    return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple3(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_mod1)));
                                    break;
                                case 5: // mod2
                                    return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple3(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_mod2)));
                                    break;
                                case 6: // ns
                                    return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple3(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_ns)));
                                    break;
                                default:
                                    return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple3(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_unk, beamqn_atom_typ_bqn_undef)));
                                    break;
                            }
                        }
                        term = enif_make_list_from_array(env, ebuf, len);
                        enif_free(ebuf);
                        break;
                    case elt_f64:
                        arr_buf.b.buf_f64 = enif_alloc(len * sizeof(double));
                        bqn_readF64Arr(*bqnv, arr_buf.b.buf_f64);

                        ebuf = enif_alloc(len * sizeof(ERL_NIF_TERM));
                        for (int i = 0; i < len; i++) {
                            ebuf[i] = enif_make_double(env, arr_buf.b.buf_f64[i]);
                        }

                        term = enif_make_list_from_array(env, ebuf, len);

                        enif_free(arr_buf.b.buf_f64);
                        enif_free(ebuf);
                        break;
                    case elt_i8:
                        return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple2(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_i8)));
                        break;
                    case elt_i16:
                        return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple2(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_i16)));
                        break;
                    case elt_i32:
                        return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple2(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_i32)));
                        break;
                    case elt_c8:
                        if (!enif_alloc_binary(len * sizeof(uint8_t), &arr_buf.b.ebin)) {
                            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple2(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_c8)));
                        }
                        bqn_readC8Arr(*bqnv, arr_buf.b.ebin.data);
                        term = enif_make_binary(env, &arr_buf.b.ebin);
                        break;
                    case elt_c16:
                        return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple2(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_c16)));
                        break;
                    case elt_c32:
                        // CBQN treats characters as unsigned 32 bit integers.
                        // This is equivalent to <<"↕"/utf32-native>> (Erlang) or <<"↕"::utf32-native>> (Elixir).
                        // However, Elixir uses UTF-8 as its default encoding ("↕" == <<"↕"::utf8>>).
                        // This currently requires external type conversions.
                        if (!enif_alloc_binary(len * sizeof(uint32_t), &arr_buf.b.ebin)) {
                            return enif_raise_exception(env,beamqn_atom_err_oom);
                        }
                        bqn_readC32Arr(*bqnv, (uint32_t *) arr_buf.b.ebin.data);
                        term = enif_make_binary(env, &arr_buf.b.ebin);
                        break;
                    default:
                        return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, enif_make_tuple2(env, beamqn_atom_typ_bqn_arr, beamqn_atom_typ_elt_undef)));
                        break;
                }
            }
            break;
        case 1: // number
            term = enif_make_double(env, bqn_readF64(*bqnv));
            break;
        case 2: // char
            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_bqn_char));
            break;
        case 3: // func
            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_bqn_func));
            break;
        case 4: // mod1
            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_bqn_mod1));
            break;
        case 5: // mod2
            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_bqn_mod2));
            break;
        case 6: // ns
            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_bqn_ns));
            break;
        default:
            return enif_raise_exception(env,enif_make_tuple2(env, beamqn_atom_err_badtype, beamqn_atom_typ_bqn_undef));
            break;

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
    {"call", 2, beamqn_bqn_call, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"call", 3, beamqn_bqn_call, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"eval", 1, beamqn_bqn_eval, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"eval", 2, beamqn_bqn_eval, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"make", 1, beamqn_bqn_make, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"make", 2, beamqn_bqn_make, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"read", 1, beamqn_bqn_read, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"read", 2, beamqn_bqn_read, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(beamqn, nif_funcs, &beamqn_init, NULL, NULL, NULL)
