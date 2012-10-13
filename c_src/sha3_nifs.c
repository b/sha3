#include "erl_nif.h"
#include "erl_nif_compat.h"
#include "KeccakNISTInterface.h"

static ErlNifResourceType* keccak_hashstate;

typedef struct
{
} keccak_handle;

// Prototypes
ERL_NIF_TERM keccak_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM keccak_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM keccak_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM keccak_hash(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// lifecycle
int load(ErlNifEnv* env, void ** priv_data, ERL_NIF_TERM load_info);
int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info);
int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info);
void unload(ErlNifEnv* env, void* priv);

static ErlNifFunc nif_funcs[] =
{
    {"init", 1, keccak_init},
    {"update", 3, keccak_update},
    {"final", 1, keccak_final},
    {"hash", 3, keccak_hash}
};

ERL_NIF_INIT(sha3, nif_funcs, load, NULL, NULL, NULL);

static char *hash_return_strings[] = {"success", "fail", "bad_hashlen"};

int load(ErlNifEnv* env, void ** priv_data, ERL_NIF_TERM load_info)
{
  keccak_hashstate = enif_open_resource_type_compat(env, "hashstate", NULL, ERL_NIF_RT_CREATE, NULL);
  return 0;
}

int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
    return 0;
}

void unload(ErlNifEnv* env, void* priv)
{
    return;
}

ERL_NIF_TERM keccak_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{   
  ERL_NIF_TERM hash_state_term;
    int bits = 0;
    if(!enif_get_int(env, argv[0], &bits))
        return enif_make_badarg(env);
    
    hashState *state = (hashState*) enif_alloc_resource_compat(env, keccak_hashstate, sizeof(hashState));
    HashReturn r = Init(state, bits);
    if (r == SUCCESS) {
        hash_state_term = enif_make_resource(env, state);
        enif_release_resource_compat(env, state);
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), hash_state_term);
    } else {
        enif_release_resource_compat(env, state);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "fail"));
    }
}

ERL_NIF_TERM keccak_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hashState *state = NULL;
    enif_get_resource(env, argv[0], keccak_hashstate, (void**)&state);

    ErlNifBinary bin;
    enif_inspect_binary(env, argv[1], &bin);
    
    int bitlength = 0;
    if(!enif_get_int(env, argv[2], &bitlength))
        return enif_make_badarg(env);

    HashReturn r = Update(state, (BitSequence *)(bin.data), bitlength);
    if (r == SUCCESS) {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_resource(env, state));
    } else {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, hash_return_strings[r]));
    }
}

ERL_NIF_TERM keccak_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hashState *state = NULL;
    enif_get_resource(env, argv[0], keccak_hashstate, (void**)&state);
    
    ErlNifBinary out;
    enif_alloc_binary_compat(env, (size_t)(state->fixedOutputLength/8), &out);

    HashReturn r = Final(state, (BitSequence *)out.data);
    if (r == SUCCESS) {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &out));
    } else {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, hash_return_strings[r]));
    }
}

ERL_NIF_TERM keccak_hash(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int bits = 0;
    enif_get_int(env, argv[0], &bits);
    
    ErlNifBinary bin, out;
    enif_inspect_binary(env, argv[1], &bin);
    enif_alloc_binary_compat(env, (size_t)(bits/8), &out);
    
    int bitlength = 0;
    if(!enif_get_int(env, argv[2], &bitlength))
        return enif_make_badarg(env);

    HashReturn r = Hash(bits, (BitSequence *)(bin.data), bitlength, (BitSequence *)out.data);
    if (r == SUCCESS) {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &out));
    } else {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, hash_return_strings[r]));
    }   
}
