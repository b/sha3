-module(sha3).
-author('b@b3k.us').

-export([init/1,
         update/2,
         update/3,
         final/1,
         hash/2,
         hash/3,
         hexhash/2]).

-on_load(init/0).

init() ->
    case code:priv_dir(sha3) of
        {error, bad_name} ->
            SoName = filename:join("../priv", "sha3_nifs");
        Dir ->
            SoName = filename:join(Dir, "sha3_nifs")
    end,
    case erlang:load_nif(SoName, 0) of
        ok -> ok;
        {error, {load, _}} -> ok;
        {error, {reload, _}} -> ok;
        {error, {upgrade, _}} -> ok;
        Error -> Error
    end.

-spec init(non_neg_integer()) -> {ok, binary()} | {error, atom()}.
init(_Bits) ->
    "NIF library not loaded".

-spec update(binary(), binary()) -> {ok, binary()} | {error, atom()}.
update(State, Data) -> update(State, Data, bit_size(Data)).

-spec update(binary(), binary(), non_neg_integer()) -> {ok, binary()} | {error, atom()}.
update(_State, _Data, _BitLength) ->
    "NIF library not loaded".

-spec final(binary()) -> {ok, binary()} | {error, atom()}.
final(_State) ->
    "NIF library not loaded".
    
hexhash(Bits, Data) ->
  {ok, Hash} = hash(Bits, Data, bit_size(Data)),
  list_to_binary(hex:bin_to_hexstr(Hash)).

-spec hash(non_neg_integer(), binary()) -> {ok, binary()} | {error, atom()}.
hash(Bits, Data) -> hash(Bits, Data, bit_size(Data)).

-spec hash(non_neg_integer(), binary(), non_neg_integer()) -> {ok, binary()} | {error, atom()}.
hash(_Bits, _Data, _BitLength) ->
    "NIF library not loaded".
