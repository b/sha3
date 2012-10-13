-module(sha3_tests).

-include_lib("eunit/include/eunit.hrl").

test_hash(Bits, [Len, Msg, Md]) ->
    {ok, Digest} = sha3:hash(Bits, hex:hexstr_to_bin(Msg), Len),
    ?assertEqual(Md, string:to_upper(hex:bin_to_hexstr(Digest))).

test_lifecycle(Bits, [Len, Msg, Md]) ->
   {ok, State} = sha3:init(Bits),
   {ok, UpdateState} = sha3:update(State, hex:hexstr_to_bin(Msg), Len),
   {ok, Digest} = sha3:final(UpdateState),
   ?assertEqual(Md, string:to_upper(hex:bin_to_hexstr(Digest))).

parse_triples(Lines) ->
    parse_triples(Lines, []).

parse_triples([], Acc) ->
    Acc;
parse_triples([Len, Msg, Md|Lines], Acc) ->
    ["Len", "=", Length] = string:tokens(Len, " "),
    ["Msg", "=", Message] = string:tokens(Msg, " "),
    ["MD", "=", Digest] = string:tokens(Md, " "),
    parse_triples(Lines, [[list_to_integer(Length), Message, Digest]|Acc]).

msgkat(Set, Bits, Fun) ->
    {ok, Cwd} = file:get_cwd(),
    Filename = filename:join([Cwd, "..", "test", "data", Set ++ "MsgKAT.zip"]),
    {ok, ZipHandle} = zip:zip_open(Filename, [memory]),
    {ok, {_, Data}} = zip:zip_get(Set ++ "MsgKAT_" ++ integer_to_list(Bits) ++ ".txt", ZipHandle),
    Lines = lists:filter(
                fun(Line) ->
                    case Line of
                        "L" ++ _ -> true;
                        "M" ++ _ -> true;
                        _ -> false
                    end
                end, string:tokens(binary_to_list(Data), "\n")),

    lists:foreach(
        fun(T) ->
            Fun(Bits, T)
        end, parse_triples(Lines)).

shortmsgkat_224_hash_test_() -> {timeout, 60, fun() -> msgkat("Short", 224, fun test_hash/2) end}.
shortmsgkat_256_hash_test_() -> {timeout, 60, fun() -> msgkat("Short", 256, fun test_hash/2) end}.
shortmsgkat_384_hash_test_() -> {timeout, 60, fun() -> msgkat("Short", 384, fun test_hash/2) end}.
shortmsgkat_512_hash_test_() -> {timeout, 60, fun() -> msgkat("Short", 512, fun test_hash/2) end}.

shortmsgkat_224_lifecycle_test_() -> {timeout, 60, fun() -> msgkat("Short", 224, fun test_lifecycle/2) end}.
shortmsgkat_256_lifecycle_test_() -> {timeout, 60, fun() -> msgkat("Short", 256, fun test_lifecycle/2) end}.
shortmsgkat_384_lifecycle_test_() -> {timeout, 60, fun() -> msgkat("Short", 384, fun test_lifecycle/2) end}.
shortmsgkat_512_lifecycle_test_() -> {timeout, 60, fun() -> msgkat("Short", 512, fun test_lifecycle/2) end}.

longmsgkat_224_hash_test_() -> {timeout, 60, fun() -> msgkat("Long", 224, fun test_hash/2) end}.
longmsgkat_256_hash_test_() -> {timeout, 60, fun() -> msgkat("Long", 256, fun test_hash/2) end}.
longmsgkat_384_hash_test_() -> {timeout, 60, fun() -> msgkat("Long", 384, fun test_hash/2) end}.
longmsgkat_512_hash_test_() -> {timeout, 60, fun() -> msgkat("Long", 512, fun test_hash/2) end}.

longmsgkat_224_lifecycle_test_() -> {timeout, 60, fun() -> msgkat("Long", 224, fun test_lifecycle/2) end}.
longmsgkat_256_lifecycle_test_() -> {timeout, 60, fun() -> msgkat("Long", 256, fun test_lifecycle/2) end}.
longmsgkat_384_lifecycle_test_() -> {timeout, 60, fun() -> msgkat("Long", 384, fun test_lifecycle/2) end}.
longmsgkat_512_lifecycle_test_() -> {timeout, 60, fun() -> msgkat("Long", 512, fun test_lifecycle/2) end}.
