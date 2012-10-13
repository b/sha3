%% from http://necrobious.blogspot.com/2008/03/binary-to-hex-string-back-to-binary-in.html

-module(hex).
-export([bin_to_hexstr/1,hexstr_to_bin/1]).

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]);
hexstr_to_bin([X|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", lists:flatten([X,"0"])),
  hexstr_to_bin(T, [V | Acc]).
