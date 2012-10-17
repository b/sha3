-module(hex_tests).

-include_lib("eunit/include/eunit.hrl").

bin_to_hexstr_empty_test() ->
	?assertEqual("", hex:bin_to_hexstr(<<>>)).

bin_to_hexstr_byte_test() ->
	?assertEqual("01", hex:bin_to_hexstr(<<1>>)).

bin_to_hexstr_string_test() ->
	?assertEqual("0123456789ABCDEFFEDCBA9876543210",
		         hex:bin_to_hexstr(<<1,35,69,103,137,171,205,239,254,220,186,152,118,84,50,16>>)).

hexstr_to_bin_empty_test() ->
	?assertEqual(<<>>, hex:hexstr_to_bin("")).

hexstr_to_bin_nibble_test() ->
	?assertEqual(<<16>>, hex:hexstr_to_bin("1")).

hexstr_to_bin_event_nibble_test() ->
	?assertEqual(<<1,2>>, hex:hexstr_to_bin("0102")).

hexstr_to_bin_odd_nibble_test() ->
	?assertEqual(<<16,32>>, hex:hexstr_to_bin("102")).

hexstr_to_bin_string_test() ->
	?assertEqual(<<1,35,69,103,137,171,205,239,254,220,186,152,118,84,50,16>>,
		         hex:hexstr_to_bin("0123456789ABCDEFFEDCBA9876543210")).
