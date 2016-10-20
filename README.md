### keccak

keccak is a NIF wrapper around SHA-3/KECCAK cryptographic hashing function

### Quick Start

You must have Erlang/OTP R14B or later and a GNU-style build system to compile and run sha3.

	git clone git://github.com/b/sha3.git
	cd sha3
	make

Start up an Erlang shell with the path to sha3 included.

	cd path/to/sha3/ebin
	erl

Hash a binary by calling sha3:hash/2 with the desired number of bits for the resulting hash:

	1> Bits = 256.
	256
	2> Data = <<"foobarbazquux">>.
	<<"foobarbazquux">>
	3> {ok, Hash} = sha3:hash(Bits, Data).
	{ok,<<44,116,195,124,154,54,203,192,209,31,49,133,80,8,
	  197,88,60,97,25,146,40,128,231,75,121,226,1,...>>}
	4> bit_size(Hash).
	256

Supported hash bit lengths are 224, 256, 384, and 512.

You may find sha3:hexhash/2 more useful, as it returns a hexadecimal-encoded string representing the hash:

	5> HexHash = sha3:hexhash(Bits, Data).
	<<"2C74C37C9A36CBC0D11F31855008C5583C6119922880E74B79E2014A28F862DA">>

Alternatively, you might want to incrementally hash a longer message:

	6> Bits = 256.
	256
	7> Data1 = <<"foobar">>.
	<<"foobar">>
	8> Data2 = <<"bazquux">>.
	<<"bazquux">>
	9> {ok, Handle} = sha3:init(Bits).
	{ok,<<>>}
	10> {ok, Handle} = sha3:update(Handle, Data1).
	{ok,<<>>}
	11> {ok, Handle} = sha3:update(Handle, Data2).
	{ok,<<>>}
	12> {ok, Hash} = sha3:final(Handle).
	{ok,<<44,116,195,124,154,54,203,192,209,31,49,133,80,8,
	      197,88,60,97,25,146,40,128,231,75,121,226,1,...>>}
	13> bit_size(Hash).
	256

### The SHA-3 Hash

The underlying hashing code in sha3 is the reference implementation of KECCAK, now SHA-3, from the official NIST submission.

Details on the algorithm as submitted and known analysis can be found at http://keccak.noekeon.org/.
