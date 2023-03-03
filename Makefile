all:
	./rebar3 compile

clean:
	./rebar3 clean

clean-deps: clean
	rm -rf _build

eunit:
	./rebar3 eunit
