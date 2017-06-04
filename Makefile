all:
	./rebar3 compile

clean:
	rm -rf _build
	./rebar3 clean

eunit:
	./rebar3 eunit
