all:
	./rebar3 compile

clean:
	./rebar3 clean --all

eunit:
	./rebar3 eunit
