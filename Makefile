all:
	./rebar3 -v get-deps
	./rebar3 -v compile

clean:
	./rebar3 -v delete-deps
	./rebar3 -v clean

eunit:
	./rebar3 -v eunit
