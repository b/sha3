all:
	./rebar -v get-deps
	./rebar -v compile

clean:
	./rebar -v delete-deps
	./rebar -v clean

eunit:
	./rebar -v eunit
