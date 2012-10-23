all:
	./rebar -v compile

clean:
	./rebar -v clean

eunit:
	./rebar -v eunit