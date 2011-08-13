all: compile

get-deps:
	./rebar get-deps

compile:
	./rebar compile

clean:
	./rebar clean

release:
	./rebar generate

