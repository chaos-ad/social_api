all: compile

get-deps:
	./rebar get-deps

compile: get-deps
	./rebar compile

app:
	./rebar compile skip_deps=true

clean:
	./rebar clean
	rm -rfv erl_crash.dump

clean-app:
	${REBAR} clean skip_deps=true
	rm -rfv erl_crash.dump

distclean: clean
	rm -rfv ebin deps

release:
	./rebar generate

