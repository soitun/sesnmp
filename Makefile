all:
	./rebar compile

dist:
	rm -rf rel/sesnmp
	./rebar generate

clean:
	./rebar clean
