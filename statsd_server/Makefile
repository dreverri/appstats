REBAR := rebar
EBIN := ebin deps/*/ebin lib/appstats/ebin lib/appstats/deps/*/ebin

.PHONY: deps test

all: deps compile

deps:
	$(REBAR) get-deps

compile: deps
	$(REBAR) compile

clean:
	$(REBAR) clean

distclean: clean
	$(REBAR) delete-deps

test: compile
ifdef suite
	$(REBAR) skip_deps=true eunit suite=$(suite)
else
	$(REBAR) skip_deps=true eunit
endif

console: compile
	@erl -pa $(EBIN) -A 64

start: compile
	@erl -pa $(EBIN) -A 64 -run statsd_server
