.PHONY: all rel start compile dialyzer distclean test eunit ct start stop
REBAR ?= rebar3
HOSTNAME ?= $(shell hostname -s)

all: compile dialyzer rel

compile:
	$(REBAR) compile

dialyzer:
	$(REBAR) dialyzer

rel: config/vars.config rel-es3_1 rel-es3_2 rel-es3_3

rel-%:
	$(REBAR) release -n $*

start: start-1 start-2 start-3

start-%:
	HTTP_PORT=898$* ./_build/default/rel/es3_$*/bin/es3_$* start

stop: stop-1 stop-2 stop-3

stop-%:
	-./_build/default/rel/es3_$*/bin/es3_$* stop

distclean:
	$(REBAR) clean
	rm -Rf _build

test: eunit ct

eunit:
	$(REBAR) eunit

ct: config/test.config
	$(REBAR) ct --sname ct_master --setcookie ct_cookie

config/%.config: config/%.config.src
	sed -e "s/{{hostname}}/$(HOSTNAME)/g" config/$*.config.src > config/$*.config