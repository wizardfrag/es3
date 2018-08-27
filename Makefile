.PHONY: all rel run compile dialyzer distclean test
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

# We run the below in sequence to ensure correct startup
# TODO: Unsure if this is necessary?
run:
	$(MAKE) run-1
	@sleep 1
	$(MAKE) run-2
	@sleep 1
	$(MAKE) run-3
	@echo "Done"

run-%:
	./_build/default/rel/es3_$*/bin/es3_$* start
	sleep 1

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