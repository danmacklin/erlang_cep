.PHONY: clean dialyzer_warnings xref_warnings deps test

REBAR=$(PWD)/rebar
RETEST=$(PWD)/deps/retest/retest

clean:
	@rm -rf ebin/*.beam inttest/rt.work rt.work .test

doc:
	@./rebar doc skip_deps=true

deploy:
	./rebar clean compile
	./rebar generate
	chmod +x ./rel/erlang_cep/bin/erlang_cep

run:
	./rel/erlang_cep/bin/erlang_cep console		

check:  dialyzer deps test

xref:
	@./rebar xref

dialyzer: dialyzer_warnings
	@diff -U0 dialyzer_reference dialyzer_warnings

dialyzer_warnings:
	-@dialyzer -q -n ebin -Wunmatched_returns -Werror_handling \
		-Wrace_conditions > dialyzer_warnings

deps:
	@REBAR_EXTRA_DEPS=1 ./rebar get-deps

test:
	@$(REBAR) eunit
