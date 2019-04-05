REBAR = rebar3 as test
all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

ct: compile
	$(REBAR) ct

eunit: compile
	$(REBAR) eunit

xref:
	$(REBAR) xref

distclean:
	rm -rf _build
	rm -f data/app.*.config
	rm -f data/vm.*.args

