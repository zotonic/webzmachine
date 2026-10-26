ERL          ?= erl
APP          := webzmachine

REBAR := ./rebar3
REBAR_URL := https://github.com/erlang/rebar3/releases/download/3.27.3/rebar3
REBAR_SHA256 := f1d87d78decfc56b87d971c75ee6355698ec0c49fadd57f2c056043b7ca291b5
REBAR_STAMP := ./.rebar3-$(REBAR_SHA256)

.PHONY: deps

all: deps
	$(REBAR) compile 

$(REBAR_STAMP):
	rm -f ./.rebar-*
	touch $@
	rm -f $(REBAR)

$(REBAR): $(REBAR_STAMP)
	./tools/download.escript "$(REBAR_URL)" "$(REBAR)" --sha256 "$(REBAR_SHA256)" --executable

deps: $(REBAR)
	$(REBAR) get-deps

clean: $(REBAR)
	$(REBAR) clean

distclean: $(REBAR) clean
	$(REBAR) delete-deps 

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: all
	$(REBAR) eunit 

