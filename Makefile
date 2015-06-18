
REBAR = ~/local/bin/rebar
ERL = erl -pa ebin -pa deps/exml/ebin
DIALYZE_SH = dialysis/dialyze.sh
RM = rm -f

all: compile


get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

exp_test: compile
	$(ERL) -s exp_test test

run: compile
	$(ERL)

run-1:
	$(ERL) -s exp_test render_iq

dialyze: compile
	$(DIALYZE_SH)

dialyze-clean:
	$(RM) dialysis/*.plt
	$(RM) dialysis/*.plt.log