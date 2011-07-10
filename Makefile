EBIN=ebin
INCLUDE=include
SRC=src

RULES=bnf

BEAM=erlc -b beam -o $(EBIN)

OBJS= \
erl_gen \
bnf \
bnf_parse

ERL_OBJS=$(foreach file, $(OBJS), $(EBIN)/$(file).beam)

run: $(ERL_OBJS)
	erl -pa $(EBIN)

$(EBIN)/%.beam: $(SRC)/%.erl
	$(BEAM) $<

test: $(ERL_OBJS)
	erl -pa $(EBIN) -noshell -run \
	erl_gen test_generate -s init stop;

parser: $(EBIN)/erl_gen.beam
	erl -pa $(EBIN) -noshell -run \
	erl_gen make_parser $(INCLUDE)/$(RULES).yrl -s init stop; \
	mv $(INCLUDE)/$(RULES)_parse.erl $(SRC)/$(RULES)_parse.erl; \
	erlc -b beam -o $(EBIN) $(SRC)/$(RULES)_parse.erl;


test_parser: $(EBIN)/erl_gen.beam lexer parser
	erl -pa $(EBIN) -noshell -run \
	erl_gen test_parser -s init stop

lexer: $(EBIN)/erl_gen.beam
	erl -pa $(EBIN) -noshell -run \
	erl_gen make_lexer $(INCLUDE)/$(RULES).xrl -s init stop; \
	mv $(INCLUDE)/$(RULES).erl $(SRC)/$(RULES).erl; \
	erlc -b beam -o $(EBIN) $(SRC)/$(RULES).erl; \


test_lexer: $(EBIN)/erl_gen.beam lexer
	erl -pa $(EBIN) -noshell -run \
	erl_gen test_lexer -s init stop

clean:
	-rm $(EBIN)/*
	-rm $(INCLUDE)/$(RULES).erl
	-rm $(INCLUDE)/$(RULES)_parse.erl