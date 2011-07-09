EBIN=ebin
INCLUDE=include
SRC=src

RULES=bnf

all: erl_gen lexer parser

run: all
	erl -pa $(EBIN)

erl_gen:
	erlc -b beam -o $(EBIN) $(SRC)/erl_gen.erl

test: 
	erl -pa $(EBIN) -noshell -run \
	erl_gen test_generate -s init stop;

parser:
	erl -pa $(EBIN) -noshell -run \
	erl_gen make_parser $(INCLUDE)/$(RULES).yrl -s init stop;
	erlc -b beam -o $(EBIN) $(INCLUDE)/$(RULES)_parse.erl 

test_parser:
	erl -pa $(EBIN) -noshell -run \
	erl_gen test_parser -s init stop

lexer:
	erl -pa $(EBIN) -noshell -run \
	erl_gen make_lexer $(INCLUDE)/$(RULES).xrl -s init stop; \
	erlc -b beam -o $(EBIN) $(INCLUDE)/$(RULES).erl 

test_lexer:
	erl -pa $(EBIN) -noshell -run \
	erl_gen test_lexer -s init stop

clean:
	-rm $(EBIN)/*
	-rm $(INCLUDE)/$(RULES).erl
	-rm $(INCLUDE)/$(RULES)_parse.erl

# parser:
# 	erl -nosh

# test: