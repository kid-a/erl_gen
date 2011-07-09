EBIN=ebin
INCLUDE=include
SRC=src

LEXER_RULES=bnf

erl_gen:
	erlc -b beam -o $(EBIN) $(SRC)/erl_gen.erl

lexer:
	erl -pa $(EBIN) -noshell -run \
	erl_gen make_lexer $(INCLUDE)/$(LEXER_RULES).xrl -s init stop; \
	erlc -b beam -o $(EBIN) $(INCLUDE)/$(LEXER_RULES).erl 

test_lexer:
	erl -pa $(EBIN) -noshell -run \
	erl_gen test_lexer -s init stop

clean:
	-rm $(EBIN)/*
	-rm $(INCLUDE)/$(LEXER_RULES).erl

# parser:
# 	erl -nosh

# test: