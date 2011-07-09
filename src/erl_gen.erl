-module (erl_gen).

-export ([make_parser/1,
	  test_parser/0,
	  make_lexer/1,
	  test_lexer/0]).












%%==============================================================================
%% ancillary functions
%%==============================================================================
make_parser (File) ->
    yecc:file (File,[{parserfile, "include/bnf_parse.erl"}]).

make_lexer (File) ->
    leex:file (File).

%% some tests for the lexer
test_lexer () ->
    String1 = "S ::= Adjective Noun;",
    io:format("~p ~n", [bnf:string (String1)]),
    
    String2 = "Adjective ::= \"Business\" | \"Data Access\" | \"Fast Lane\";",
    io:format("~p ~n", [bnf:string (String2)]),
    
    String3 = "S ::= Adjective Noun; S::= Blah Bla",
    io:format("~p ~n", [bnf:string (String3)]).

test_parser () ->
    String1 = "S ::= Adjective Noun;",
    {ok, Tokens, _} = bnf:string (String1),
    {ok, ParseTree} = bnf_parse:parse (Tokens),
    io:format ("~w ~n", [ParseTree]),
    
    String2 = "S ::= Adjective Noun; Adjective ::= \"good\";",
    {ok, Tokens2, _} = bnf:string (String2),
    
%%io:format ("~w ~n", [Tokens2]),
    {ok, ParseTree2} = bnf_parse:parse (Tokens2),
    io:format ("~w ~n", [ParseTree2]).

