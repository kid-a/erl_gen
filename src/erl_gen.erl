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
    io:format("~p ~n", [bnf:string (remove_whitespaces (String1))]),
    
    String2 = "Adjective ::= \"Business\" | \"Data Access\" | \"Fast Lane\";",
    io:format("~p ~n", [bnf:string (remove_whitespaces (String2))]).

test_parser () ->
    String1 = "S ::= Adjective Noun;",
    {ok, Tokens, _} = bnf:string (remove_whitespaces (String1)),
    io:format ("~w ~n", 
	       [bnf_parse:parse (Tokens)]).

%% remove whitespaces from a given string
remove_whitespaces (String) ->
    lists:filter (
      fun (C) -> 
	      case C of 32 -> false;
		  _ -> true
	      end
      end,
      String).
