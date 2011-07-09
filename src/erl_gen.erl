-module (erl_gen).

-export ([make_lexer/1,
	  test_lexer/0]).



%% ancillary functions

make_lexer (File) ->
    leex:file (File).

%% some tests for the lexer
test_lexer () ->
    String1 = "S ::= Adjective Noun;",
    io:format("~p ~n", [bnf:string (remove_whitespaces (String1))]),
    
    String2 = "Adjective ::= \"Business\" | \"Data Access\" | \"Fast Lane\";",
    io:format("~p ~n", [bnf:string (remove_whitespaces (String2))]).

%% remove whitespaces from a given string
remove_whitespaces (String) ->
    lists:filter (
      fun (C) -> 
	      io:format ("~p ~n", [C]),
	      case C of 32 -> false;
		  _ -> true
	      end
      end,
      String).
	    
    
