-module (erl_gen).

-export ([generate/1,
	  test_generate/0,
	  make_parser/1,
	  test_parser/0,
	  make_lexer/1,
	  test_lexer/0]).

generate ([{rule, Axiom, _, _} | Rest] = RulesList) ->
    io:format ("Axiom is ~w ~n~n", [Axiom]),
    generate (RulesList, [Axiom], []).

generate (RulesList, [], Sentence) -> Sentence;

generate (RulesList, [{nonterminal, N} | R], Sentence) ->
    ApplicableRules = [B || {rule, {nonterminal, Nonterminal}, _, B} <- RulesList,
			    Nonterminal == N],

    %%io:format ("Applicable Rules: ~w ~n", [ApplicableRules]),

    lists:foreach (
      fun (E) -> io:format ("~w~n", [E]) end,
      ApplicableRules),

    case length (ApplicableRules) of 1 ->
	    [Rule] = ApplicableRules,
	    %%io:format ("Append:~w ~n", [lists:append (Rule, R)]),
	    generate (RulesList, lists:append (Rule, R), Sentence);
	N ->
	    Number = 1 + random:uniform (N - 1),
	    Rule = get_rule_number (ApplicableRules, Number),
	    generate (RulesList, lists:concat ([Rule, R]), Sentence)
    end;

generate (RulesList, [{terminal, T} | R], Sentence) ->
    generate (RulesList, R, lists:append ([Sentence, remove_quotation_marks (T), " "])).


get_rule_number ([F|R], 0) -> F;
get_rule_number ([F|R], N) -> get_rule_number (R, N - 1).

remove_quotation_marks (String) ->
    lists:filter (fun (C) -> if C == 34 -> false;				  
				true -> true
			     end
		  end,
		  String).
			      
     


%% generate (RulesList, [F|R] = SententialForm , Sentence) ->
%%     io:format ("SententialForm: ~w ~n", [SententialForm]),
%%     io:format ("Sentence: ~w ~n", [Sentence]),
%%     case F of {nonterminal} ->
%% 	    %% look for a substitution
%% 	    generate (RulesList, R, lists:concat ([Sentence, element(2, F)])); %% temporary
%% 	_ ->
%% 	    generate (RulesList, R, lists:concat ([Sentence, element(2, F)]))
%%     end.



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
    {ok, ParseTree2} = bnf_parse:parse (Tokens2),
    io:format ("~w ~n", [ParseTree2]),

    String3 = design_patterns (),
    {ok, Tokens3, _} = bnf:string (String3),
    {ok, ParseTree3} = bnf_parse:parse (Tokens3),
    io:format ("~w ~n", [ParseTree3]).

test_generate () ->
    String = simple_rules (),
    {ok, Tokens, _} = bnf:string (String),
    {ok, ParseTree} = bnf_parse:parse (Tokens),
    io:format ("~w ~n~n", [ParseTree]),
    io:format ("~p ~n", [generate (ParseTree)]).

simple_rules () ->
    "S ::= Adjective Noun; Adjective ::= \"concurrent\"; Noun ::= \"software\";".

design_patterns () ->
    "S ::= Adjective Noun; Adjective ::= \"Business\" | \"Data Access\" | \"Fast Lane\" | \"Composite\" | \"Front\" | \"Intercepting\" | \"Session\" | \"Transfer\" | \"Value List\" | \"View\" | \"Half\" | \"Immutable\" | \"Dynamic\" | \"Virtual\" | \"Model\" | \"Layered\" | \"Null\" | \"Template\"; Noun ::= \"Delegate\" | \"Abstract\" | \"Entity\" | \"View\" | \"Object\" | \"Reader\" | \"Controller\" | \"Filter\" | \"Locator\" | \"Facade\" | \"Handler\" | \"Helper\" | \"Call\" | \"Interface\" | \"Proxy\" | \"Builder\" | \"Method\" | \"Prototype\" | \"Singleton\" | \"Pool\" | \"Marker\" | \"Factory\" | \"Bridge\" | \"Flyweight\" | \"Service\" | \"Adapter\" | \"Listener\" | \"Decorator\" | \"Cache\" | \"Command\" | \"Strategy\" | \"Producer\" | \"Consumer\";".

