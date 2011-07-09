-module (erl_gen).

-behaviour (gen_server).

-record (state, { grammar = nil }).

-export ([start_link/0,
	  set_grammar/1,
	  generate/0,

	  init/1,
	  handle_call/3,
	  terminate/2,
	  

	  test_generate/0,
	  make_parser/1,
	  test_parser/0,
	  make_lexer/1,
	  test_lexer/0]).

%%==============================================================================
%% api
%%==============================================================================
start_link () -> 
    gen_server:start_link({local, erl_gen}, ?MODULE, [], []).

set_grammar (File) ->
    gen_server:call (erl_gen, {set_grammar, File}).

generate () ->
    gen_server:call (erl_gen, {generate}).

%%==============================================================================
%% callbacks
%%==============================================================================
init ([]) -> {ok, #state{}}.

handle_call ({set_grammar, File}, _From, State) ->
    io:format ("A"),
    {ok, G} = file:read_file (File),
    io:format ("B"),
    GrammarContent = binary_to_list (G),
    {ok, Tokens, _} = bnf:string (GrammarContent),
    {ok, AST} = bnf_parse:parse (Tokens),    
    NewState = #state { grammar = AST },
    {reply, ok, NewState};

handle_call ({generate}, _From, State) ->
    Sentence = generate (State#state.grammar),
    {reply, Sentence, State}.

terminate (_Reason, _State) -> ok.
     
%%==============================================================================
%% ancillary functions
%%==============================================================================
generate ([{rule, Axiom, _, _} | Rest] = RulesList) ->
    generate (RulesList, [Axiom], []).

generate (RulesList, [], Sentence) -> Sentence;

generate (RulesList, [{nonterminal, N} | R], Sentence) ->
    AR = [B || {rule, {nonterminal, Nonterminal}, _, B} <- RulesList,
	       Nonterminal == N],

    %%io:format ("Applicable Rules before: ~p ~n", [AR]),
    ApplicableRules = alternatives (AR),
    %%io:format ("Applicable Rulesa after: ~p ~n",[ApplicableRules]),
    
    %% lists:foreach (
    %%   fun (E) -> io:format ("~w~n", [E]) end,
    %%   ApplicableRules),
    
    %%io:format ("~n~n ~p ~n~n", [length(ApplicableRules)]),

    case length (ApplicableRules) of 1 ->
	    [Rule] = ApplicableRules,
	    %%io:format ("Append:~w ~n", [lists:append (Rule, R)]),
	    generate (RulesList, lists:append (Rule, R), Sentence);
	_ ->
	    Number = random:uniform (length (ApplicableRules) - 1),
	    Rule = get_rule_number (ApplicableRules, Number),
	    %%io:format ("Rule to Apply: ~p ~n", [Rule]),
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

alternatives ([], Accumulator) -> Accumulator;
alternatives (Body, Accumulator) -> 
    case lists:any (fun (S) -> S == pipe end, Body) of false ->
	    [Body | Accumulator];
	_ ->
	    Alternative = lists:takewhile (fun (S) -> 
						   case S of pipe -> false;
						       _ -> true
						   end
					   end, Body),
	    
	    %%io:format ("Remaining: ~p ~n", [lists:nthtail (length (Alternative) + 1, Body)]),
	    
	    alternatives ( lists:nthtail (length (Alternative) + 1, Body), 
			   [Alternative | Accumulator])
    end.
   


alternatives (Bodies) ->
    lists:foldl (fun (Body, Acc) -> lists:append (alternatives (Body, []), Acc) end,
		 [],
		 Bodies).

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
    String = design_patterns (),
    %%String = design_patterns_no_or (),
    {ok, Tokens, _} = bnf:string (String),
    {ok, ParseTree} = bnf_parse:parse (Tokens),
    %%io:format ("~w ~n~n", [ParseTree]),

    io:format ("~p ~n", [generate (ParseTree)]),
    io:format ("~p ~n", [generate (ParseTree)]),
    io:format ("~p ~n", [generate (ParseTree)]),
    io:format ("~p ~n", [generate (ParseTree)]).
    

simple_rules () ->
    "S ::= Adjective Noun; Adjective ::= \"stupid\"; Noun ::= \"software\";".

design_patterns () ->
    "S ::= Adjective Noun; Adjective ::= \"Business\" | \"Data Access\" | \"Fast Lane\" | \"Composite\" | \"Front\" | \"Intercepting\" | \"Session\" | \"Transfer\" | \"Value List\" | \"View\" | \"Half\" | \"Immutable\" | \"Dynamic\" | \"Virtual\" | \"Model\" | \"Layered\" | \"Null\" | \"Template\"; Noun ::= \"Delegate\" | \"Abstract\" | \"Entity\" | \"View\" | \"Object\" | \"Reader\" | \"Controller\" | \"Filter\" | \"Locator\" | \"Facade\" | \"Handler\" | \"Helper\" | \"Call\" | \"Interface\" | \"Proxy\" | \"Builder\" | \"Method\" | \"Prototype\" | \"Singleton\" | \"Pool\" | \"Marker\" | \"Factory\" | \"Bridge\" | \"Flyweight\" | \"Service\" | \"Adapter\" | \"Listener\" | \"Decorator\" | \"Cache\" | \"Command\" | \"Strategy\" | \"Producer\" | \"Consumer\";".

