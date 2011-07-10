%% @author Loris Fichera <loris.fichera@gmail.com>
%% @doc The infamous erl_gen program.
-module (erl_gen).

-behaviour (gen_server).

-record (state, { grammar = nil }).

-export ([start_link/0,
	  set_grammar/1,
	  generate/0,
	  generate/1,

	  init/1,
	  handle_call/3,
	  terminate/2,

	  make_parser/1,
	  make_lexer/1,

	  test_generate/0,
	  test_parser/0,
	  test_lexer/0]).

%%==============================================================================
%% api
%%==============================================================================
%%------------------------------------------------------------------------------
%% Func: start_link/0
%% @doc Starts the server.
%%------------------------------------------------------------------------------
start_link () -> gen_server:start_link({local, erl_gen}, ?MODULE, [], []).


%%------------------------------------------------------------------------------
%% Func: set_grammar/1
%% @doc Give erl_gen the path to the file containing the CFG.
%%      erl_gen parses the file and saves the CFG in its state.
%%      If the CFG is not well-formed, erl_gen CRASHES.
%%
%% Parameters:
%%   File :: string ()
%%
%% Reply:
%%   ok
%%------------------------------------------------------------------------------
set_grammar (File) -> gen_server:call (erl_gen, {set_grammar, File}).


%%------------------------------------------------------------------------------
%% Func: generate/0
%% @doc Returns a randomly-derived sentence from the CFG contained in 
%%      erl_gen state.
%%
%% Reply:
%%   Sentence :: string ()
%%------------------------------------------------------------------------------
generate () -> gen_server:call (erl_gen, {generate}).

%%------------------------------------------------------------------------------
%% Func: generate/1
%% @doc Returns a randomly-derived sentence from the CFG indicated by the
%%      passed parameter.
%%
%% Parameters:
%%   CFG :: dan_brown | london_tube_status | design_patterns
%%
%% Reply:
%%   Sentence :: string ()
%%------------------------------------------------------------------------------
generate (dan_brown) -> 
    gen_server:call (erl_gen, {set_grammar, "priv/dan_brown.grm"}),
    gen_server:call (erl_gen, {generate});

generate (london_tube_status) ->
    gen_server:call (erl_gen, {set_grammar, "priv/london_tube_status.grm"}),
    gen_server:call (erl_gen, {generate});

generate (design_patterns) ->
    gen_server:call (erl_gen, {set_grammar, "priv/oo_design_patterns.grm"}),
    gen_server:call (erl_gen, {generate}).


%%==============================================================================
%% callbacks
%%==============================================================================
init ([]) -> {ok, #state{}}.


handle_call ({set_grammar, File}, _From, State) ->
    {ok, G} = file:read_file (File),
    GrammarContent = binary_to_list (G),
    {ok, Tokens, _} = bnf:string (GrammarContent),
    {ok, AST} = bnf_parse:parse (Tokens),    
    NewState = #state { grammar = AST },
    {reply, ok, NewState};

handle_call ({generate}, _From, State) ->
    Sentence = generate_a (State#state.grammar),
    {reply, Sentence, State}.


terminate (_Reason, _State) -> ok.

   
%%==============================================================================
%% ancillary functions
%%==============================================================================
generate_a ([{rule, Axiom, _, _} | Rest] = RulesList) ->
    generate_a (RulesList, [Axiom], []).


generate_a (RulesList, [], Sentence) -> Sentence;
generate_a (RulesList, [{nonterminal, N} | R], Sentence) ->
    AR = [B || {rule, {nonterminal, Nonterminal}, _, B} <- RulesList,
	       Nonterminal == N],
    ApplicableRules = alternatives (AR),
    
    case length (ApplicableRules) of 1 ->
	    [Rule] = ApplicableRules,
	    generate_a (RulesList, lists:append (Rule, R), Sentence);
	_ ->
	    %%Number = random:uniform (length (ApplicableRules)),
	    Number = crypto:rand_uniform (1, length (ApplicableRules) + 1),
	    Rule = lists:nth (Number, ApplicableRules),
	    generate_a (RulesList, lists:concat ([Rule, R]), Sentence)
    end;
generate_a (RulesList, [{terminal, T} | R], Sentence) ->
    generate_a (RulesList, R, lists:append ([Sentence, remove_quotation_marks (T), " "])).


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

%% some tests for the parser
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

    io:format ("~p ~n", [generate_a (ParseTree)]),
    io:format ("~p ~n", [generate_a (ParseTree)]),
    io:format ("~p ~n", [generate_a (ParseTree)]),
    io:format ("~p ~n", [generate_a (ParseTree)]).
    

simple_rules () ->
    "S ::= Adjective Noun; Adjective ::= \"stupid\"; Noun ::= \"software\";".

design_patterns () ->
    "S ::= Adjective Noun; Adjective ::= \"Business\" | \"Data Access\" | \"Fast Lane\" | \"Composite\" | \"Front\" | \"Intercepting\" | \"Session\" | \"Transfer\" | \"Value List\" | \"View\" | \"Half\" | \"Immutable\" | \"Dynamic\" | \"Virtual\" | \"Model\" | \"Layered\" | \"Null\" | \"Template\"; Noun ::= \"Delegate\" | \"Abstract\" | \"Entity\" | \"View\" | \"Object\" | \"Reader\" | \"Controller\" | \"Filter\" | \"Locator\" | \"Facade\" | \"Handler\" | \"Helper\" | \"Call\" | \"Interface\" | \"Proxy\" | \"Builder\" | \"Method\" | \"Prototype\" | \"Singleton\" | \"Pool\" | \"Marker\" | \"Factory\" | \"Bridge\" | \"Flyweight\" | \"Service\" | \"Adapter\" | \"Listener\" | \"Decorator\" | \"Cache\" | \"Command\" | \"Strategy\" | \"Producer\" | \"Consumer\";".

