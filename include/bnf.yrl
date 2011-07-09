Nonterminals grammar rule rule_attach body elem body_attach.
Terminals terminal nonterminal pipe equals end_of_rule.
Rootsymbol grammar.

% grammar -> rule rule_attach : {grammar, '$1', '$2'}.
% rule_attach -> grammar : ['$1'].
% rule_attach -> '$empty'.
% rule -> nonterminal equals body : {rule, '$1', '$2'}.
% body -> elem body_attach : ['$1', '$2'].
% body_attach -> body : ['$1'].
% body_attach -> '$empty'.
% elem -> terminal : {'$1'}.
% elem -> nonterminal : {'$1'}.
% elem -> pipe : {'$1'}.
% elem -> end_of_rule.

grammar -> rule rule_attach : flatten (['$1', '$2']).
rule_attach -> grammar : '$1'.
rule_attach -> '$empty' : [].
rule -> nonterminal equals body : {rule, {type_of ('$1'), value_of ('$1')}, '$2', '$3'}.
body -> elem body_attach : flatten (['$1', '$2']).
body_attach -> body : '$1'.
body_attach -> end_of_rule : [].
elem -> terminal : {type_of ('$1'), value_of ('$1')}.
elem -> nonterminal : {type_of ('$1'), value_of ('$1')}.
elem -> pipe : type_of('$1').
%%elem -> end_of_rule : [].

Erlang code.
flatten (L) -> 
  case L of nil -> [];
       _ -> lists:flatten (L)
  end. 

type_of (Token) ->
  element (1, Token).

value_of (Token) ->
  element (3, Token).