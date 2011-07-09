Nonterminals grammar rule rule_attach body elem body_attach.
Terminals terminal nonterminal pipe equals end_of_rule.
Rootsymbol grammar.

grammar -> rule rule_attach : {grammar, '$1', '$2'}.
rule_attach -> grammar : ['$1'].
rule_attach -> '$empty'.
rule -> nonterminal equals body : {rule, '$1', '$2'}.
body -> elem body_attach : ['$1', '$2'].
body_attach -> body : ['$1'].
body_attach -> '$empty'.
elem -> terminal : {'$1'}.
elem -> nonterminal : {'$1'}.
elem -> pipe : {'$1'}.
elem -> end_of_rule.
