Nonterminals grammar rule rule_attach body elem body_attach.
Terminals terminal nonterminal pipe equals end_of_rule.
Rootsymbol grammar.

grammar -> rule rule_attach.
rule_attach -> grammar.
rule_attach -> '$empty'.
rule -> nonterminal equals body.
body -> elem body_attach.
body_attach -> body.
body_attach -> '$empty'.
elem -> terminal.
elem -> nonterminal.
elem -> pipe.
elem -> end_of_rule.
