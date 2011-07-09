Definitions.
N = [A-Z][a-zA-Z]*
T = "(\.|[^\\"])*"
P = \|
E = ::=
W = \s
EOR = ;

Rules.
{N} : {token, {nonterminal, TokenLine, TokenChars}}.
{T} : {token, {terminal, TokenLine, TokenChars}}.
{P} : {token, {pipe, TokenLine, TokenChars}}.
{E} : {token, {equals, TokenLine, TokenChars}}.
{W} : skip_token.
{EOR} : {token, {end_of_rule, TokenLine, TokenChars}}.

Erlang code.