Definitions.
N = [A-Z][a-zA-Z]*
T = "(\.|[^\\"])*"
P = \|
E = ::=

Rules.
{N} : {token, {nonterminal, TokenLine, TokenChars}}.
{T} : {token, {terminal, TokenLine, TokenChars}}.
{P} : {token, {pipe, TokenLine, TokenChars}}.
{E} : {token, {equals, TokenLine, TokenChars}}.

Erlang code.