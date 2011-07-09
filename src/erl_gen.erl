-module (erl_gen).

-export ([make_lexer/1]).



%% ancillary functions

make_lexer (File) ->
    leex:file (File).
