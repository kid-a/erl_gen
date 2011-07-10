erl_gen: a random content generator for Erlang[1]
=================================================

0. erl_gen for the impatient
----------------------------
Type in your terminal:

$ make run

You will be provided an Erlang shell. Type:

1> erl_gen:start_link ().
2> erl_gen:generate (london_tube_status).

Et voila`, you've been served. As long as you will invoke the 
erl_gen:generate (london_tube_status) function, you will be provided with a 
random 'tube update' message.

Tired of London tube updates? Try with 

erl_gen:generate (dan_brown).
erl_gen:generate (design_patterns).



1. Description
--------------

erl_gen generates random sentences according to a given context free grammar (CFG),
expressed in Backus-Naur form (BNF).

More formally, given a well-formed CFG, it picks up the axiom and 
performs a sequence of one-step leftmost derivations, until it gets a complete 
sentence.

Grammars for erl_gen must comply with the following rules:
  - nonterminals are string literals with the first character uppercase (e.g. Nonterminal)
  - terminals are strings surrounded by quotation marks (e.g. "this is a string")
  - all rules must be in the form 'Nonterminal ::= <body>;', 
    where <body> can be any sequence of nonterminals, teminals and pipes ('|')

An example will make things clearer. 
Suppose you have defined the following CFG:

    S ::= Adjective Noun;
    Adjective ::= "good" | "bad";
    Noun ::= "boy" | "girl";


The following are all feasible sequences of one-step leftmost derivations for this 
grammar is ('->' means 'derives in one step')


    S -> Adjective Noun -> "good" Noun -> "good" "boy"
    S -> Adjective Noun -> "bad" Noun -> "bad" "boy"
    S -> Adjective Noun -> "good" Noun -> "good" "girl"
    S -> Adjective Noun -> "bad" Noun -> "bad" "girl"


2. Getting erl_gen up and running
---------------------------------

All you have to do is type in your terminal:

    $ make run

The Erlang shell will be started. Then, type:

    > erl_gen:start_link ().
    > erl_gen:set_grammar (<grammar path>).
    > erl_gen:generate ().

where <grammar path> is the path to the file containing the CFG specification.

erl_gen comes with three sample grammars (all available under the priv/ directory).



3. Behind the hood
------------------

erl_gen relies on leex and yecc to parse the CFG.
The include/ directory contains the specification files, namely bnf.xrl and 
bnf.yrl, to generate the lexical analyzer and the parser.

If one whishes to change any of the specification files, it has also to 
re-generate the lexical analyzer and/or the parser, with:

    $ make lexer
    $ make parser
 
These Makefile targets relies on the following two functions exported by the
erl_gen module:

make_lexer/1
make_parser/1 

The output of the parser is an Abstract Syntax Tree (AST), representing the 
parsed CFG. The AST is represented by means of a list of rules, e.g.:

    [
     {rule, {nonterminal, ...}, {equals, ...} [ <rule body> ]
     ...
    ]

The first rule contained in the CFG specification is taken as the grammar's 
axiom. 
When asked for a random sentence, erl_gen starts the body of the axiom, then
selects the leftmost appearing nonterminal and reduces it by means of the 
body of one of the rules in which the nonterminal appears as its head, 
until no nonterminals appears in the sentential form.

The choice of the rule to apply is driven by crypto:uniform/2.


4. Why did you do that?
-----------------------

Because:
  1. i wanted to have fun
  2. i wanted to have a lot of fun
  3. i wanted to have really a lot of fun :D

erl_gen has been largely inspired by polygen (http://www.polygen.org).

Although erl_gen can be seen as a (really?) serious program, it gives its best 
when used as a tool to make parodies about habits and stereotypes.
That is, you can focus on a ridiculous theme, extract its rules and its patterns,
then reproduce it infinite times!


5. Contact me
-------------

You have seen how i waste my spare time: writing futile and stupid software.
Do you really want to contact me?






















Really?
























You sure?


























Ok, here is my mail address: loris [dot] fichera [at] gmail [dot] com


[1] wait... why did you name the repository erlspike?
Well... because i changed my mind overnight about what i was going to do. :)

