
% Erlang with Types

Terminals '(' ')' '->' ',' '.' 'fun' cat product sum '=' var atom 'end'.

Nonterminals program types type fun_ctor cat_ctor sum_ctor rec_ctor type_args value_args fun_body exp match bind.

Rootsymbol program.

program -> types : '$1'.

types -> '$empty' : [].
types -> type ',' types : ['$1' | '$3'].
types -> type : ['$1'].

type -> sum_ctor : '$1'.
type -> fun_ctor : '$1'.
type -> cat_ctor : '$1'.
type -> rec_ctor : '$1'.

fun_ctor -> 'fun' '(' value_args ')' '->' fun_body 'end' : #'fun'     { type='$3', value='$6' }.
cat_ctor -> 'cat' '(' type_args ')' '->' types 'end'       : #'cat'     { type='$3', value='$6' }.
rec_ctor -> 'product' '(' types ')'                  : #'product' { type='$3', value=[] }.
sum_ctor -> 'sum' '(' types ')'                      : #'sum'     { type='$3',   value=[] }.

type_args  -> exp : '$1'.
value_args -> exp : '$1'.
fun_body   -> exp : '$1'.
fun_body   -> exp ',' fun_body : ['$1'|'$3'].

exp -> match : '$1'.
exp -> bind : '$1'.
exp -> '$empty' : [].

match -> bind '=' exp : #bind{ var = '$1', value = '$3'}.

bind -> var : '$1'.
bind -> atom : '$1'.

Erlang code.

-include("ter.hrl").
