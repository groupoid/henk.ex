-module(om_tok).
-compile(export_all).
-define(is_space(C), C =:= $\r; C =:= $\s; C =:= $\t).
-define(is_alpha(C), C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9; C=:=$@; C=:=$#).
-define(is_termi(C), C =:= $!; C =:= $$; C =:= $%; C =:= $&; C =:= $(; C =:= $:;
                     C =:= $+; C =:= $-; C =:= $*; C =:= $/; C =:= $.; C =:= $\\;C =:= $);
                     C =:= $<; C =:= $>; C =:= $=; C =:= $|; C =:= $^; C =:= $~).

% om parser depends on three functions:

rev(X)      -> lists:reverse(X).
flat(X)     -> lists:flatten(X).
tokens(X,Y) -> string:tokens(X,Y).

tokens(<<>>,                  _, {_,C}, Acc) -> lists:reverse(stack(C,Acc));
tokens(<<$\n,     R/binary>>, L, {_,C}, Acc) -> tokens(R,L+1,{1,[]},   stack(C,Acc));
tokens(<<$(,      R/binary>>, L, {t,_}, Acc) -> tokens(R,L,{t,[$(]},   Acc);
tokens(<<$),      R/binary>>, L, {t,C}, Acc) -> tokens(R,L,{t,[$)|C]}, Acc);
tokens(<<$(,      R/binary>>, L, {_,C}, Acc) -> tokens(R,L,{t,[]},     [open   | stack(C,  Acc)]);
tokens(<<$),      R/binary>>, L, {_,C}, Acc) -> tokens(R,L,{t,[]},     [close  | stack(C,  Acc)]);
tokens(<<$:,      R/binary>>, L, {_,C}, Acc) -> tokens(R,L,{1,[]},     [colon  | stack(C,  Acc)]);
tokens(<<$*,      R/binary>>, L, {_,C}, Acc) -> tokens(R,L,{1,[]},     [star   | stack(C,  Acc)]);
tokens(<<"→"/utf8,R/binary>>, L, {_,C}, Acc) -> tokens(R,L,{1,[]},     [arrow  | stack(C,  Acc)]);
tokens(<<"λ"/utf8,R/binary>>, L, {_,C}, Acc) -> tokens(R,L,{1,[]},     [lambda | stack(C,  Acc)]);
tokens(<<"∀"/utf8,R/binary>>, L, {_,C}, Acc) -> tokens(R,L,{1,[]},     [pi     | stack(C,  Acc)]);
tokens(<<X,       R/binary>>, L, {a,C}, Acc) when ?is_alpha(X) -> tokens(R,L,{a,[X|C]},            Acc);
tokens(<<X,       R/binary>>, L, {_,C}, Acc) when ?is_alpha(X) -> tokens(R,L,{a,[X]},    stack([C],Acc));
tokens(<<X,       R/binary>>, L, {t,C}, Acc) when ?is_termi(X) -> tokens(R,L,{t,[X|C]},            Acc);
tokens(<<X,       R/binary>>, L, {_,C}, Acc) when ?is_termi(X) -> tokens(R,L,{t,[X]},    stack(C, [Acc]));
tokens(<<X,       R/binary>>, L, {_,C}, Acc) when ?is_space(X) -> tokens(R,L,{s,[C]},              Acc).

stack(C,Acc) -> case rev(flat(C)) of []  -> Acc;
                                     "(" -> [open|Acc];
                                     ")" -> [close|Acc];
                                  [$#|A] -> inet(A,Acc);
                 [X|A] when ?is_alpha(X) -> vars([X|A],Acc);
                 [X|A] when ?is_termi(X) -> name([X|A],Acc);
                                      X  -> atom(X,Acc) end.

inet(X,Acc) -> [{name,{X,0}}|Acc].
atom(X,Acc) -> [list_to_atom(X)|Acc].
name(X,Acc) -> [{name,{X,0}}|Acc].
ivar([N,I]) -> [N,I];
ivar([N])   -> [N,"0"].
vars(X,Acc) -> [Name,Index]= ivar(tokens(X,"@")),
               [{name,{list_to_atom(Name),list_to_integer(Index)}}|Acc].
