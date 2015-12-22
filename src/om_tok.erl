-module(om_tok).
-description('Om Tokenizer').
-compile(export_all).
-define(is_space(C), C==$\r; C==$\s; C==$\t).
-define(is_alpha(C), C>=$a,  C=<$z;  C>=$A,  C=<$Z;  C>=$0,  C=<$9;  C==$@;  C==$#).
-define(is_termi(C), C==$!;  C==$$;  C==$%;  C==$&;  C==$(;  C==$:;  C==$~;  C==$+;
                     C==$-;  C==$*;  C==$/;  C==$.;  C==$\\; C==$);  C==$<;  C==$>;
                     C==$=;  C==$|;  C==$^).

tokens(<<>>,                  _, {_,C}, Acc) -> om:rev(stack(C,Acc));
tokens(<<$\n,     R/binary>>, L, {_,C}, Acc) -> tokens(R,L+1,{1,[]},   stack(C,Acc));
tokens(<<$(,      R/binary>>, L, {t,C}, Acc) -> tokens(R,L,{t,[$(]},   stack(C,Acc));
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

stack(C,Ac) -> case om:rev(om:flat(C)) of [] -> Ac;
                                         "(" -> [open|Ac];
                                         ")" -> [close|Ac];
                                      [$#|A] -> inet(A,Ac);
                     [X|A] when ?is_alpha(X) -> vars([X|A],Ac);
                     [X|A] when ?is_termi(X) -> name([X|A],Ac);
                                          X  -> atom(X,Ac) end.

inet(X,Acc) -> [{var,{X,0}}|Acc].
atom(X,Acc) -> [list_to_atom(X)|Acc].
name(X,Acc) -> [{var,{X,0}}|Acc].
ivar([N,I]) -> [N,I];
ivar([N])   -> [N,"0"].
vars(X,Acc) -> [Name,Index]= ivar(om:tokens(X,"@")),
               [{var,{list_to_atom(Name),list_to_integer(Index)}}|Acc].
