-module(om_check).
-compile(export_all).

error({input,X})        -> om:print("Invalid input type: ~p~n",[X]);
error({output,X})       -> om:print("Invalid output type: ~p~n",[X]);
error({untyped,X})      -> om:print("No type for: ~p~n",[X]);
error({mismatch,{X,Y}}) -> om:print("Invalid Application~nExpected type: ~p~nArgument type: ~p~n",[X,Y]);
error({unbound,U})      -> om:print("Unbound variable ~p.~n",[U]);
error({app,A})          -> om:print("Only functions may be applied to values ~p.~n",[A]);
error(Error)            -> om:print("Unknown type error: ~p~n",[Error]).

freeIn({var,{X,N}}=_V,{{Fun,Y},{In,Out}}) when Fun=="λ";Fun=="∀";X==Y -> freeIn(_V,In) orelse freeIn({var,{X,N+1}},Out);
freeIn({var,{X,N}}=_V,{{Fun,Y},{In,Out}}) when Fun=="λ";Fun=="∀"      -> freeIn(_V,In) orelse freeIn(_V,Out);
freeIn({var,{_,_}}=_V,{app,{In,Out}})                                 -> freeIn(_V,In) orelse freeIn(_V,Out);
freeIn({var,{X,N}}=_V,{var,{Y,O}})   -> X == Y;
freeIn(_,_)                          -> false.

shift(D,Text,X) -> shift(D,Text,X,0).
shift(D,Text,{{Fun,X},{A,B}},C) when Fun=="λ";Fun=="∀" -> {{Fun,X},{shift(D,Text,A,C),shift(D,Text,B,case X of Text->C+1; _->C end)}};
shift(D,Text,{app,{F,A}},C) -> {app,{shift(D,Text,F,C),shift(D,Text,A,C)}};
shift(D,Text,{var,{X,N}},C) -> {var,{X,case X of Text when N >= C -> N + D; _ -> N end}};
shift(D,Text,X,C) -> X.

subst(X,N,EP,{{Fun,XP},{A,B}}) when Fun=="λ";Fun=="∀" -> {{Fun,X},{subst(X,N,EP,A),subst(X,case X of XP -> N+1;_->N end,shift(1,XP,EP),B)}};
subst(X,N,EP,{app,{F,A}}) -> {app,{subst(X,N,EP,F),subst(X,N,EP,A)}};
subst(X,N,EP,{var,{XP,NP}}=E) -> case X of XP when N==NP -> EP; _-> E end;
subst(X,N,EP,Else) -> Else.

test() -> lists:all(fun assert/1,lists:seq(1,10)).

assert(1) -> freeIn({var,{head,0}},om:type("List/Cons")) == true;
assert(_) -> true.

