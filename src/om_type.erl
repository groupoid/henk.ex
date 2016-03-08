-module(om_type).
-description('Type Checker').
-compile(export_all).

hierarchy(Arg,Out) -> Out.           % impredicative
%hierarchy(Arg,Out) -> max(Arg,Out). % predicative

type(Term) -> type(Term, []). % closed term (w/o free vars)
type({star,N},D)              -> {star,N+1};
type({var,{N,I}},D)           -> assertVar(N,D), proplists:get_value(N,D); % TODO respect index of var
type({"→",{I,O}},D)           -> {star,hierarchy(star(type(I,D)),star(type(O,D)))};
type({{"∀",{N,0}},{I,O}},D)   -> {star,hierarchy(star(type(I,D)),star(type(O,[{N,normalize(I)}|D])))};
type({{"λ",{N,0}},{I,O}},D)   -> star(type(I,D)), NI = normalize(I), {{"∀",{N,0}},{NI,type(O,[{N,NI}|D])}};
type({app,{F,A}},D)           -> T = type(F,D),
                                 assertFunc(T),
                                 {{"∀",{N,0}},{I,O}} = T,
                                 eq(I,type(A,D)),
                                 normalize(subst(O,N,A)).

normalize(none)                          -> none;
normalize(any)                           -> {star,1};
normalize({"→",        {I,O}})           -> {{"∀",{'_',0}},{normalize(I),normalize(O)}};
normalize({{"∀",{N,0}},{I,O}})           -> {{"∀",{N,0}},  {normalize(I),normalize(O)}};
normalize({{"λ",{N,0}},{I,O}})           -> {{"λ",{N,0}},  {normalize(I),normalize(O)}};
normalize({app,{{{"λ",{N,0}},{I,O}},A}}) -> normalize(subst(O,N,A));
normalize({app,{F,A}})                   -> {app,          {normalize(F),normalize(A)}};
normalize({var,{N,I}})                   -> {var,{N,I}};
normalize({star,N})                      -> {star,N}.

subst(Term,Name,Value)           -> subst(Term,Name,Value,0).
subst({"→",        {I,O}},N,V,L) -> {"→",        {subst(I,N,V,L),subst(O,N,V,L)}};
subst({{"∀",{F,0}},{I,O}},N,V,L) -> {{"∀",{F,0}},{subst(I,N,V,L),subst(O,N,V,L+1)}};
subst({{"∀",{F,X}},{I,O}},N,V,L) -> {{"∀",{F,X}},{subst(I,N,V,L),subst(O,N,V,L)}};
subst({{"λ",{F,0}},{I,O}},N,V,L) -> {{"λ",{F,0}},{subst(I,N,V,L),subst(O,N,V,L+1)}};
subst({{"λ",{F,X}},{I,O}},N,V,L) -> {{"λ",{F,X}},{subst(I,N,V,L),subst(O,N,V,L)}};
subst({app, {F,A}},       N,V,L) -> {app,        {subst(F,N,V,L),subst(A,N,V,L)}};
subst({var,{N,I}},_,_,_)         -> {var,{N,I}}; % no match
subst({var,_},_,Value,_)         -> Value;       % index match
subst({star,N},_,_,_)            -> {star,N}.

eq(T,T)                                           -> true;
eq({{"∀",{"_",0}},X},{"→",Y})                     -> eq(X,Y);
eq({{"∀",{N1,0}},{I1,O1}},{{"∀",{N2,0}},{I2,O2}}) -> eq(I1,I2), eq(O1,subst(O2,N2,{var,{N1,0}},0));
eq({{"λ",{N1,0}},{I1,O1}},{{"λ",{N2,0}},{I2,O2}}) -> eq(I1,I2), eq(O1,subst(O2,N2,{var,{N1,0}},0));
eq({app,{F1,A1}},{app,{F2,A2}})                   -> eq(F1,F2), eq(A1,A2);
eq(A,B)                                           -> erlang:error(["==", A, B]).

star({star,N})        -> N;
star(_)               -> erlang:error("*").

univ({star,N})        -> true;
univ({{"∀",N},{I,O}}) -> univ(O);
univ(_)               -> false.

assertFunc({{"∀",N},{I,O}}) -> true;
assertFunc(T)               -> erlang:error(["∀",T]).

assertVar(Name,Bind)        -> assertVar(Name,Bind,proplists:is_defined(Name,Bind)).
assertVar(Name,Bind,true)   -> true;
assertVar(Name,Bind,false)  -> erlang:error(["free var", Name, Bind]).
