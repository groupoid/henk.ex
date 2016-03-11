-module(om_type).
-description('Type Checker').
-compile(export_all).

hierarchy(Arg,Out) -> Out.           % impredicative
%hierarchy(Arg,Out) -> max(Arg,Out). % predicative

type2(T) -> type(T,[]).
type2(T,D) ->
    om:debug("type?: T = ~tp~n // D = ~tp~n -------------------------~n", [om:bin(T), lists:map(fun(P) -> {V,E}=P, {V,om:bin(E)} end, D)]),
    R=type(T,D),
    om:debug("type!: ~tp :~n ~tp ~n",[om:bin(T), om:bin(R)]),
    R.

type(Term) -> type(Term, []). % closed term (w/o free vars)
type({box,N},D)               -> {box,N};
type({star,N},D)              -> {star,N+1};
type({var,{N,I}},D)           -> assertVar(N,D), lists:nth(I+1,proplists:get_all_values(N,D)); % TODO respect index of var
type({"→",{I,O}},D)           -> {star,hierarchy(star(type2(I,D)),star(type2(O,D)))};
type({{"∀",{N,0}},{I,O}},D)   -> {star,hierarchy(star(type2(I,D)),star(type2(O,[{N,normalize2(I)}|D])))};
type({{"λ",{N,0}},{I,O}},D)   -> star(type2(I,D)), NI = normalize2(I), {{"∀",{N,0}},{NI,type2(O,[{N,NI}|D])}};
type({app,{F,A}},D)           -> T = type2(F,D),
                                 assertFunc(T),
                                 {{"∀",{N,0}},{I,O}} = T,
                                 eq(I,type2(A,D)),
                                 normalize2(subst(O,N,A)).

normalize2(T) -> NT=normalize(T),
    om:debug("normalize (~tp)=>(~tp)~n...~n",[om:bin(T), om:bin(NT)]),
    NT.

normalize(none)                          -> none;
normalize(any)                           -> {star,1};
normalize({"→",        {I,O}})           -> {{"∀",{'_',0}},{normalize(I),normalize(O)}};
normalize({{"∀",{N,0}},{I,O}})           -> {{"∀",{N,0}},  {normalize(I),normalize(O)}};
normalize({{"λ",{N,0}},{I,O}})           -> {{"λ",{N,0}},  {normalize(I),normalize(O)}};
normalize({app,{F,A}})                   -> NF=normalize(F),case NF of
    {{"λ",{N,0}},{I,O}} -> normalize(subst(O,N,A));
    _ -> {app,{NF,normalize(A)}} end; % be lazy
normalize({var,{N,I}})                   -> {var,{N,I}};
normalize({star,N})                      -> {star,N}.

shift({var,{NN,I}},N)        -> {var,{NN, case N of NN -> I+1; _ -> I end}};
shift({Q, {L,R}},N)          -> {Q, {shift(L,N),shift(R,N)}};
shift(T,N)                   -> T.

subst(Term,Name,Value)           -> subst(Term,Name,Value,0).
subst({"→",        {I,O}},N,V,L) -> {"→",        {subst(I,N,V,L),subst(O,N,V,L)}};
subst({{"∀",{N,0}},{I,O}},N,V,L) -> {{"∀",{N,0}},{subst(I,N,V,L),subst(O,N,shift(V,N),L+1)}};
subst({{"∀",{F,X}},{I,O}},N,V,L) -> {{"∀",{F,X}},{subst(I,N,V,L),subst(O,N,shift(V,N),L)}};
subst({{"λ",{N,0}},{I,O}},N,V,L) -> {{"λ",{N,0}},{subst(I,N,V,L),subst(O,N,shift(V,N),L+1)}};
subst({{"λ",{F,X}},{I,O}},N,V,L) -> {{"λ",{F,X}},{subst(I,N,V,L),subst(O,N,shift(V,N),L)}};
subst({app, {F,A}},       N,V,L) -> {app,        {subst(F,N,V,L),subst(A,N,V,L)}};
subst({var, {N,L}},       N,V,L) -> V;           % index match
subst({var, {N,I}},       _,_,_) -> {var,{N,I}}; % no match
subst({star,N},           _,_,_) -> {star,N}.

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
