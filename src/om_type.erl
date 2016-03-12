-module(om_type).
-description('Type Checker').
-compile(export_all).

type2(T,D) ->
    om:debug("type?: T = ~tp~n // D = ~tp~n -------------------------~n", [om:bin(T), lists:map(fun(P) -> {V,E}=P, {V,om:bin(E)} end, D)]),
    R=type(T,D),
    om:debug("type!: ~tp :~n ~tp ~n....................~n",[om:bin(T), om:bin(R)]),
    R.

type({box,N},_)               -> {star,3};
type({star,N},_)              -> {star,N+1};
type({var,{N,I}},D)           -> assertVar(N,D), om:keyget(N,D,I);
type({"→",{I,O}},D)           -> {star,om:hierarchy(star(om:type(I,D)),star(om:type(O,D)))};
type({{"∀",{N,0}},{I,O}},D)   -> {star,om:hierarchy(star(om:type(I,D)),star(om:type(O,[{N,om:normalize(I)}|D])))};
type({{"λ",{N,0}},{I,O}},D)   -> star(om:type(I,D)), NI = om:normalize(I), {{"∀",{N,0}},{NI,om:type(O,[{N,NI}|D])}};
type({app,{F,A}},D)           -> T = om:type(F,D),
                                 assertFunc(T),
                                 {{"∀",{N,0}},{I,O}} = T,
                                 om:eq(I,om:type(A,D)),
                                 om:normalize(subst(O,N,A)).

normalize2(T) -> NT=normalize(T),
    om:debug("normalize (~tp)=>(~tp)~n...~n",[om:bin(T), om:bin(NT)]),
    NT.

normalize(none)                          -> none;
normalize(any)                           -> any;
normalize({"→",        {I,O}})           -> {{"∀",{'_',0}},{normalize(I),normalize(O)}};
normalize({{"∀",{N,0}},{I,O}})           -> {{"∀",{N,0}},  {normalize(I),normalize(O)}};
normalize({{"λ",{N,0}},{I,O}})           -> {{"λ",{N,0}},  {normalize(I),normalize(O)}};
normalize({app,{F,A}})                   -> NF=normalize(F),case NF of
    {{"λ",{N,0}},{I,O}} -> normalize(subst(O,N,A));
    _ -> {app,{NF,normalize(A)}} end; % be lazy
normalize(T)                             -> T.

shift({var,{N,I}},N,P) when I>=P -> {var,{N,I+1}};
shift({{"∀",{N,0}},{I,O}},N,P)  -> {{"∀",{N,0}},{shift(I,N,P),shift(O,N,P+1)}};
shift({{"λ",{N,0}},{I,O}},N,P)  -> {{"λ",{N,0}},{shift(I,N,P),shift(O,N,P+1)}};
shift({Q,{L,R}},N,P)            -> {Q,{shift(L,N,P),shift(R,N,P)}};
shift(T,N,P)                    -> T.

subst(Term,Name,Value)           -> subst(Term,Name,Value,0).
subst({"→",        {I,O}},N,V,L) -> {"→",        {subst(I,N,V,L),subst(O,N,V,L)}};
subst({{"∀",{N,0}},{I,O}},N,V,L) -> {{"∀",{N,0}},{subst(I,N,V,L),subst(O,N,shift(V,N,0),L+1)}};
subst({{"∀",{F,X}},{I,O}},N,V,L) -> {{"∀",{F,X}},{subst(I,N,V,L),subst(O,N,shift(V,F,0),L)}};
subst({{"λ",{N,0}},{I,O}},N,V,L) -> {{"λ",{N,0}},{subst(I,N,V,L),subst(O,N,shift(V,N,0),L+1)}};
subst({{"λ",{F,X}},{I,O}},N,V,L) -> {{"λ",{F,X}},{subst(I,N,V,L),subst(O,N,shift(V,F,0),L)}};
subst({app, {F,A}},       N,V,L) -> {app,        {subst(F,N,V,L),subst(A,N,V,L)}};
subst({var, {N,L}},       N,V,L) -> V;           % index match
subst({var, {N,I}},       N,V,L) when I>L -> {var, {N,I-1}}; % unshift
subst(T,       _,_,_)            -> T.

eq2(X,Y) ->
    om:debug("eq?: X = ~tp~n // Y = ~tp~n.................~n",[om:bin(X),om:bin(Y)]),
    eq(X,Y).

eq(T,T)                                           -> true;
%eq({{"∀",{"_",0}},X},{"→",Y})                     -> eq(X,Y);
eq({{"∀",{N1,0}},{I1,O1}},{{"∀",{N2,0}},{I2,O2}}) -> eq(I1,I2), eq(O1,subst(shift(O2,N1,0),N2,{var,{N1,0}},0));
eq({{"λ",{N1,0}},{I1,O1}},{{"λ",{N2,0}},{I2,O2}}) -> eq(I1,I2), eq(O1,subst(shift(O2,N1,0),N2,{var,{N1,0}},0));
eq({app,{F1,A1}},{app,{F2,A2}})                   -> eq(F1,F2), eq(A1,A2);
eq(A,B)                                           -> erlang:error(["==", A, B]).

star({star,N})        -> N;
star(_)               -> erlang:error("*").

assertFunc({{"∀",N},{I,O}}) -> true;
assertFunc(T)               -> erlang:error(["∀",T]).

assertVar(Name,Bind)        -> assertVar(Name,Bind,proplists:is_defined(Name,Bind)).
assertVar(Name,Bind,true)   -> true;
assertVar(Name,Bind,false)  -> erlang:error(["free var", Name, Bind]).
