-module(om_erase).
-compile(export_all).

-define(is_fun(F), F == "∀"; F== "λ").

erase2(T,D) ->
    om:debug("erase?: T = ~tp~n // D = ~tp~n -------------------------~n", [om:bin(T), lists:map(fun(P) -> {V,E}=P, {V,om:bin(E)} end, D)]),
    {E,R} = erase(T,D),
    om:debug("erase!: ~tp :~n ~tp ~n",[om:bin(T), om:bin(E)]),
    {E,R}.

erase({box,N},_)     -> {none,{star,3}};
erase({star,N},_)    -> {none,{star,N+1}};
erase({var,{N,I}},D) -> om_type:assertVar(N,D), T = om:keyget(N,D,I),
    case univ(T) of
         true  -> {none,T};
         false -> {{var,{N,I}},T} end;
erase({"→",{I,O}},D) -> {none,{star,om:hierarchy(star(om:type(I,D)),star(om:type(O,D)))}};
erase({{"∀",{N,0}},{I,O}},D)   -> {none,{star,om:hierarchy(star(om:type(I,D)),star(om:type(O,[{N,om:normalize(I)}|D])))}};
erase({{"λ",{N,0}},{I,O}},D) -> star(om:type(I,D)), NI = om:normalize(I), {B1,S1} = om:erase(O,[{N,NI}|D]), T = {{"∀",{N,0}},{NI,S1}},
    case univ(NI) of
        true  -> {B1,T};
        false -> {{{"λ",{N,0}},{any,B1}},T} end;
erase({app,{F,A}},D) -> {B1,S1} = om:erase(F,D), {B2,S2} = om:erase(A,D), om_type:assertFunc(S1), {{"∀",{N,0}},{I,O}} = S1, om_type:eq(I,S2),
    T=om:normalize(om_type:subst(O,N,A)), case univ(S1) of
         true  -> {none,T};
         false -> case univ(S2) of
                       true  -> {B1,T};
                       false -> {{app,{B1,B2}},T} end end;
erase({remote,N},D)        -> om_cache:load(erased,N).


univ({star,N})        -> true;
univ({{"∀",N},{I,O}}) -> univ(O);
univ(_)               -> false.

star(X)        -> om_type:star(X).
