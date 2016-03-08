-module(om_erase).
-compile(export_all).

-define(is_fun(F), F == "∀"; F== "λ").

erase(Term)          -> erase(Term,[]).
erase({star,N},_)    -> {none,{star,N+1}};

erase({"→",{I,O}},D) ->
    {B1,S1} = erase(I,D),
          L = star(S1),
     case univ(normalize(B1)) of
         true  -> erase(O,D);
         false -> {B2,S2} = erase(O,D),
                        R = star(S2),
                  {{"→",{B1,B2}},{star,hierarchy(L,R)}} end;

erase({{F,{N,0}},{I,O}},D) when ?is_fun(F) ->
    NI = normalize(I),
    {B1,S1} = erase(O,[{N,NI}|D]),
    case univ(NI) of
        true  -> {B1,S1};
        false -> case F of
                     "∀" -> {{{F,{N,0}},{any,B1}},{star,hierarchy(star(om:type(I,D)),star(S1))}};
                     "λ" -> {{{F,{N,0}},{any,B1}},{{"∀",{N,0}},{any,S1}}} end end;

erase({app,{F,A}},D) ->
    {B1,S1} = erase(F,D),
    case univ(S1) of
         true  -> {none,S1};
         false -> om_type:assertFunc(S1),
                  {{"∀",{N,0}},{I,O}} = S1,
                  {B2,S2} = erase(A,D),
                  case univ(S2) of
                       true  -> {B2,S2};
                       false -> {{app,{B1,B2}},normalize(om_type:substVar(O,N,B2))} end end;

erase({var,{N,I}},D) ->
    om_type:assertVar(N,D),
    T = proplists:get_value(N,D),
    case univ(T) of
         true  -> {none,T};
         false -> {{var,{N,I}},T} end.

% used

hierarchy(L,R) -> om_type:hierarchy(L,R).
normalize(X)   -> om_type:normalize(X).
star(X)        -> om_type:getStar(X).
univ(X)        -> om_type:isUniv(X).

