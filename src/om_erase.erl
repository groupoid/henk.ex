-module(om_erase).
-compile(export_all).

-define(is_fun(F), F == "∀"; F== "λ").

erase(Term)          -> erase(Term,[]).
erase({star,N},_)    -> {none,{star,N+1}};

erase({"→",{I,O}},D) ->
    {B1,S1} = erase(I,D),
     case univ(normalize(B1)) of
         true  -> erase(O,D);
         false -> {B2,S2} = erase(O,D),
                  {{"→",{B1,B2}},{star,hierarchy(star(S1),star(S2))}} end;

erase({var,{N,I}},D) ->
    om_type:assertVar(N,D),
    T = proplists:get_value(N,D),
    case univ(T) of
         true  -> {none,T};
         false -> {{var,{N,I}},T} end;

erase({{F,{N,X}},{I,O}},D) when ?is_fun(F) ->
    NI = normalize(I),
    {B1,S1} = erase(O,[{N,NI}|D]),
    case univ(NI) of
        true  -> {B1,S1};
        false -> case F of
                     "∀" -> {{{F,{N,X}},{any,B1}},{star,hierarchy(star(om_type:getType(I,D)),star(S1))}};
                     "λ" -> {{{F,{N,X}},{any,B1}},{{"∀",{N,X}},{any,S1}}} end end;

erase({app,{F,A}},D) ->
    {B1,S1} = erase(F,D),
    case univ(S1) of
         true  -> {none,S1};
         false -> {{"∀",{N,_}},{I,O}} = S1,
                  {B2,S2} = erase(A,D),
                  case univ(S2) of
                       true  -> {B1,S1};
                       false -> {{app,{B1,B2}},normalize(om_type:substVar(O,N,B2))} end end.

% used

hierarchy(L,R) -> om_type:hierarchy(L,R).
normalize(X)   -> om_type:normalize(X).
star(X)        -> om_type:getStar(X).
univ(X)        -> om_type:isUniv(X).

