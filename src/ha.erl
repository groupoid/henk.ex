-module(ha).
-compile(export_all).

id(X) -> X.
ap(Fun,Args) -> lists:foldl(fun(X,Acc) -> Acc(X) end,Fun,Args).

bool(ctor)  -> [ bool(X) || X <- [false,true]];
bool(false) -> fun(X) -> fun(Y) -> Y end end;
bool(true)  -> fun(X) -> fun(Y) -> X end end;
bool(fold)  -> fun(X) -> fun(Y) -> fun(true)  -> X;
                                      (false) -> Y end end end.

nat(ctor)   -> [ nat(X) || X <- [succ,zero]];
nat(zero)   ->           fun(Z) -> fun(S) -> Z end end;
nat(succ)   -> fun(N) -> fun(Z) -> fun(S) -> S(N(Z,S)) end end end;
nat(fold)   -> fun(X) -> fun(Y) -> fun(zero)  -> X;
                                      (succ)  -> fun(M) -> Y(ap(nat(fold),[X,Y(M)])) end end end end.

main() ->
   io:format("Bool: ~p~n",[ap(bool(fold),bool(ctor) ++ [true,1,2])]),
   io:format("Nat: ~p~n", [ap(nat(fold), nat(ctor) ++ [succ,nat(zero),nat(succ)])]).
