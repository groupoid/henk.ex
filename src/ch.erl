-module(ch).
-compile(export_all).

ap(Fun,Args) -> lists:foldl(fun(X,Acc) -> Acc(X) end,Fun,Args).

id()         -> fun (X) -> X end.
inc()        -> fun (X) -> X + 1 end.
zero()       -> fun(_) -> fun(X) -> X end end.
successor(N) -> fun(F) -> fun(X) -> ap(F,[ap(N,[F,X])]) end end.
unchurch(N)  -> ap(N,[inc(),0]).

main()       -> io:format("Zero: ~p~n", [unchurch(zero())]),
                io:format("Two: ~p~n",  [unchurch(successor(successor(zero())))]).
