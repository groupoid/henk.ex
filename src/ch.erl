-module(ch).
%-compile([{parse_transform, pi}]).
-compile(export_all).

ap(Fun,Args) -> lists:foldl(fun(X,Acc) -> Acc(X) end,Fun,Args).

id   () -> fun (X) -> X end.
inc  () -> fun (X) -> X + 1 end.

nat  () -> [X()||X<-[zero,succ]].
zero () -> fun (F) -> fun (X) -> X end end.
succ () -> fun (N) -> fun (F) -> fun(X) -> ap(F,[ap(N,[F,X])]) end end end.

church (0) -> zero();
church (I) -> fun (F) -> fun(X) -> ap(F,[ap(church(I-1),[F,X])]) end end.

unchurch (N) -> ap(N,[inc(),0]).

main()        -> io:format("Zero: ~p~n", [unchurch(zero())]),
                 io:format("20: ~p~n",   [unchurch(church(100000))]),
                 io:format("Two: ~p~n",  [unchurch(ap(succ(),[ap(succ(),[zero()])]))]).
