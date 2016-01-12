-module(ch).
%-compile([{parse_transform, pi}]).
-compile(export_all).

ap(Fun,Args) -> lists:foldl(fun(X,Acc) -> Acc(X) end,Fun,Args).

id   () -> fun (X) -> X end.
inc  () -> fun (X) -> X + 1 end.

nat  () -> [{X,?MODULE:X()}||X<-[succ,zero]].
zero () ->              fun (Succ) -> fun (Zero) -> Zero end end.
succ () -> fun (Nat) -> fun (Succ) -> fun (Zero) -> ap(Succ,[ap(Nat,[Succ,Zero])]) end end end.

list () -> [{X,?MODULE:X()}||X<-[cons,nil]].
nil  () ->                          fun (Cons) -> fun (Nil) -> Nil end end.
cons () -> fun (A) -> fun (List) -> fun (Cons) -> fun (Nil) -> [A|[]] end end end end.

church (0) -> zero();
church (I) -> fun (Succ) -> fun(Zero) -> ap(F,[ap(church(I-1),[Succ,Zero])]) end end.

unchurch (N) -> ap(N,[inc(),0]).

main()        -> io:format("Zero: ~p~n", [unchurch(zero())]),
                 io:format("Test Big Numeral: ~p~n",   [unchurch(church(100000))]),
                 io:format("Two: ~p~n",  [unchurch(ap(succ(),[ap(succ(),[zero()])]))]).
