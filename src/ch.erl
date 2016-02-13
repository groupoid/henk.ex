-module(ch).
%-compile([{parse_transform, pi}]).
-compile(export_all).

ap(Fun,Args) -> lists:foldl(fun(X,Acc) -> Acc(X) end,Fun,Args).

id   () -> fun (X) -> X end.
inc  () -> fun (X) -> X + 1 end.
kons () -> fun (A) -> fun (L) -> [A|L] end end.

nat_ () -> [{X,?MODULE:X()}||X<-[nat,succ,zero]].
nat  () -> fun (Nat) -> fun (Succ) -> fun (Zero) -> Nat end end end.
zero () ->              fun (Succ) -> fun (Zero) -> Zero end end.
succ () -> fun (Nat) -> fun (Succ) -> fun (Zero) -> ap(Succ,[ap(Nat,[Succ,Zero])]) end end end.

list_() -> [{X,?MODULE:X()}||X<-[list,cons,nil]].
list () ->            fun (List) -> fun (Cons) -> fun (Nil) -> List end end end.
nil  () ->                          fun (Cons) -> fun (Nil) -> Nil end end.
cons () -> fun (A) -> fun (List) -> fun (Cons) -> fun (Nil) -> ap(Cons,[A,ap(List,[Cons,Nil])]) end end end end.

nat   (0) -> zero();
nat   (I) -> fun (Succ) -> fun(Zero) -> ap(Succ,[ap(nat(I-1),[Succ,Zero])]) end end.
unnat (N) -> ap(N,[inc(),0]).

list  ([])       -> nil();
list  ([A|List]) -> fun (Cons) -> fun (Nil) -> ap(Cons,[A,ap(list(List),[Cons,Nil])]) end end.
unlist (L)       -> ap(L,[kons(),[]]).

main  ()  -> io:format("Zero: ~p~n",               [unnat(zero())]),
             io:format("Nil: ~p~n",                [unlist(ap(cons(),[2,ap(cons(),[1,nil()])]))]),
             L = lists:seq(1,1000000),
             io:format("Pack/Unpack 100 000 Inductive Nat: ~p~n",   [timer:tc(fun () ->unnat(nat(100000)) end)]),
             io:format("Pack/Unpack 100 000 Inductive List: ~p~n",   [{element(1,timer:tc(fun () ->unlist(list(L)) end)),'_'}]),
             io:format("Test Big List: ~p~n",      [unlist(list([2,3,5,8,11,19]))]),
             io:format("Two: ~p~n",                [unnat(ap(succ(),[ap(succ(),[zero()])]))]).
