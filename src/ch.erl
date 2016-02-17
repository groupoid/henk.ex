-module(ch).
-compile(export_all).

ap(Fun,Args) -> lists:foldl(fun(X,Acc) -> Acc(X) end,Fun,Args).

%  data nat: * :=
%       (zero: () → nat)
%       (succ: nat → nat)

nat_   () ->                            [{X,?MODULE:X()} || X <- [nat,succ,zero]].
nat    () ->                        [fun (Succ) -> fun (Zero) -> succ end end, zero].
zero   () ->                         fun (Succ) -> fun (Zero) -> Zero end end.
succ   () ->            fun (Nat) -> fun (Succ) -> fun (Zero) -> ap(Succ,[ap(Nat,[Succ,Zero])]) end end end.

             % mapping to erlang integer

             nat   (0) -> zero();
             nat   (I) -> fun (Succ) -> fun(Zero) -> ap(Succ,[ap(nat(I-1),[Succ,Zero])]) end end.
             inc    () -> fun (X) -> X + 1 end.
             unnat (N) -> ap(N,[inc(),0]).
             plus(A,B) -> nat(elrnag:'+'(unnat(A),unnat(B))).

%  data list: (A:*) → * :=
%       (nil: () → list A)
%       (cons: A → list A → list A)
         
list_  () ->                            [{X,?MODULE:X()} || X <- [list,cons,nil]].
list   () ->                         [fun (Cons) -> fun (Nil) -> cons end end, nil].
nil    () ->                          fun (Cons) -> fun (Nil) -> Nil end end.
cons   () -> fun (A) -> fun (List) -> fun (Cons) -> fun (Nil) -> ap(Cons,[A,ap(List,[Cons,Nil])]) end end end end.

             % mapping to erlang list

             list   ([])          -> nil();
             list   ([A|List])    -> fun (Cons) -> fun (Nil) -> ap(Cons,[A,ap(list(List),[Cons,Nil])]) end end.
             kons   ()            -> fun (A) -> fun (L) -> [A|L] end end.
             unlist (L)           -> ap(L,[kons(),[]]).

% benchmarks in 1M

main  ()  -> io:format("Zero: ~p~n",               [unnat(zero())]),
             io:format("Nil: ~p~n",                [unlist(ap(cons(),[2,ap(cons(),[1,nil()])]))]),
             L = lists:seq(1,1000000),
             io:format("Pack/Unpack 100 000 Inductive Nat: ~p~n",   [timer:tc(fun () ->unnat(nat(100000)) end)]),
             io:format("Pack/Unpack 100 000 Inductive List: ~p~n",   [{element(1,timer:tc(fun () ->unlist(list(L)) end)),'_'}]),
             io:format("Test Big List: ~p~n",      [unlist(list([2,3,5,8,11,19]))]),
             io:format("Two: ~p~n",                [unnat(ap(succ(),[ap(succ(),[zero()])]))]).
