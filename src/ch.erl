-module(ch).
-compile(export_all).

% Erlang has no partial apply, so we draw it

ap(Fun,Args) -> lists:foldl(fun(X,Acc) -> Acc(X) end,Fun,Args).

% MANUAL COMPILATION and formatting for visual algorithm description.

%   data bool: * :=
%        (true: bool)
%        (false: bool)

bool   () ->           [true, false].
true   () ->            fun (T) -> fun (F) -> T end end.
false  () ->            fun (T) -> fun (F) -> F end end.

              bool(N) -> ap(?MODULE:N(),[]).
              unbool(A) -> ap(A,ch:bool()).

%   data return: * :=
%        (ok: * → return)
%        (error: * → return)

return () ->           [fun (Ok) -> ok end, fun (Error) -> error end].
id     () -> fun (R) -> fun (Ok) -> fun (Error) -> R end end end.
ok     () -> fun (V) -> fun (Ok) -> fun (Error) -> ap(Ok,   [ap(V,[Ok,Error])]) end end end.
error  () -> fun (V) -> fun (Ok) -> fun (Error) -> ap(Error,[ap(V,[Ok,Error])]) end end end.

             ret({N,A}) -> ap(?MODULE:N(),[ap(id(),[A])]).
             unret(A) -> ap(A,[fun (X) -> {ok,X} end, fun (X) -> {error,X} end]).

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
list   () ->                            [fun (Cons) -> fun (Nil) -> cons end end, nil].
nil    () ->                             fun (Cons) -> fun (Nil) -> Nil end end.
cons   () -> fun (Head) -> fun (Tail) -> fun (Cons) -> fun (Nil) -> ap(Cons,[Head,ap(Tail,[Cons,Nil])]) end end end end.

             % mapping to erlang list

             list   ([])          -> nil();
             list   ([Head|Tail]) -> fun (Cons) -> fun (Nil) -> ap(Cons,[Head,ap(list(Tail),[Cons,Nil])]) end end.
             kons   ()            -> fun (A) -> fun (L) -> [A|L] end end.
             unlist (L)           -> ap(L,[kons(),[]]).

% benchmarks in 1M

main  ()  -> io:format("Zero: ~p~n",               [unnat(zero())]),
             io:format("Nil: ~p~n",                [unlist(ap(cons(),[2,ap(cons(),[1,nil()])]))]),
             io:format("Pack/Unpack 1 000 000 Inductive Nat: ~p~n",   [timer:tc(fun () ->unnat(nat(1000000)) end)]),
             io:format("Pack/Unpack 1 000 000 Inductive List: ~p~n",   [{element(1,timer:tc(fun () ->unlist(list(lists:seq(1,1000000))) end)),'_'}]),
             io:format("Test Big List: ~p~n",      [unlist(list([2,3,5,8,11,19]))]),
             io:format("Two: ~p~n",                [unnat(ap(succ(),[ap(succ(),[zero()])]))]).
