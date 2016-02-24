-module(ch).
-description('Natural Encoding Schema').
-compile({parse_transform, dump}).
-compile(export_all).

% Erlang Partial Application

ap(Fun,Args) -> lists:foldl(fun(X,Acc) -> Acc(X) end,Fun,Args).

ap1(Fun,Arg) -> Fun(Arg).

%   data bool: * :=
%        (true: bool)
%        (false: bool)

bool   () ->      [true, false].
true   () ->       fun (T) -> fun (F) -> T end end.
false  () ->       fun (T) -> fun (F) -> F end end.

% record prod: * :=
%        (pr1: *)
%        (pr2: *)

prod  () ->       [fun (A) -> fun (B)->  {prod,A,B} end end].
prodid () ->                  fun (X) -> fun (Mk) -> X end end.
mk     () ->       fun (A) -> fun (B) -> fun (Mk) -> ap(Mk,[ap(A,[Mk]),ap(B,[Mk])]) end end end.
pr1    () ->       [fun (A) -> fun (B) -> A end end].
pr2    () ->       [fun (A) -> fun (B) -> B end end].

            prod({A,B}) -> ap(mk(),[ap(prodid(),[A]),ap(prodid(),[B])]).
            unprod(X) -> ap(X,prod()).

% > ch:ap(ch:prod({io,2}),ch:pr1()).
% io
% 2> ch:ap(ch:prod({io,2}),ch:prod()).
% {prod,io,2}

%   data proto: * :=
%        (ok: * → proto)
%        (error: * → proto)
%        (io: * → * → proto)

ret    () ->           [fun (X) -> {ok,X} end, fun (X) -> {error,X} end, fun(X) -> fun (Y) -> {io,X,Y} end end].
retid  () ->            fun (R) -> fun (Ok) -> fun (Error) -> fun (Io) -> R end end end end.
ok     () ->            fun (V) -> fun (Ok) -> fun (Error) -> fun (Io) -> ap(Ok,   [ap(V,[Ok,Error,Io])]) end end end end.
error  () ->            fun (V) -> fun (Ok) -> fun (Error) -> fun (Io) -> ap(Error,[ap(V,[Ok,Error,Io])]) end end end end.
io     () -> fun (X) -> fun (Y) -> fun (Ok) -> fun (Error) -> fun (Io) -> ap(Io,   [ap(X,[Ok,Error,Io]),ap(Y,[Ok,Error,Io])]) end end end end end.

             ret({N,A}) -> ap(?MODULE:N(),[ap(retid(),[A])]);
             ret({io,A,B}) -> ap(io(),[ap(retid(),[A]),ap(retid(),[B])]).
             unret(A) -> ap(A,ret()).

%  data nat: * :=
%       (zero: () → nat)
%       (succ: nat → nat)

nat    () ->                        [fun (Succ) -> {succ,Succ} end, zero].
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

list   () ->                            [fun (H) -> fun (T) -> {cons,H,T} end end, nil].
nil    () ->                             fun (Cons) -> fun (Nil) -> Nil end end.
cons   () -> fun (Head) -> fun (Tail) -> fun (Cons) -> fun (Nil) -> ((Cons(Head))((Tail(Cons))(Nil))) end end end end.

             % mapping to erlang list

             list   ([])          -> nil();
             list   ([Head|Tail]) -> fun (Cons) -> fun (Nil) -> ap(Cons,[Head,ap(list(Tail),[Cons,Nil])]) end end.
             kons   ()            -> fun (A) -> fun (L) -> [A|L] end end.
             unlist (L)           -> ap(L,[kons(),[]]).

% marshaling sample

% > ch:ap(ch:ap(ch:succ(),[ch:ap(ch:succ(),[ch:zero()])]),ch:nat()).
% {succ,{succ,zero}}

% > ch:ap(ch:ap(ch:cons(),[2,ch:ap(ch:cons(),[1,ch:nil()])]),ch:list()).
% {cons,2,{cons,1,nil}}

% benchmarks in 1M

main  ()  -> io:format("Zero: ~p~n",               [unnat(zero())]),
             io:format("Cons/Nil: ~p~n",                [unlist(ap(cons(),[2,ap(cons(),[1,nil()])]))]),
             io:format("Pack/Unpack 1 000 000 Inductive Nat: ~p~n",   [timer:tc(fun () ->unnat(nat(1000000)) end)]),
             io:format("Pack/Unpack 1 000 000 Inductive List: ~p~n",   [{element(1,timer:tc(fun () ->unlist(list(lists:seq(1,1000000))) end)),'_'}]),
             io:format("Test Big List: ~p~n",      [unlist(list([2,3,5,8,11,19]))]),
             io:format("Two: ~p~n",                [unnat(ap(succ(),[ap(succ(),[zero()])]))]).
