-module(ch).
-compile({parse_transform, ch}).
-export([parse_transform/2]).
-compile(export_all).

parse_transform(Forms, _Options) ->
    io:format("Forms: ~p~n",[Forms]),
    compile:forms(Forms,[binary,export_all]),
    Forms.

ap(Fun,Args) -> lists:foldl(fun(X,Acc) -> Acc(X) end,Fun,Args).

id   () -> fun (X) -> X end.
inc  () -> fun (X) -> X + 1 end.
zero ()       -> fun (F) -> fun(X) -> X end end.
successor (N) -> fun (F) -> fun(X) -> ap(F,[ap(N,[F,X])]) end end.
unchurch  (N) ->                      ap(N,[inc(),0]).
church (0)    -> zero();
church (I)    -> fun (F) -> fun(X) -> ap(F,[ap(church(I-1),[F,X])]) end end.

main()        -> io:format("Zero: ~p~n", [unchurch(zero())]),
                 io:format("20: ~p~n",   [unchurch(church(20))]),
                 io:format("Two: ~p~n",  [unchurch(successor(successor(zero())))]).
