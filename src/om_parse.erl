-module(om_parse).
-description('Parser').
-compile(export_all).

%     I := #identifier
%     O := ∅ | ( O ) |
%          □ | ∀ ( I : O ) → O |
%          * | λ ( I : O ) → O |
%          I | O → O | O O

expr(P,[],                       Acc)  ->      rewind(Acc,[],[]);
expr(P,[close               |T], Acc)  -> case rewind(Acc, T,[]) of
                                               {error,R} -> {error,R};
                                               {T1,Acc1} -> expr(P,T1,Acc1) end;

expr(P,[{remote,{X,L}}      |T], Acc)  -> expr(P,T,[ret(om:term([],L))|Acc]);
expr(P,[{remote,L}          |T], Acc)  -> expr(P,T,[om:term(L)|Acc]);
expr(P,[{star,I}            |T], Acc)  -> expr(P,T,[{star,I}|Acc]);
expr(P,[{N,X}|T],  [{typevar,Y}| Acc]) -> expr(P,T,[{N,X},{typevar,Y}|Acc]);
expr(P,[{N,X}|T],        [{C,Y}| Acc]) -> expr(P,T,[{app,{{C,Y},{N,X}}}|Acc]);
expr(P,[star                |T], Acc)  -> expr(P,T,[{star,1}|Acc]);
expr(P,[box                 |T], Acc)  -> expr(P,T,[{box,1}|Acc]);
expr(P,[open                |T], Acc)  -> expr(P,T,[{open}|Acc]);
expr(P,[arrow               |T], Acc)  -> expr(P,T,[{arrow}|Acc]);
expr(P,[lambda              |T], Acc)  -> expr(P,T,[{lambda}|Acc]);
expr(P,[pi                  |T], Acc)  -> expr(P,T,[{pi}|Acc]);
expr(P,[colon               |T], Acc)  -> expr(P,T,[{colon}|Acc]);
expr(P,[{var,L},colon       |T], Acc)  -> expr(P,T,[{typevar,L}|Acc]);
expr(P,[{var,L}             |T], Acc)  -> expr(P,T,[{var,L}|Acc]).

% During forward pass we stack applications (except typevars), then
% on reaching close paren ")" we perform backward pass and stack arrows,
% until neaarest unstacked open paren "(" appeared (then we just return
% control to the forward pass).

% We need to preserve applies to typevars as they should
% be processes lately on rewind pass, so we have just typevars bypassing rule.
% On the rewind pass we stack lambdas by matching arrow/apply signatures
% where typevar(x) is an introduction of variable "x" to the Gamma context.
%
%                   apply: (A->B) x A -> B
%                  lambda: arrow(app(typevar(x),A),B)
%

-define(is_fun(F), F == lambda; F== pi).

rewind([{F}|Acc],            T,  [{"→",{{app,{{typevar,{L,M}},{A,X}}},{B,Y}}}|R]) when ?is_fun(F) -> rewind2(Acc,T,[{{func(F),{L,M}},{{A,X},{B,Y}}}|R]);
rewind([{F}|Acc],            T,  [{"→",{{app,{{app,{{typevar,M},A}},X}},Y}}|R])   when ?is_fun(F) -> {error,{"parser1",{M,A,X,Y}}};
rewind([{F}|Acc],            T,  [{"→",{{L,{{_,{{typevar,M},A}},X}},{B,Y}}}|R])   when ?is_fun(F) -> rewind2(Acc,T,[{{func(F),M},{{L,{A,X}},{B,Y}}}|R]);
rewind([{open},{F}|Acc],     T,  [{_,{{app,{{typevar,{L,M}},{A,X}}},{B,Y}}}|R])   when ?is_fun(F) -> {T,om:flat([{{func(F),{L,M}},{{A,X},{B,Y}}}|[R|Acc]])};
rewind([{open},{F}|Acc],     T,  [{_,{{L,{{app,{{typevar,M},A}},X}},{B,Y}}}|R])   when ?is_fun(F) -> {T,om:flat([{{func(F),M},{{L,{A,X}},{B,Y}}}|[R|Acc]])};
rewind([{open},{typevar,Z}|Acc], T,[{I,{{app,X},Y}}|R]) -> {error,{"parser2",{Z,I,X,Y}}};
rewind([{open},{A,X}|Acc],   T,  [{B,Y}|R])             -> {T,om:flat([{app,{{A,X},{B,Y}}}|[R|Acc]])};
rewind([{open}|Acc],         T,  R)                     -> {T,om:flat([R|Acc])};
rewind([{A,X}|Acc],          T,  [{B,Y}|R])             -> rewind2(Acc,T,[{app,{{A,X},{B,Y}}}|R]);
rewind([{A,X}|Acc],          T,  R)                     -> rewind2(Acc,T,[{A,X}|R]);
rewind([{arrow},{{F,N},{I,O}}|Acc],T,[X|R])             -> rewind2(Acc,T,[{{func(F),N},{{func(arrow),{I,O}},X}}|R]);
rewind([{arrow},{"→",{{app,X},I}}|Acc],T,[Y|R])         -> {error,{"parser3",{X,I,Y}}};
rewind([{arrow},Y|Acc],      T,  [X|R])                 -> rewind2(Acc,T,[{func(arrow),{Y,X}}|R]);
rewind([{colon}|Acc],        T,  R)                     -> {T,om:flat([R|Acc])};
rewind([],                   T,  [{"→",{{app,X},Y}}|R]) -> {error,{"parser4",{X,Y}}};
rewind([],                   T,  R)                     -> {T,R};
rewind(X,                    T,  [Y|R])                 -> {error,{"parser5",{X,Y}}}.
rewind2(X,T,Y) ->
%   io:format("rewind: ~tp -- ~tp~n",[lists:sublist(X,2),lists:sublist(Y,1)]),
    rewind(X,T,Y).

%rewind([{arrow},{{F,N},{I,O}}|Acc],T,[X|R]) -> rewind2(Acc,T,[{func(arrow),{{{func(F),N},{I,O}},X}}|R]);
%rewind([{arrow},{{F,N},{I,O}}|Acc],T,[X|R]) when ?is_fun(F) -> rewind2(Acc,T,[{{func(F),N},{{func(arrow),{I,O}},X}}|R]);
%rewind([{arrow},{{F,N},{I,O}}|Acc],T,[X|R]) -> rewind2(Acc,T,[{func(arrow),{{{func(F),N},{I,O}},X}}|R]);

test() -> F = [ "(x : ( \\ (o:*) -> o ) -> p ) -> o",
                "\\ (x : ( err (o:*) -> o ) -> p ) -> o",
                "\\ (x : ( (o:*) -> o ) -> p ) -> o",
                "\\ (x : ( \\ (o:*) -> o ) -> p ) err -> o",
                "\\ (x : \\ (x: x -> l) -> o ) l -> z",
                "\\ (x : ( (o:*) -> o ) -> p ) -> o",
                "\\ (x : ( err (o:*) -> o ) -> p ) -> o"
              ],

          T = [ "\\ (x : (\\ (o:*) -> o) l -> p ) -> o",
                "\\ (x : ( \\ (o: \\ (x : (\\ (o:*) -> o) l -> p ) -> o) -> o ) -> p ) -> o",
                "a \\ (x : (a \\ (o:*) -> o) l -> p ) -> o",
                 "\\(x : (\\ (o:*) -> o) -> p ) -> o"
               ],

          lists:foldl(fun(X,Acc) ->  {X,{M,_}=A} = {X,om:a(X)},
                                     case M of error -> erlang:error(["test",X,"failed",A]); _ -> ok end,
                                     io:format("case: ~tp~nterm: ~tp~n",[X,A]) end, [], T),

          lists:foldl(fun(X,Acc) -> {_,{error,{M,A}}} = {X,om:a(X)},
                                    io:format("case: ~tp, stack: ~tp~n",[M,A]) end, [], F),

          ok.


func(lambda) -> "λ";
func(pi)     -> "∀";
func(arrow)  -> "→";
func(star)   -> "*";
func(Sym)    -> Sym.

ret({[],[X]}) -> X;
ret(Y) -> Y.
