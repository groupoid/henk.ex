-module(om_parse).
-description('Parser').
-compile(export_all).

%     I := #identifier
%     O := ∅ | ( O ) |
%          □ | ∀ ( I : O ) → O |
%          * | λ ( I : O ) → O |
%          I | O → O | O O

expr(P,[],                       Acc)  ->      rewind2(Acc,[],[]);
expr(P,[close               |T], Acc)  -> om:debug("backwd: ~tp~n",[Acc]),
                                          case rewind(Acc, T,[]) of
                                               {error,R} -> {error,R};
                                               {T1,Acc1} -> expr2(P,T1,Acc1) end;

expr(P,[{remote,{_,L}}|T], [{C,Y}|Acc]) -> expr2(P,T,[{app,{{C,Y},ret(om:parse([],L))}}|Acc]);
expr(P,[{remote,{_,L}}|T],        Acc)  -> expr2(P,T,[ret(om:parse([],L))|Acc]);
expr(P,[{star,I}             |T], Acc)  -> expr2(P,T,[{star,I}|Acc]);
expr(P,[{N,X}|T],          [{a,Y}|Acc]) -> expr2(P,T,[{N,X},{a,Y}|Acc]);
expr(P,[{N,X}|T],          [{C,Y}|Acc]) -> expr2(P,T,[{app,{{C,Y},{N,X}}}|Acc]);
expr(P,[star                 |T], Acc)  -> expr2(P,T,[{star,1}|Acc]);
expr(P,[box                  |T], Acc)  -> expr2(P,T,[{box,1}|Acc]);
expr(P,[open                 |T], Acc)  -> expr2(P,T,[{open}|Acc]);
expr(P,[arrow                |T], Acc)  -> expr2(P,T,[{arrow}|Acc]);
expr(P,[lambda               |T], Acc)  -> expr2(P,T,[{lambda}|Acc]);
expr(P,[pi                   |T], Acc)  -> expr2(P,T,[{pi}|Acc]);
expr(P,[colon                |T], Acc)  -> expr2(P,T,[{colon}|Acc]);
expr(P,[{var,L},colon        |T], Acc)  -> expr2(P,T,[{a,L}|Acc]);
expr(P,[{var,L}              |T], Acc)  -> expr2(P,T,[{var,L}|Acc]).

expr2(X,T,Y) ->
    om:debug("forwrd: ~tp -- ~tp~n",[lists:sublist(X,2),lists:sublist(Y,1)]),
    expr(X,T,Y).

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
%                  lambda: arrow(app(arg(x),A),B)
%

-define(arr(F), F == lambda; F== pi).

rewind([],            T,[{"→",    {{app,X},Y}}            |R])              -> {error,{"parser1",{X,Y}}};
rewind([{F}|A],       T,[{"→",{{L,{{app,{{a,M},C}},X}},Y}}|R]) when ?arr(F) -> {error,{"parser2",{M,C,X,Y}}};
rewind([{F}|A],       T,[{"→",    {{app,{{a,{L,M}},X}},Y}}|R]) when ?arr(F) -> rewind2(A,T,[{{func(F),{L,M}},{X,Y}}|R]);
rewind([{open},{F}|A],T,[{"→",{{L,{{app,{{a,M},C}},X}},Y}}|R]) when ?arr(F) -> {T,om:flat([{{func(F),M},{{L,{C,X}},Y}}|[R|A]])};
rewind([{open},{F}|A],T,[{"→",    {{app,{{a,{L,M}},X}},Y}}|R]) when ?arr(F) -> {T,om:flat([{{func(F),{L,M}},{X,Y}}|[R|A]])};
rewind([{open},{a,Z}|A],T,               [{I, {{app,X},Y}}|R])              -> {error,{"parser4",{Z,I,X,Y}}};
rewind([{open},{a,Z}|A],T,                        [{app,X}|R])              -> {error,{"parser3",{Z,X}}};
rewind([{open},{var,X}|A],T,                        [{B,Y}|R])              -> {T,om:flat([{app,{{var,X},{B,Y}}}|[R|A]])};
rewind([{open}|A],    T,                                   R)               -> {T,om:flat([R|A])};
rewind([{C,X}|A],     T,                            [{B,Y}|R])              -> rewind2(A,T,[{app,{{C,X},{B,Y}}}|R]);
rewind([{C,X}|A],     T,                                   R)               -> rewind2(A,T,[{C,X}|R]);
rewind([{arrow},Y|A], T,                                [X|R])              -> rewind2(A,T,[{func(arrow),{Y,X}}|R]);
rewind([],            T,                                   R)               -> {T,R}.

rewind2(X,T,Y) ->
    om:debug("rewind: ~tp -- ~tp~n",[lists:sublist(X,2),lists:sublist(Y,1)]),
    rewind(X,T,Y).

test() -> F = [ "(x : ( \\ (o:*) -> o ) -> p ) -> o",        % parser1
                "\\ (x : ( err (o:*) -> o ) -> p ) -> o",    % parser3
                "\\ (x : ( (o:*) -> o ) -> p ) -> o",        % parser4
                "\\ (x : ( \\ (o:*) -> o ) -> p ) err -> o", % parser2
                "\\ (x : \\ (x: x -> l) -> o ) l -> z",      % parser2
                "\\ (x : ( (o:*) -> o ) -> p ) -> o"         % parser4
              ],

          T = [ "\\ (x : (\\ (o:*) -> o) l -> p ) -> o",
                "\\ (x : ( \\ (o: \\ (x : (\\ (o:*) -> o) l -> p ) -> o) -> o ) -> p ) -> o",
                "* -> a \\ (x : a (\\ (o:*) -> o) l -> p ) -> o",
                 "\\(x : (\\ (o:*) -> o) -> p ) -> o"
               ],

          TT = lists:foldl(fun(X,Acc) ->  {X,{M,_}=A} = {X,om:a(X)},
                                     case M of error -> erlang:error(["test",X,"failed",A]); _ -> ok end,
                                     [{X,A}|Acc] end, [], T),

          FF =lists:foldl(fun(X,Acc) -> {X,{error,{M,A}}} = {X,om:a(X)},
                                    [{M,X,A}|Acc] end, [], F),

          FF ++ TT.

pad(D)                         -> lists:duplicate(D,"  ").

print(any,D)                   -> ["any"];
print({var,{N,I}},D)           -> [ om:cat(N) ];
print({star,N},D)              -> [ "*",om:cat(N) ];
print({"→",{I,O}},D)           -> [ "(", print(I,D+1),"\n",pad(D),"→ ",print(O,D), ")\n" ];
print({app,{I,O}},D)           -> [ "(",print(I,D)," ",print(O,D),")" ];
print({{"∀",{N,_}},{any,O}},D) -> [ "( ∀ ",om:cat(N),"\n",pad(D),"→ ",print(O,D),")" ];
print({{"∀",{N,_}},{I,O}},D)   -> [ "( ∀ (",om:cat(N),": ",print(I,D+1),")\n",pad(D),"→ ",print(O,D),")" ];
print({{"λ",{N,_}},{any,O}},D) -> [ "( λ ",om:cat(N),"\n",pad(D),"→ ",print(O,D),")" ];
print({{"λ",{N,_}},{I,O}},D)   -> [ "( λ (",om:cat(N),": ",print(I,D+1),")\n",pad(D),"→ ",print(O,D),")" ].

func(lambda) -> "λ";
func(pi)     -> "∀";
func(arrow)  -> "→";
func(star)   -> "*";
func(Sym)    -> Sym.

ret({[],[X]}) -> X;
ret(Y) -> {error,Y}.
