-module(om_parse).
-description('Parser').
-compile(export_all).
-define(arr(F), (F == lambda orelse F== pi)).
-define(int(F), (F == ':' orelse  F == '$')).
-define(ast(F), (F /= ':' andalso F /= '$')).

%     I := #identifier
%     O := ∅ | ( O ) |
%          □ | ∀ ( I : O ) → O |
%          * | λ ( I : O ) → O |
%          I | O → O | O O

expr(P,[],                       Acc)  ->      rewind2(Acc,[],[]);
expr(P,[close               |T], Acc)  -> %om:debug("backwd: ~tp~n",[Acc]),
                                          case rewind2(Acc, T,[]) of
                                               {error,R} -> {error,R};
                                               {T1,Acc1} -> expr2(P,T1,Acc1) end;

expr(P,[F,open,{var,L},colon   |T], Acc)  when ?arr(F)  -> expr2(P,T,[{'$',{func(F),L}}|Acc]);
expr(P,[{remote,{_,L}}|T],   [{C,Y}|Acc]) when C /= '$' -> expr2(P,T,[{app,{{C,Y},ret(om:parse([],L))}}|Acc]);
expr(P,[{remote,{_,L}}         |T], Acc)                -> expr2(P,T,[ret(om:parse([],L))|Acc]);
expr(P,[{N,X}|T],            [{C,Y}|Acc]) when C /= '$' -> expr2(P,T,[{app,{{C,Y},{N,X}}}|Acc]);
expr(P,[box                    |T], Acc)                -> expr2(P,T,[{box,1}|Acc]);
expr(P,[{N,X}                  |T], Acc)                -> expr2(P,T,[{N,X}|Acc]);
expr(P,[X                      |T], Acc)                -> expr2(P,T,[{X}|Acc]).

expr2(X,T,Y) ->
    %om:debug("forwrd: ~tp -- ~tp~n",[lists:sublist(T,3),lists:sublist(Y,2)]),
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


%rewind([],            T,[{"→",    {{app,X},Y}}            |R])              -> {error,{"parser1",{X,Y}}};
%rewind([{F},{open},{'$',M}|A],T, [{C,Y}|R]) when ?arr(F) -> {T,om:flat([{{func(F),M},{{L,{C,X}},Y}}|[R|A]])};
%rewind([{F}|A],       T,[{"→",{{L,{{app,{{'$',M},C}},X}},Y}}|R]) when ?arr(F) -> {error,{"parser2",{M,C,X,Y}}};
%rewind([{F}|A],       T,[{"→",    {{app,{{'$',{L,M}},X}},Y}}|R]) when ?arr(F) -> rewind2(A,T,[{{func(F),{L,M}},{X,Y}}|R]);
%rewind([{open},{F}|A],T,[{"→",{{L,{{app,{{'$',M},C}},X}},Y}}|R]) when ?arr(F) -> {T,om:flat([{{func(F),M},{{L,{C,X}},Y}}|[R|A]])};
%rewind([{open},{F}|A],T,[{"→",    {{app,{{'$',{L,M}},X}},Y}}|R]) when ?arr(F) -> {T,om:flat([{{func(F),{L,M}},{X,Y}}|[R|A]])};
%rewind([{open},{F}|A],T,[{app,{{'$',M},X}}|R])                   when ?arr(F) -> {T,om:flat([{{func(F),M},X}|[R|A]])};
%rewind([{open},{'$',Z}|A],T,               [{I, {{app,X},Y}}|R])              -> {error,{"parser4",{Z,X,Y}}};
%rewind([{open},{'$',Z}|A],T,                        [{app,X}|R])              -> {error,{"parser3",{Z,X}}};
%rewind([{C,X}|A],     T,                            [{B,Y}|R])              -> rewind2(A,T,[{app,{{C,X},{B,Y}}}|R]);
%rewind([{C,X}|A],     T,                                   R)               -> rewind2(A,T,[{C,X}|R]);
%rewind([{open},{var,X}|A],T,                        [{B,Y}|R])              -> {T,om:flat([{app,{{var,X},{B,Y}}}|[R|A]])};
%rewind([{open}|A],    T,                                   R)               -> {T,om:flat([R|A])};
%rewind([{arrow},{C,X}|A], T,                        [{B,Y}|R])              -> rewind2(A,T,[{func(arrow),{{B,Y},{C,X}}}|R]);
%rewind([],            T,                                   R)               -> {T,R}.

%rewind([{'$',M},{arrow}|A],T,[{B,Y}|R])            -> {T,om:flat([{{':',M},{B,Y}}|[R|A]])};
%rewind([{'$',M}|A],        T,[{B,Y}|R])            -> {T,om:flat([{{':',M},{B,Y}}|[R|A]])};
%rewind([{':',M}|A],        T,[{B,Y}|R])            -> {T,om:flat([{M,{B,Y}}|[R|A]])};
%rewind([{B,Y},{'$',M}|A],T,R)                      -> io:format("Fold~n"),
%                                                      {T,om:flat([{{':',M},{B,Y}}|[R|A]])};
%rewind([{C,X},{arrow},{'$',M}|A],T,R)              -> {T,om:flat([{{':',M},{C,X}}|[R|A]])};
%                                                      %rewind2(A,T,[{{':',M},{C,X}}|R]);
%rewind([{C,X},{arrow},{{':',M},I}|A],T,R)          -> %{T,om:flat([{M,{I,{C,X}}}|[R|A]])};
%                                                      rewind2(A,T,[{M,{I,{C,X}}}|R]);
%rewind([{arrow},{{':',M},I}|A],T,[{B,Y}|R])        -> rewind2(A,T,[{M,{I,{B,Y}}}|R]);
%                                                      %{T,om:flat([{M,{I,{B,Y}}}|[R|A]])};
%rewind([{arrow},{B,Y}|A],T,[{C,X}|R])              -> rewind2(A,T,[{func(arrow),{{B,Y},{C,X}}}|R]);
%rewind([{C,X},{arrow},{B,Y}|A],T,R)                -> rewind2(A,T,[{func(arrow),{{B,Y},{C,X}}}|R]);
%rewind([{C,X},{open},{B,Y},{'$',M}|A],T,R) when ?ast(C) -> {T,om:flat([{{':',M},{app,{B,Y},{C,X}}}|[R|A]])};
%rewind([{C,X},{open},{B,Y}|A],T,R) when ?ast(C)    -> io:format("App~n"),
                                                      %{T,om:flat([{app,{B,Y},{C,X}}|[R|A]])};
%                                                      rewind2(A,T,[{app,{B,Y},{C,X}}|R]); 
%rewind([{B,Y},{open}|A],T,R)                       -> {T,om:flat([{B,Y}|[R|A]])};

%rewind([{'$',M}|A],T,[{B,Y}|R]) when ?ast(B)       -> io:format("S3: ~tp~n",["MEET 2$ RETURN"]),
%                                                          rewind2(A,T,om:flat([{M,{B,Y}}|R]));
%                                                          %   {T,om:flat([R|A])};

%rewind([{'$',M}|A],T,[{B,Y}|R])                        -> io:format("S2: ~tp~n",["CONTINUE :"]),
                                                          %{T,om:flat([{'$',M}|[R|A]])};
%                                                          rewind2(A,T,om:flat([{{':',M},{B,Y}}|R]));
%rewind([{':',M}|A],        T,[{B,Y}|R])                -> io:format("S1: ~tp~n",[[]]),
%                                                          rewind2(A,T,om:flat([{M,{B,Y}}|R]));


rewind([{'$',_}|_]=A,T,[{{':',_},_}|_]=R)             -> om:debug("S1: ~tp~n",["CAN'T FOLD $. RETURN"]),
                                                          {T,om:flat([R|A])};

rewind([{{':',_},_}|_]=A,T,R)                          -> om:debug("S1: ~tp~n",["CAN'T FOLD :. RETURN"]),
                                                          {T,om:flat([R|A])};

rewind([{'$',{F,M}}|A],T,[{B,Y}|R])                       -> om:debug("S2: ~tp~n",["FOLD :"]),
                                                          rewind2(A,T,[{{':',{F,M}},{B,Y}}|R]);

rewind([{B,Y},{'$',M}|A],T,R)                         -> om:debug("S3: ~tp~n",["BUILD :"]),
                                                          rewind2(A,T,[{{':',M},{B,Y}}|R]);

rewind([{C,X},{open},{B,Y}|A],T,R)                     -> om:debug("S1: ~tp~n",["FOUND (. RETURN"]),
                                                          rewind2([{app,{{B,Y},{C,X}}}|A],T,R);

rewind([{arrow},{{':',M},I}|_]=A,T,[{{':',_},X}|_]=R)      -> om:debug("S4: ~tp~n",["FOUND FUN" ]),
                                                         {T,om:flat([R|A])};

rewind([{arrow},{{':',M},I}|A],T,[{C,X}|R])           -> om:debug("S5: ~tp~n",["FOUND FUN"]),
                                                         rewind2(A,T,[{M,{I,{C,X}}}|R]);

rewind([{arrow},{{':',M},I}|A],T,[{C,X}|R])           -> om:debug("S5: ~tp~n",["FOUND FUN"]),
                                                         rewind2(A,T,[{M,{I,{C,X}}}|R]);

rewind([{arrow},{B,Y}|A],T,[{C,X}|R])           -> om:debug("S5: ~tp~n",["FOUND ARROW"]),
                                                         rewind2(A,T,[{func(arrow),{{B,Y},{C,X}}}|R]);

rewind([{C,X},{arrow},{{':',M},I}|A],T,R)              -> om:debug("S6: ~tp~tp~n",["FOUND FUN 2 ",{M,I,C,X}]),
                                                          rewind2(A,T,[{M,{I,{C,X}}}|R]);


rewind([{C,X},{arrow},{B,Y}|A],T,R)                -> om:debug("S7: ~tp~n",["MEET [] RETURN"]),
                                                          rewind2(A,T,[{func(arrow),{{B,Y},{C,X}}}|R]);

rewind([],T,R)                                         -> om:debug("S9: ~tp~n",["MEET [] RETURN"]),
                                                          {T,R};

rewind(A,T,R)                                         -> om:debug("S8: ~tp~n",["CONTINUE"]),
                                                          {T,om:flat([R|A])}.

rewind2(X,T,Y) ->
    om:debug("rewind A: ~tp~nrewind R: ~tp~n",[lists:sublist(X,14),lists:sublist(Y,2)]),
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
