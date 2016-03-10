-module(om_parse).
-description('Parser').
-compile(export_all).
-define(arr(F), (F == lambda orelse F== pi)).

%     I := #identifier
%     O := ∅ | ( O ) |
%          □ | ∀ ( I : O ) → O |
%          * | λ ( I : O ) → O |
%          I | O → O | O O

% During forward pass we stack applications, then
% on reaching close paren ")" we perform backward pass and stack arrows,
% until neaarest unstacked open paren "(" appeared (then we just return
% control to the forward pass).

expr(P,[],                       Acc)  ->      rewind2(Acc,[],[]);
expr(P,[close               |T], Acc)  -> case rewind2(Acc, T,[]) of
                                               {error,R} -> {error,R};
                                               {T1,Acc1} -> expr2(P,T1,Acc1) end;

expr(P,[F,open,{var,L},colon   |T], Acc)  when ?arr(F)  -> expr2(P,T,[{'$',{func(F),L}}|Acc]);
expr(P,[{remote,{_,L}}|T],   [{C,Y}|Acc]) when C /= '$' -> expr2(P,T,[{app,{{C,Y},ret(om:parse([],L))}}|Acc]);
expr(P,[{remote,{_,L}}         |T], Acc)                -> expr2(P,T,[ret(om:parse([],L))|Acc]);
expr(P,[{N,X}|T],            [{C,Y}|Acc]) when C /= '$' -> expr2(P,T,[{app,{{C,Y},{N,X}}}|Acc]);
expr(P,[box                    |T], Acc)                -> expr2(P,T,[{box,1}|Acc]);
expr(P,[{N,X}                  |T], Acc)                -> expr2(P,T,[{N,X}|Acc]);
expr(P,[X                      |T], Acc)                -> expr2(P,T,[{X}|Acc]).

rewind([{{':',_},_}|_]=A,T,R)               -> trail(1, ": RET"),   {T,om:flat([R|A])};
rewind([{'$',M}|A],T,[{B,Y}|R])             -> trail(2, ": 1"),     rewind2([{{':',M},{B,Y}}|A],T,R);
rewind([{B,Y},{'$',M}|A],T,R)               -> trail(3, ": 2"),     rewind2([{{':',M},{B,Y}}|A],T,R);
rewind([{C,X},{open},{B,Y}|A],T,R)          -> trail(4, "("),       rewind2([{app,{{B,Y},{C,X}}}|A],T,R);
rewind([{C,X},{open}|A],T,R)                -> trail(4, "("),       rewind2([{C,X}],T,R);
rewind([{arrow},{{':',M},I}|A],T,[{C,X}|R]) -> trail(5, "FUN"),     rewind2([{M,{I,{C,X}}}|A],T,R);
rewind([{C,X},{arrow},{{':',M},I}|A],T,R)   -> trail(7, "FUN 2"),   rewind2([{M,{I,{C,X}}}|A],T,R);
rewind([{arrow},{B,Y}|A],T,[{C,X}|R])       -> trail(6, "ARROW"),   rewind2([{func(arrow),{{B,Y},{C,X}}}|A],T,R);
rewind([{C,X},{arrow},{B,Y}|A],T,R)         -> trail(8, "ARROW 2"), rewind2([{func(arrow),{{B,Y},{C,X}}}|A],T,R);
rewind([],T,R)                              -> trail(10,"[] RET"),  {T,R};
rewind(A,T,R)                               -> trail(11,"CONT"),    {T,om:flat([R|A])}.

trail(I,S)     -> om:debug("~p: FOUND ~tp~n",[I,S]).
expr2(X,T,Y)   -> om:debug("forwrd: ~tp -- ~tp~n",[lists:sublist(T,3),lists:sublist(Y,2)]), expr(X,T,Y).
rewind2(X,T,Y) -> om:debug("rewind: ~tp -- ~tp~n",[lists:sublist(X,3),lists:sublist(Y,2)]), rewind(X,T,Y).

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
