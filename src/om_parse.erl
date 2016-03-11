-module(om_parse).
-description('Parser').
-compile(export_all).
-define(arr(F), (F == lambda orelse F== pi)).

expr(P,[],                         A,{V,D}) ->      rewind2(A,{V,D},[]);
expr(P,[close                 |T], A,{V,D}) -> case rewind2(A,{D,D},[]) of
                                                {error,R} -> {error,R};
                                                {{V1,D1},Acc1} -> expr2(P,T,Acc1,{V1,D1}) end;
expr(P,[F,open,{var,L},colon  |T], Acc, {V,D}) when ?arr(F)   -> expr2(P,T,[{'$',{func(F),L}}|Acc],{V,D+1});
expr(P,[{remote,{_,L}}|T],  [{C,Y}|Acc],{V,D}) when C /= '$'  -> expr2(P,T,[{app,{{C,Y},ret(om:parse([],L))}}|Acc],{V,D});
expr(P,[{remote,{_,L}}        |T], Acc, {V,D})                -> expr2(P,T,[ret(om:parse([],L))|Acc],{V,D});
expr(P,[{N,X}|T],           [{C,Y}|Acc],{V,D}) when C /= '$'  -> expr2(P,T,[{app,{{C,Y},{N,X}}}|Acc],{V,D});
expr(P,[box                   |T], Acc, {V,D})                -> expr2(P,T,[{box,1}|Acc],{V,D});
expr(P,[{N,X}                 |T], Acc, {V,D})                -> expr2(P,T,[{N,X}|Acc],{V,D});
expr(P,[open                  |T], Acc, {V,D})                -> expr2(P,T,[{open}|Acc],{V,D+1});
expr(P,[X                     |T], Acc, {V,D})                -> expr2(P,T,[{X}|Acc],{V,D}).

rewind([{{':',_},_}|_]=A,{V,D},R)                -> trail(1, ": RET"),   {{V,D-1},om:flat([R|A])};
rewind([{'$',M}|A],{V,D},[{B,Y}|R])              -> trail(2, ": 1"),     rewind2([{{':',M},{B,Y}}|A],{V,D},R);
rewind([{B,Y},{'$',M}|A],{V,D},R) when V == D    -> trail(3, "$ -> :"),  rewind2([{{':',M},{B,Y}}|A],{V,D},R);
rewind([{B,Y},{'$',M}|_]=A,{V,D},R)              -> trail(4, "$ RET"),   {{V,D},om:flat([A|R])};
rewind([{C,X},{open},{{':',M},I}|A],{V,D},   R)  -> trail(5, "("),       {{V,D-1},om:flat([{C,X},{{':',M},I}|[R|A]])};
rewind([{C,X},{open},{'$',M}|A],{V,D},       R)  -> trail(6, "("),       {{V,D-1},om:flat([{C,X},{'$',M}    |[R|A]])};
rewind([{C,X},{open},{open} |A],{V,D},       R)  -> trail(7, "("),       {{V,D},om:flat([{C,X},{open}     |[R|A]])};
rewind([{C,X},{open},{B,Y}  |A],{V,D},       R)  -> trail(8, "("),       {{V,D-1},om:flat([{app,{{B,Y},{C,X}}}|[R|A]])};
rewind([{arrow},{{':',M},I} |A],{V,D},[{C,X}|R]) -> trail(9, "FUN"),     rewind2([{M,{I,{C,X}}}|A],{V,D},R);
rewind([{C,X},{arrow},{{':',M},I}|A],{V,D}, R)   -> trail(10, "FUN 2"),  rewind2([{M,{I,{C,X}}}|A],{V,D},R);
rewind([{arrow},{B,Y}       |A],{V,D},[{C,X}|R]) -> trail(11, "ARROW"),  rewind2([{func(arrow),{{B,Y},{C,X}}}|A],{V,D},R);
rewind([{C,X},{arrow},{B,Y} |A],{V,D},       R)  -> trail(12, "ARROW 2"),rewind2([{func(arrow),{{B,Y},{C,X}}}|A],{V,D},R);
rewind([],                      {V,D},       R)  -> trail(13,"[] RET"),  {{V,D},om:flat(R)};
rewind(A,                       {V,D},       R)  -> trail(14,"CONT"),    {{V,D},om:flat([R|A])}.

% Syntax and Algorithm

%     I := #identifier
%     O := ∅ | ( O ) |
%          □ | ∀ ( I : O ) → O |
%          * | λ ( I : O ) → O |
%          I | O → O | O O

% During forward pass we stack applications, then
% on reaching close paren ")" we perform backward pass and stack arrows,
% until neaarest unstacked open paren "(" appeared (then we just return
% control to the forward pass).

trail(I,S)     -> om:debug("~p: FOUND ~tp~n",[I,S]).
expr2(X,T,Y,C) -> %om:debug("forwrd: ~tp -- ~tp~n",[lists:sublist(T,3),lists:sublist(Y,2)]),
                  expr(X,T,Y,C).
rewind2(X,T,Y) -> om:debug("rewind: ~tp -~p- ~tp~n",[lists:sublist(X,40),T,lists:sublist(Y,2)]), rewind(X,T,Y).

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

ret({_,[X]}) -> X;
ret(Y) -> Y.
%io:format("Y: ~p~n",[Y]), {error,Y}.
