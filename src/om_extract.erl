-module(om_extract).
-description('Extractor').
-compile(export_all).

scan() -> Root = "priv/",
        [ begin om:restart(), om:mode(X), extract(om:cat([Root,X])) end
          || X <- om:snd(file:list_dir(Root)), lists:member(X,om:modes()) ].

replace(X,Y,Z) -> string:join(string:tokens(X,Y),Z).
name(X,F) -> om:cat([X,"/",lists:flatten([F|[]])]).
normal("",X) -> om:cname(X);
normal(A,X)  -> A.

extr(X) -> om:restart(), om:mode(X), extract("priv/"++X).
extract(X)  ->  O = replace(om:pname(X),"/","."),
                save(X, [{attribute,1,module,om:atom(normal(O,X))},
                         {attribute,1,compile,export_all}] ++ [ begin
                         Norm = om:norm(om:fst(om:erase(om:snd(om:parse(om:read(name(X,F))))))),
                         Ext = extract(F,Norm,1),
                         io:format("Trace: ~p~n",[{X,F,Ext}]),
                         Ext
                   end || F <- element(2,file:list_dir(X)), not filelib:is_dir(name(X,F)) ] ++ [{eof,1}] ),
                [  extract(name(X,Subdir)) || Subdir <- om:snd(file:list_dir(X)), filelib:is_dir(name(X,Subdir)) ],
                ok.

save(X,Forms) -> case compile:forms(om:flat(Forms),[debug_info]) of
                 {ok,Name,Bin} -> file:write_file(om:cat([ebin,"/",Name,".beam"]),Bin);
                 Error -> om:info(?MODULE,"Extract Error: ~p~n",[Forms]) end.

extract(F,T,C) -> case ext(F,T,C) of
                       [] -> [];
                       Ex -> {function,C,om:atom(F),0,[{clause,C,[],[],[Ex]}]} end.

ext(F,[],N)                       -> [];
ext(F,{{"∀",Name},{_,Out}},N)     -> [];
ext(F,{"→",{_,Out}},N)            -> [];
ext(F,{{"λ",{Name,_}},{_,Out}},N) -> {'fun', N,{clauses,[{clause,N,[{var,N,Name}],[],[ext(F,Out,N)]}]}};
ext(F,{app,{A,B}},N)              -> {'call',N,ext(F,A,N),[ext(F,B,N)]};
ext(F,{var,{Name,I}},N)           -> {'var', N,Name};
ext(F,_,N)                        -> [].
