-module(om_extract).
-description('Extractor').
-compile(export_all).

prologue(X) -> [{attribute,1,module,X},{attribute,1,compile,export_all}].
scan()      -> [ extract(X) || X <- filelib:wildcard(om:name([],"/**","*")), filelib:is_dir(X) == true ].
extract(X)  -> save ( prologue(om:atom(om:cname(X)))
                 ++ [ extract(F,om:fst(om:erase(om:snd(om:parse(om:read(X++"/"++F))))),1)
                 || F <- om:snd(file:list_dir(X)) ]
                 ++ [{eof,1}] ).

save(Forms) -> om:debug("Forms: ~p~n",[Forms]),
               {ok,Name,Bin} = compile:forms(lists:flatten([Forms])),
               file:write_file(lists:concat([ebin,"/",Name,".beam"]),Bin).

% Erlang AST extraction

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
