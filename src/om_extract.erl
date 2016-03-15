-module(om_extract).
-description('Extractor').
-compile(export_all).

scan() -> [ extract(X) || X <- filelib:wildcard(om:name([],"/**","*")), filelib:is_dir(X) == true ].

% Erlang AST extraction

extract(X)  -> save( [{attribute,1,module,om:atom(om:cname(X))},
                      {attribute,1,compile,export_all}]
                  ++ [ extract(F,om:fst(om:erase(om:normal(om:snd(om:parse(om:read(X++"/"++F)))))),1)
                       || F <- om:snd(file:list_dir(X)) ]
                  ++ [{eof,1}] ).

save(Forms) -> om:debug("Forms: ~p~n",[Forms]),
               {ok,Name,Bin} = compile:forms(om:flat(Forms),[debug_info]),
               file:write_file(om:cat([ebin,"/",Name,".beam"]),Bin).

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
