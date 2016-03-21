-module(om_extract).
-description('Extractor').
-compile(export_all).

scan() -> [ extract(X) || X <- filelib:wildcard(om:name([],"/**","*")), filelib:is_dir(X) == true ].

% Erlang AST extraction

replace(X,Y,Z) -> string:join(string:tokens(X,Y),Z).
name(X,F) -> om:cat([X,"/",lists:flatten([F|[]])]).

extract(X)  ->  O = replace(om:pname(X),"/","."),
                io:format("X: ~p~n",[O]),
                save(X, [{attribute,1,module,om:atom(O)},
                      {attribute,1,compile,export_all}]
                  ++ [ begin
                io:format("XF: ~tp~n",[name(X,F)]),
                   extract(F,om:normal(om:fst(om:erase(om:snd(om:parse(om:read(name(X,F))))))),1)
                   end
                       || F <- om:snd(file:list_dir(X)), not filelib:is_dir(name(X,F)) ]
                  ++ [{eof,1}] ),
                [  extract(name(X,Subdir)) || Subdir <- om:snd(file:list_dir(X)), filelib:is_dir(name(X,Subdir)) ],
                ok.

save(X,Forms) -> om:debug("Forms: ~p~n",[Forms]),
               case compile:forms(om:flat(Forms),[debug_info]) of
                 {ok,Name,Bin} -> file:write_file(om:cat([ebin,"/",Name,".beam"]),Bin);
                 Error -> io:format("Extract Error: ~p~n",[X]) end.

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
