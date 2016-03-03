-module(om_extract).
-description('Extract').
-compile(export_all).

prologue(Name)  -> [{attribute,1,module,Name},{attribute,1,compile,export_all}].
scan()          -> [ extract(F) || F <- filelib:wildcard(string:join(["priv",om:mode(),"**","*"],"/")), filelib:is_dir(F) == true ].
extract(F,T,C)  -> case ext(F,T,C) of [] -> []; Ex -> {function,C,om:atom(F),0,[{clause,C,[],[],[Ex]}]} end.
extract(X)      -> Last = om:last(string:tokens(X,"/")),
                   mad:info("Type: ~p at ~p~n",[Last,X]),
                   Forms = prologue(om:atom(Last))
                      ++ [ begin Name = string:join([Last,F],"/"),
                                 mad:info("Ctor: ~tp~n",[Name]),
                                 om:show(string:join([X,F],"/")),
                                 Erased = case om:mode() of "erased" -> om:term(Name);
                                                            _ -> erasure(F,om:term(Name),1) end,
                                 mad:info("Erased: ~tp~n",[Erased]),
                                 Extract = extract(F,Erased,1),
                                 mad:info("Tree: ~tp~n",[Extract]),
                                 Extract
                     end || F <- element(2,file:list_dir(X)), F /= "@" ] ++ [{eof,1}],
                   {ok,Name,Bin} = compile:forms(lists:flatten([Forms])),
                   file:write_file(lists:concat([ebin,"/",Name,".beam"]),Bin).

% Erase Type Information from Normalized Files

erasure(F,{{"∀",Name},{In,Out}},N)   -> []; %erasure(F,Out,N+1);
erasure(F,{"→",{_,Out}},N)           -> []; %erasure(F,Out,N+1);
erasure(F,{{"λ",Name},{In,Out}}=T,N) -> eraseLambda(F,{"λ",Name},In,Out,N);
erasure(F,{app,{A,B}},N)             -> eraseApp(erasure(F,A,N),erasure(F,B,N));
erasure(F,{var,{Name,I}},N)          -> {var,{Name,N}};
erasure(F,_,N)                       -> [].

eraseLambda(F,{"λ",Name},{"∀",_},Out,N)     -> []; %erasure(F,Out,N+1);
eraseLambda(F,{"λ",Name},"→",Out,N)         -> []; %erasure(F,Out,N+1);
eraseLambda(F,{"λ",Name},In,Out,N)          -> {{"λ",Name},{In,erasure(F,Out,N)}};
eraseLambda(F,_,_,Out,N)                    -> [].

eraseApp([],[]) -> [];
eraseApp(EA,[]) -> EA;
eraseApp([],EB) -> EB;
eraseApp(EA,EB) -> {app,{EA,EB}}.

% Erlang AST extraction

ext(F,[],N)                    -> [];
ext(F,{{"∀",Name},{_,Out}},N)  -> [];
ext(F,{"→",{_,Out}},N)         -> [];
ext(F,{{"λ",{Name,_}},{_,Out}},N) -> {'fun', N,{clauses,[{clause,N,[{var,N,Name}],[],[ext(F,Out,N)]}]}};
ext(F,{app,{A,B}},N)          -> {'call',N,ext(F,A,N),[ext(F,B,N)]};
ext(F,{var,{Name,I}},N)       -> {'var', N,Name}.

