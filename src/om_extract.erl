-module(om_extract).
-description('Extract').
-compile(export_all).

prologue(Name)  -> [{attribute,1,module,Name},{attribute,1,compile,export_all}].
scan()          -> [ extract(F) || F <- filelib:wildcard(string:join(["priv",om:mode(),"**","*"],"/")), filelib:is_dir(F) == true ].
extract(F,T,C)  -> case ext(F,T,C+1) of [] -> []; Ex -> {function,C,om:atom(F),0,[{clause,C,[],[],[Ex]}]} end.
extract(X)      -> Last = om:last(string:tokens(X,"/")),
                   io:format("Type: ~p at ~p~n",[Last,X]),
                   put(x,0),
                   Forms = prologue(om:atom(Last))
                      ++ [ begin Name = string:join([Last,F],"/"),
                                 io:format("Ctor: ~tp~n",[Name]),
                                 om:show(string:join([X,F],"/")),
                                 %C = put(x,get(x)+1),
                                 Erased = case om:mode() of "normal" -> erasure(F,om:type(Name),get(x)+1);
                                                            "erased" -> om:type(Name) end,
                                 io:format("Erased: ~tp~n",[Erased]),
                                 Extract = extract(F,Erased,get(x)+1),
                                 io:format("Tree: ~tp~n",[Extract]),
                                 Extract
                     end || F <- element(2,file:list_dir(X)) ] ++ [{eof,1}],
                  io:format("Forms: ~tp~n",[Forms]),
                   {ok,Name,Bin} = compile:forms(lists:flatten([Forms])),
                   file:write_file(lists:concat([ebin,"/",Name,".beam"]),Bin).

erasure(F,{{"∀",Name},{In,Out}},N)   -> []; % eraseLambda(F,{"λ",Name},In,Out,N);
erasure(F,{"→",{_,Out}},N)           -> [];
erasure(F,{{"λ",Name},{In,Out}}=T,N) -> eraseLambda(F,{"λ",Name},In,Out,N+1);
erasure(F,{app,{A,B}},N)             -> eraseApp(erasure(F,A,N),erasure(F,B,N+1));
erasure(F,{var,{Name,I}},N)          -> {var,{Name,N}}.

%eraseLambda(F,{"λ",Name},{const,"*"},Out,N) -> erasure(F,Out,N+1);
eraseLambda(F,{"λ",Name},{"∀",_},Out,N)     -> erasure(F,Out,N+1);
eraseLambda(F,{"λ",Name},In,Out,N)          -> {{"λ",Name},{In,erasure(F,Out,N+1)}};
eraseLambda(F,_,_,Out,N)                    -> [].

eraseApp([],[]) -> [];
eraseApp(EA,[]) -> EA;
eraseApp([],EB) -> EB;
eraseApp(EA,EB) -> {app,{EA,EB}}.

% Erlang AST extraction

ext(F,[],N)                    -> [];
ext(F,{{"∀",Name},{_,Out}},N)  -> [];
ext(F,{"→",{_,Out}},N)         -> [];
ext(F,{{"λ",Name},{_,Out}},N) -> {'fun',N,{clauses,[{clause,N,[{var,N,Name}],[],[ext(F,Out,N)]}]}};
ext(F,{app,{A,B}},N)          -> {call,N,ext(F,A,N),[ext(F,B,N)]};
ext(F,{var,{Name,I}},N)       -> {var,N,Name}.

