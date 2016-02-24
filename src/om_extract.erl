-module(om_extract).
-description('Extract').
-compile(export_all).

prologue(Name)  -> [{attribute,1,module,Name},{attribute,1,compile,export_all}].
scan()          -> [ extract(F) || F <- filelib:wildcard(string:join(["priv",om:mode(),"**","*"],"/")), filelib:is_dir(F) == true ].
extract(F,Term) -> {function,1,om:atom(F),0,[{clause,1,[],[],[ext(F,Term,1)]}]}.
extract(X)      -> Last = om:last(string:tokens(X,"/")),
                   io:format("Type: ~p at ~p~n",[Last,X]),
                   Forms = prologue(om:atom(Last))
                      ++ [ begin
                                Name = string:join([Last,F],"/"),
                                io:format("Ctor: ~p~n",[Name]),
                                om:show(string:join([X,F],"/")),
                                extract(F,om:type(Name)) end || F <- element(2,file:list_dir(X)) ]
                      ++ [{eof,1}],
                   {ok,Name,Bin} = compile:forms(Forms),
                   file:write_file(lists:concat([ebin,"/",Name,".beam"]),Bin).

% cartesian closed Erlang AST extraction

ext(F,{{"λ",Name},{_,Out}},N) -> {'fun',N,{clauses,[{clause,N,[{var,N,Name}],[],[ext(F,Out,N)]}]}};
ext(F,{app,{A,B}},N) -> {call,N,ext(F,A,N),[ext(F,B,N)]};
ext(F,{var,{Name,I}},N) -> {var,N,Name}.

