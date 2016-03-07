-module(om).
-description('CoC Compiler').
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1]).
-compile(export_all).

% providing functions

parse(X)    -> om_parse:expr([],om:str([],X),[]).
extract()   -> om_extract:scan().
type(S)     -> om_type:getType(om:term(S)).
modes()     -> ["erased","girard","hurkens","normal","setoids"].
priv(Mode)  -> lists:concat(["priv/",Mode]).
mode(S)     -> application:set_env(om,mode,S).
mode()      -> application:get_env(om,mode,"erased").
debug(S)    -> application:set_env(om,debug,S).
debug()     -> application:get_env(om,debug,false).
a(F)        -> case parse(F) of {error,R} -> {error,R}; {[],[A]} -> A end.

term(F)     -> T = string:tokens(F,"/"),
               P = string:join(rev(tl(rev(T))),"/"),
               term(P,lists:last(T)).

scan()      -> Res = [ {element(1,show(F))/=[],F} || {F} <- lists:umerge(wildcard(),syscard()) ],
               mad:info("Tests: ~tp~n",[Res]),
               Passed = lists:all(fun({X,B}) -> X == true end, Res),
               case Passed of
                    true -> mad:info("PASSED~n",[]);
                    false -> mad:info("FAILED~n",[]) end,
               Res.

show(F)     -> T = string:substr(string:tokens(F,"/"),3),
               Type = term(string:join(T,"/")),
               mad:info("~n~ts~nTerm: ~60tp~n",
                     [file(F),size(term_to_binary(Type))]), Type.

% system functions

main(A)     -> mad:main(A).
start()     -> start(normal,[]).
start(_,_)  -> supervisor:start_link({local,om},om,[]).
stop(_)     -> ok.
init([])    -> mode("normal"), {ok, {{one_for_one, 5, 10}, []}}.

% internal functions

term(P,F)   -> case parse(P,F) of {[],error} -> parse([],F); {[],[]} -> {[],error}; {[],[X]} -> X end.
name(M,P,F) -> string:join(["priv",mode(),case P of [] -> F; _ -> P ++ "/" ++ F end],"/").
str(P,F)    -> om_tok:tokens(P,unicode:characters_to_binary(F),0,{1,[]},[]).
read(P,F)   -> om_tok:tokens(P,file(F),0,{1,[]},[]).
all()       -> lists:flatten([ begin om:mode(M), om:scan() end || M <- modes() ]).
syscard()   -> [ {F} || F <- filelib:wildcard(name(mode(),"**","*")), filelib:is_dir(F) /= true ].

wildcard()  -> lists:flatten([ {A} || {A,B} <- ets:tab2list(filesystem),
               lists:sublist(A,length(om:priv(mode()))) == om:priv(mode()) ]).

parse(P,F)  -> try om_parse:expr(P,read(P,name(mode(),P,F)),[]) catch E:R ->
               mad:info("ERROR: file: ~tp~n~tp~n",[erlang:get_stacktrace(),R]),
               {[],error} end.

% relying functions

rev(X)       -> lists:reverse(X).
flat(X)      -> lists:flatten(X).
tokens(X,Y)  -> string:tokens(X,Y).
debug(S,A)   -> case om:debug() of true -> io:format(S,A); false -> ok end.
atom(X)      -> list_to_atom(X).
last(X)      -> lists:last(X).

file(F) -> case file:read_file(F) of
                {ok,Bin} -> Bin;
                {error,_} -> mad(F) end.

mad(F)  -> case mad_repl:load_file(F) of
                {ok,Bin} -> Bin;
                {error,_} -> <<>> end.
