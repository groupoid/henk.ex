-module(om).
-description('Om Intermediate Compiler').
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1]).
-compile(export_all).

% provided functions

main(A)     -> mad:main(A).
start()     -> start(normal,[]).
start(_,_)  -> supervisor:start_link({local,om},om,[]).
stop(_)     -> ok.
mode()      -> application:get_env(om,mode,"erased").
init([])    -> {ok, {{one_for_one, 5, 10}, []}}.
type(F)     -> T = string:tokens(F,"/"), P = string:join(rev(tl(rev(T))),"/"), type(P,lists:last(T)).
type(P,F)   -> case parse(P,F) of {[],error} -> parse(P,F); {[],[X]} -> X end.
parse(P,F)  -> try om_parse:expr(P,read(P,string:join(["priv",mode(),P,F],"/")),[]) catch E:R -> io:format("ERROR: ~p:~p~n",[R,erlang:get_stacktrace()]), {[],error} end.
str(P,F)    -> om_tok:tokens(P,unicode:characters_to_binary(F),0,{1,[]},[]).
a(F)        -> {[],[X]} = om_parse:expr([],om_tok:tokens([],list_to_binary(F),0,{1,[]},[]),[]), X.
read(P,F)   -> om_tok:tokens(P,file(F),0,{1,[]},[]).
file(F)     -> {ok,Bin} = read_file(F), Bin.
scan()      -> [ show(F) || F <- filelib:wildcard(string:join(["priv",mode(),"**","*"],"/")), filelib:is_dir(F) /= true ], ok.
show(F)     -> T = string:substr(string:tokens(F,"/"),3),
               error("~ts~n~ts~nsize: ~p~n",[F,file(F),size(term_to_binary(type(string:join(T,"/"))))]).

% relying function

rev(X)       -> lists:reverse(X).
flat(X)      -> lists:flatten(X).
tokens(X,Y)  -> string:tokens(X,Y).
print(S,A)   -> io_lib:format(S,A).
error(S,A)   -> io:format(S,A).
read_file(F) -> file:read_file(F).
atom(X)      -> list_to_atom(X).
last(X)      -> lists:last(X).

