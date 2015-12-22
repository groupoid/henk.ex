-module(om).
-description('Om Intermediate Compiler').
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, main/1]).
-compile(export_all).

main(A)     -> mad:main(A).
start()     -> start(normal,[]).
start(_,_)  -> supervisor:start_link({local,om},om,[]).
stop(_)     -> ok.
init([])    -> {ok, {{one_for_one, 5, 10}, []}}.
main()      -> Term=om_parse:expr(read(),[]),
               io:fwrite("~70p~n",[Term]),
               ok.
read()      -> Bin = file(),
               io:format("~ts~n",[unicode:characters_to_list(Bin)]),
               om_tok:tokens(Bin,0,{1,[]},[]).
file()      -> {ok,Bin2} = file:read_file("priv/List/@"),
               Bin2.

% om parser depends on three functions:

rev(X)      -> lists:reverse(X).
flat(X)     -> lists:flatten(X).
tokens(X,Y) -> string:tokens(X,Y).
