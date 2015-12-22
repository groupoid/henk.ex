-module(om).
-description('Om Intermediate Compiler').
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1]).
-compile(export_all).

main(A)     -> mad:main(A).
start()     -> start(normal,[]).
start(_,_)  -> supervisor:start_link({local,om},om,[]).
stop(_)     -> ok.
init([])    -> scan(), {ok, {{one_for_one, 5, 10}, []}}.
show(F)     -> error_logger:info_msg("~80p~n~ts~n~p~n",[F,unicode:characters_to_binary(file(F)),parse(F)]).
parse(F)    -> om_parse:expr(read(F),[]).
read(F)     -> om_tok:tokens(file(F),0,{1,[]},[]).
file(F)     -> {ok,Bin} = file:read_file(F), Bin.
scan()      -> [ show(F) || F <- filelib:wildcard("priv/**/*"), not filelib:is_dir(F) ], ok.

% om parser depends on three functions:

rev(X)      -> lists:reverse(X).
flat(X)     -> lists:flatten(X).
tokens(X,Y) -> string:tokens(X,Y).
