-module(om).
-description('Om Intermediate Compiler').
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1]).
-compile(export_all).

% provided functions

a(X)        -> {_,[R]}=om_parse:expr(flat([om:str(X)]),[]),R.
main(A)     -> mad:main(A).
start()     -> start(normal,[]).
start(_,_)  -> supervisor:start_link({local,om},om,[]).
stop(_)     -> ok.
init([])    -> scan(), {ok, {{one_for_one, 5, 10}, []}}.
type(F)     -> Name = lists:concat(["priv/Om/",F]), {_,[X]}=parse(Name), X.
type_(F)    -> Name = lists:concat(["priv/Om/",F]), io:format("[~ts]:~n~ts~n~n",[Name,om:file(Name)]), {_,[X]}=parse(Name), X.
parse(F)    -> try om_parse:expr(read(F),[]) catch E:R -> io:format("PARSE: ~tp:~tp~nin ~s~n",[E,R,F]), {[],[[]]} end.
str(F)      -> om_tok:tokens(unicode:characters_to_binary(F),0,{1,[]},[]).
read(F)     -> om_tok:tokens(file(F),0,{1,[]},[]).
file(F)     -> {ok,Bin} = read_file(F), Bin.
scan()      -> [ show(F) || F <- filelib:wildcard("priv/Om/**/*"), filelib:is_dir(F) /= true ], ok.
show(F)     -> error("~130p~n~ts~n~130tp~n",[F,file(F),parse(F)]).

% relying function

rev(X)       -> lists:reverse(X).
flat(X)      -> lists:flatten(X).
tokens(X,Y)  -> string:tokens(X,Y).
print(S,A)   -> io_lib:format(S,A).
error(S,A)   -> error_logger:info_msg(S,A).
read_file(F) -> file:read_file(F).
