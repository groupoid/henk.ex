-module(om).
-description('CoC Compiler').
-behaviour(supervisor).
-include("om.hrl").
-behaviour(application).
-export([init/1, start/2, stop/1]).
-compile(export_all).

% env

privdir()    -> application:get_env(om,priv,"priv").
mode(S)      -> application:set_env(om,mode,S).
mode()       -> application:get_env(om,mode,"normal").
debug(S)     -> application:set_env(om,debug,atom(S)).
debug()      -> application:get_env(om,debug,false).

% providing functions

help(_)      -> help().
help()       -> om_help:help().
pwd(_)       -> mad_repl:cwd().
print(X)     -> io:format("~ts~n",[bin(X)]).
bin(X)       -> unicode:characters_to_binary(om:flat(om_parse:print(X,0))).
extract()    -> om_extract:scan().
extract(X)   -> om_extract:extract(X).
type(S)      -> om_type:type(S).
erase(X)     -> om_erase:erase(X).
type(S,B)    -> om_type:type(S,B).
modes(_)     -> modes().
modes()      -> ["girard","hurkens","normal","setoids"].
priv(Mode)   -> lists:concat([privdir(),"/",Mode]).
name(M,[],F) -> string:join([priv(mode()),F],"/");
name(M,P,F)  -> string:join([priv(mode()),P,F],"/").
str(F)       -> om_tok:tokens([],unicode:characters_to_binary(F),0,{1,[]},[]).
read(F)      -> om_tok:tokens([],file(F),0,{1,[]},[]).
comp(F)      -> rev(tokens(F,"/")).
cname(F)     -> hd(comp(F)).
tname(F)     -> tname(F,[]).
tname(F,S)   -> X = hd(tl(comp(F))), case om:mode() of X -> []; _ -> X ++ S end.
show(F)      -> Term = snd(parse(tname(F),cname(F))), mad:info("~n~ts~n~n", [bin(Term)]), Term.
a(F)         -> snd(parse(str(F))).
fst({X,_})   -> X.
snd({error,X}) -> {error,X};
snd({_,[X]}) -> X;
snd({_,X})   -> X.
parse(X)     -> om_parse:expr2([],X,[],{0,0}).
parse(T,C)   -> om_parse:expr2(T,read(name(mode(),T,C)),[],{0,0}).

% system functions

unicode()    -> io:setopts(standard_io, [{encoding, unicode}]).
main(A)      -> unicode(), case A of [] -> mad:main(["sh"]); A -> console(A) end.
start()      -> start(normal,[]).
start(_,_)   -> unicode(), mad:info("~tp~n",[om:ver()]), supervisor:start_link({local,om},om,[]).
stop(_)      -> ok.
init([])     -> mode("normal"), {ok, {{one_for_one, 5, 10}, []}}.
ver(_)       -> ver().
ver()        -> {version,[keyget(I,element(2,application:get_all_key(om)))||I<-[description,vsn]]}.
console(S)   -> mad_repl:load(), put(ret,0),
                Fold = lists:foldr(fun(I,O) ->
                      R = rev(I),
                      Res = lists:foldl(fun(X,A) -> om:(atom(X))(A) end,hd(R),tl(R)),
                      io:format("~tp~n",[Res]),
                      [get(ret)|O]
                      end, [], string:tokens(S,[","])),
                halt(lists:sum(Fold)).

% test suite

typed(X)     -> try Y = om:type(X),  {Y,[]} catch E:R -> {X,typed}  end.
erased(X)    -> try A = om:erase(X), {A,[]} catch E:R -> {X,erased} end.
parsed(F)    -> case parse(tname(F),cname(F)) of {_,[X]} -> {X,[]}; _ -> {F,parsed} end.
pipe(L)      -> lists:foldl(fun(X,{A,D}) -> {N,E}=?MODULE:X(A), {N,[E|D]} end,{L,[]},[parsed,typed]).
pass(0)      -> "PASSED";
pass(X)      -> "FAILED " ++ integer_to_list(X).
all(_)       -> all().
all()        -> om:debug(false), lists:flatten([ begin om:mode(M), om:scan() end || M <- modes() ]).
syscard()    -> [ {F} || F <- filelib:wildcard(name(mode(),"**","*")), filelib:is_dir(F) /= true ].
wildcard()   -> lists:flatten([ {A} || {A,B} <- ets:tab2list(filesystem),
                lists:sublist(A,length(om:priv(mode()))) == om:priv(mode()) ]).
scan(_)      -> scan().
scan()       -> om:debug(false),
                Res = [ { flat(element(2,pipe(F))),lists:concat([tname(F,"/"),cname(F)])}
                     || {F} <- lists:umerge(wildcard(),syscard()) ],
                Passed = lists:foldl(fun({X,_},B) -> case X of [] -> B; _ -> B + 1 end end, 0, Res),
                {mode(),pass(Passed),Res}.
test(_)      -> All = om:all(),
                io:format("~tp~n",[om_parse:test()]),
                io:format("~tp~n",[All]),
                case lists:all(fun({Mode,Status,Tests}) -> Status /= om:pass(false) end, All) of
                     true  -> 0;
                     false -> put(ret,1),1 end .

% relying functions

rev(X)       -> lists:reverse(X).
flat(X)      -> lists:flatten(X).
tokens(X,Y)  -> string:tokens(X,Y).
debug(S,A)   -> case om:debug() of true -> io:format(S,A); false -> ok end.
atom(X)      -> list_to_atom(cat([X])).
cat(X)       -> lists:concat(X).
keyget(X,Y)  -> proplists:get_value(X,Y).

file(F) -> case file:read_file(F) of
                {ok,Bin} -> Bin;
                {error,_} -> mad(F) end.

mad(F)  -> case mad_repl:load_file(F) of
                {ok,Bin} -> Bin;
                {error,_} -> <<>> end.
