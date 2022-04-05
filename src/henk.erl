-module(henk).
-description('Henk: Pure Type System').
-behaviour(supervisor).
-include("om.hrl").
-behaviour(application).
-export([init/1, start/2, stop/1,main/1]).
-compile(export_all).

opt()        -> [ set, named_table, { keypos, 1 }, public ].
tables()     -> [ term, norm, type, erased ].
boot()       -> [ ets:new(T,opt()) || T <- tables() ].
unicode()    -> io:setopts(standard_io, [{encoding, unicode}]).
main(A)      -> unicode(), case A of [] -> mad:main(["sh"]); A -> console(A) end.
start()      -> start(normal,[]).
start(_,_)   -> unicode(), supervisor:start_link({local,henk},henk,[]).
ets_clear()  -> [ ets:delete(T) || T <- tables() ].
stop(_)      -> ets_clear(), ok.
init([])     -> boot(), om:mode("normal"), {ok, {{one_for_one, 5, 10}, []}}.
console(S)   -> boot(),
                mad_repl:load(), put(ret,0),
                Fold = lists:foldr(fun(I,O) ->
                      R = om:rev(I),
                      Res = lists:foldl(fun(X,A) -> om:(om:atom(X))(A) end,hd(R),tl(R)),
                      io:format("~tp~n",[Res]),
                      [get(ret)|O]
                      end, [], string:tokens(S,[","])),
                halt(lists:sum(Fold)).
