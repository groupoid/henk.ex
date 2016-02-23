-module(pi).
-export([parse_transform/2]).
-compile(export_all).

parse_transform(Forms, _Options) ->
    {_,_,_,Name} = hd(tl(Forms)),
    io:format("Name: ~p~n",[Name]),
    file:write_file(lists:concat([Name,".ast"]),list_to_binary(io_lib:format("~p",[Forms]))),
    compile:forms(Forms,[binary,export_all]),
    Forms.

main() ->
    io:format("Main: ~p~n",[0]).

