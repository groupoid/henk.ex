-module(om).
-description('Om Intermediate Compiler').
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1]).
-compile(export_all).

% > om:a("#List/map") == om:type("List/map").
% true
% > om:a("\\(x:*)->\\(y:#List/map)->y").
% {lambda,{{arg,x},
%          {const,star},
%          {lambda,{{arg,y},
%                   {lambda,{{arg,a},
%                            {const,star},
%                            {lambda,{{arg,b},
%                                     {const,star},
%                                     {lambda,{{arg,f},...
% > om:type("List/@").
% {lambda,{{arg,a},
%         {const,star},
%         {pi,{{arg,'List'},
%              {const,star},
%              {pi,{{arg,'Cons'},
%                   {pi,{{arg,head},
%                        {var,{a,0}},
%                        {pi,{{arg,tail},{var,{'List',0}},{var,{'List',0}}}}}},
%                   {pi,{{arg,'Nil'},{var,{'List',0}},{var,{'List',0}}}}}}}}}}

a(X)        -> {_,[R]}=om_parse:expr(flat([om:str(X)]),[]),R.
main(A)     -> mad:main(A).
start()     -> start(normal,[]).
start(_,_)  -> supervisor:start_link({local,om},om,[]).
stop(_)     -> ok.
init([])    -> scan(), {ok, {{one_for_one, 5, 10}, []}}.
type(F)     -> {_,[X]}=parse(lists:concat(["priv/",F])),X.
parse(F)    -> om_parse:expr(read(F),[]).
str(F)      -> om_tok:tokens(unicode:characters_to_binary(F),0,{1,[]},[]).
read(F)     -> om_tok:tokens(file(F),0,{1,[]},[]).
file(F)     -> {ok,Bin} = file:read_file(F), Bin.
scan()      -> [ show(F) || F <- filelib:wildcard("priv/**/*"), not filelib:is_dir(F) ], ok.
show(F)     -> {X,Y,Z} = {F,unicode:characters_to_binary(file(F)),parse(F)},
               error_logger:info_msg("~80p~n~ts~n~p~n",[X,Y,Z]),Z.

% om parser depends on three functions:

rev(X)      -> lists:reverse(X).
flat(X)     -> lists:flatten(X).
tokens(X,Y) -> string:tokens(X,Y).
