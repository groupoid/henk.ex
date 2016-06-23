-module(om_cache).
-compile(export_all).

% TODO implement caches as a global mutable state
% TODO add error handling (exceptions? monads? whatever?)
% TODO when to flush caches?

% abstract caches identified by atoms
caches() -> [ src, % source binary content of files
              term, % AST of parsed terms
              normal, % AST of normalized terms
              type, % AST of (normalized) types of terms
              erased, % AST of erased terms
              extracted]. % for extracted erlang code

% cache ops, low-level, TODO
cache_put(Cache,Key,Data) -> put({termcache,Cache,Key},Data),Data.
%    whereis(termcache) ! {self(),{put,Cache,Key,Data}},
%    receive {_,{res,D}} -> D end.
cache_get(Cache,Key) -> case get({termcache,Cache,Key}) of
    undefined -> {none,{}}; X -> {some, X} end.
%    whereis(termcache) ! {self(),{get,Cache,Key}},
%    receive {_,{M,D}} -> {M,D} end.

% cache ops, high-level
has(Cache,Key) -> case cache_get(Cache,Key) of {none,_} -> false; {some,_} -> true end.
load(Cache,Key) -> case cache_get(Cache,Key) of
        {none,_} -> cache_put(Cache,Key,loader(Cache,Key)); {some,S} -> S end.

% to call other modules, to be called by load/2
loader(src, Key) -> om:file(om:name({},[],Key));
loader(term, Key) -> om:snd(om:parse(om:tokens(load(src,Key))));
loader(normal, Key) -> om:normalize(load(term,Key));
loader(type, Key) -> om:type(load(term,Key));
loader(erased, Key) -> om:erase(load(term,Key));
loader(extracted, MN) -> om:extract(MN).

% actor (???)
cache_actor(D) -> receive
    {P,{put,Cache,Key,Data}} ->
        P ! {self(),{res,Data}}, cache_actor([{{Cache,Key},Data}|D]);
    {P,{get,Cache,Key}} -> case proplists:is_defined({Cache,Key},D) of
        true -> P ! {self(), {some, proplists:get_value({Cache,Key},D)}};
        false -> P ! {self(), {none, []}} end, cache_actor(D)
    end.
start() -> P = spawn(?MODULE, cache_actor, [[]]), register(termcache, P).
