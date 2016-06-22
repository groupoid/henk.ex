-module(om_cache).
-compile(export_all).

% TODO implement caches as a global mutable state
% TODO add error handling (exceptions? monads? whatever?)
% TODO when to flush caches?

% abstract caches identified by atoms
caches() -> [ src, % source binary content of files
              term, % AST of parsed terms
              type, % AST of types of terms
              erased, % AST of erased terms
              extracted]. % for extracted erlang code

% cache ops, low-level, TODO
put(Cache,Key,Data) -> Data. % store into cache, returns Data stored
get(Cache,Key) -> {none, "not implemented"}. % {some,Data} or {none,"why"}

% cache ops, high-level
has(Cache,Key) -> case get(Cache,Key) of {none,_} -> false; {some,_} -> true end.
load(Cache,Key) -> case get(Cache,Key) of
        {none,_} -> put(Cache,Key,loader(Cache,Key)); {some,S} -> S end.

% to call other modules, to be called by load/2
loader(src, Key) -> om:file(om:name({},[],Key));
loader(term, Key) -> om:snd(om:parse(om:tokens(load(src,Key))));
loader(type, Key) -> om:type(load(term,Key));
loader(erased, Key) -> om:erase(load(term,Key));
loader(extracted, MN) -> om:extract(MN).
