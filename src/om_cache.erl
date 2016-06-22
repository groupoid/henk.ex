-module(om_cache).
-compile(export_all).

% TODO implement caches as a global mutable state
% TODO add error handling (exceptions? monads? whatever?)
% TODO when to flush caches?

% abstract caches
caches() -> [src, term, type, erased, extracted].

% cache ops, low-level, TODO
put(Cache,Key,Data) -> {}, Data. % store into cache
get(Cache,Key) -> {}. % {some,Data} or {none,"why"}
loader(Cache,Key) -> {}. % call the loader func stored
register_loader(Cache,Func) -> {}. % store a new loader func

% cache ops, high-level
has(Cache,Key) -> case get(Cache,Key) of
        {none,_} -> false; {some,_} -> true end.
load(Cache,Key) -> case get(Cache,Key) of
        {none,_} -> put(Key,loader(Cache,Key)); {some,S} -> S end.

% actual doers
make_src(Key) -> om:file(om:name({},[],Key)).
make_term_by_src(Bin) -> om:snd(om:parse(om:tokens(Bin))).
make_type_by_term(Ast) -> om:type(Ast).
make_erased_by_term(Ast) -> om:erase(Ast).
make_extracted(ModuleName) -> om:extract(ModuleName). % TODO ??? how to call extractor

% register loaders
register() ->
    register_loader(src, fun(Key) -> make_src(Key) end),
    register_loader(term, fun(Key) -> make_term_by_src(load(src,Key)) end),
    register_loader(type, fun(Key) -> make_type_by_term(load(term,Key)) end),
    register_loader(erased, fun(Key) -> make_erased_by_term(load(term,Key)) end),
    register_loader(extracted, fun(MN) -> make_extracted(MN) end).
