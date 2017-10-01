-module(om_cache).
-compile(export_all).

ets(Tab, Key, [])    -> ets:delete(Tab,Key);
ets(Tab, Key, Value) -> ets:insert(Tab,{Key,Value}), Value.
ets(Tab, Key) ->
    Res = ets:lookup(Tab,Key),
    Val = case Res of [] -> []; [Value] -> Value; Values -> Values end,
    case Val of [] -> [];
                {_,X} -> X end.

cache(type,N,D)   -> case ets(type,N)   of [] -> ets(type,N,om_type:type(om_parse:ret(om:parse([],N)),D)); T -> T end;
cache(norm,N,_)   -> case ets(norm,N)   of [] -> ets(norm,N,om_type:norm(om_parse:ret(om:parse([],N)))); T -> T end;
cache(erased,N,D) -> case ets(erased,N) of [] -> ets(erased,N,om_erase:erase(om_parse:ret(om:parse([],N)),D)); T -> T end.
