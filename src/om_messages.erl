-module(om_messages).
-compile(export_all).

% output of type errors
var(N,B)  -> try om_type:assertVar(N,B) of true -> true
                catch error:ERR -> io:format(
                    "~n ERROR: must be a declared variable.~n..............................~n Name ""~tp"" ~n..............................~n ",
                    [om:bin({var,{N,0}})]),
                    erlang:error(ERR) end.
func(E,T) -> try om_type:assertFunc(T) of true -> true
                catch error:ERR -> io:format(
                    "~n ERROR: must be a function.~n..............................~n Expression ~tp ~n Expression type ~tp ~n..............................~n ",
                    [om:bin(E), om:bin(T)]),
                    erlang:error(ERR) end.
star(E,T) -> try om_type:star(T) of N -> N
                catch error:ERR -> io:format(
                    "~n ERROR: must be a type.~n..............................~n Expression ~tp ~n Expression type ~tp ~n..............................~n ",
                    [om:bin(E), om:bin(T)]),
                    erlang:error(ERR) end.
eq(F,I,A,Q) -> try om:eq(I,Q) of true -> true
                catch error:ERR -> io:format(
                    "~n ERROR: wrong type of an argument.~n..............................~n Function ~tp ~n Function argument type ~tp ~n Argument ~tp ~n Argument type ~tp ~n..............................~n ",
                    [om:bin(F), om:bin(I), om:bin(A), om:bin(Q)]),
                    erlang:error(ERR) end.
