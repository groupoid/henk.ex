
string() = list(char) = string/0.
int = integer().
at = atom().
array(T) = list(list(T)).
cube(T) = list(array(T)).
typeCons(A,B,C) = product(A,B,list(C)).
typeCons = product(any,any,list/1).
main = fun/2.
product/3 = product(any,any,any).
mylist = product(list(product/3),sum/2,any).
lst = list/1.
functorArg = type/1.

listmap = fun((Fun::fun(A::X,B::Y),In::list(X))->list(Y)) ->
    io:format("In: ~p",[In]),
    Out = [ Fun(E) || E <- In],
    io:format("Out: ~p",[Out]),
    Out end.

fmap = fun(fun(any,any),list/1).
map(A,B) = fun(fun(A,B),list(A),list(B)).
unimap(A,B,T::type/1) = fun(fun(A,B),T(A),T(B)).
tree(A) = sum(product(A),product(tree(A),tree(A))) = {A} | {tree(A),tree(A)}.
Functor = cat(Type::type/1) -> fmap = fun(fun(A,B),Type(A),Type(B)). end.
Listfunctor = Functor(list/1) -> fmap(F,X) -> listmap(F,X). end.
Square = fun(X) -> X * X end.
Listfunctor:fmap(Square,[1,2,3,4]).
TreeFunctor = Functor(tree/1) ->
    fmap(F,{X}) -> { F(X) };
    fmap(F,{L,R}) -> { fmap(F,{L}), fmap(F,{R}) }. end.

program = cat ->
    a = product/2 -> {string,integer}.
    a = product(string,integer).
    a = {string,integer}.
    lst(Type) = type/1 -> {Type,lst(Type)} | none.
    lst = type(Type) -> sum(product(Type,lst(Type)),none).
    strlst = type/0 -> lst(string).
    b = list(string).
    join = fun(A::a,B::b) -> lists:join(element(1,A),hd(B),program:join(A,B)) end.
    program(EP::program,List::type/1) :: cat
        b = list(string).
        join = fun((A::a,B::b) -> string()) -> v2:join(A,B) end.
    end.
    functor = cat(Type::type/1) ->
        fmap = fun(fun(A::x,B::y),Type(x),Type(y)).
    end.
    listFunctor = functor(list/1).
    atomFunctor = functor(product/1).
end.
