Om: Erlang System F-Omega
=========================

Maxim Sokhatsky maxim@synrc.com

![Om](http://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/Georg_Simon_Ohm3.jpg/200px-Georg_Simon_Ohm3.jpg)

Georg Simon Ohm -- a German physicist and mathematician who was born in german town Erlangen. 

Abstract
--------

Here is very early proposal to include some type system which is partially
compatible with Dializer syntax on top of Erlang. We are going to implement
Typed Erlang Compiler that will produce regular BEAM files.

The purpose of this document is to discuss Typed Erlang syntax and runtime
implementation details. Internally all types data are going to be represented as
native erlang complex types: tuples and lists.

The core of type system are proposed to be classic second-order
polymorphic lambda calculus.

Types
-----

All morphisms, constructable domains and categories itself are treated as types.
Categories which are Modules are denoted as 'cat'. Morphisms which are
Function are denoted as 'fun'. Algebraic data types are denoted
as 'product' and 'sum'. Here is syntax of type definitions:

    fun(A,B,...) -> A -> B -> ...
    product(A,...) -> {A,...}
    sum(A,B,...) -> A | B | ...
    cat(A,B,...) -> Domains(A,B,...), Morphisms(A,B,...), ... end.

where A, B, ... are type constructors.

Type Constructors
-----------------

Type constructors are being parametrized by types or other type constructors.
Type constructors are Λ type-level functions and are distinguishable
from λ value-level functions.

    type/Arity = product/Arity | sum/Arity | fun/Arity | cat/Arity | list/1 | any .

Note than list/1 is special type constructor only for built-in Erlang list types.
All type constructors with zero arity denoted as concrete types. It has 
compatiblity with Erlang dializer syntax, e.g.:

    string() = list(char) = string/0.
    integer().
    atom().

Here is type constructors depended only from concrete types.

    tree(A) = sum(product(A),product(tree(A),tree(A))) = {A} | {tree(A),tree(A)}.
    array(T) = list(list(T)).
    cube(T) = list(array(T)).
    typeCons(A,B,C) = product(A,B,list(C)).

Here is type constructors which are partially constructed with not concrete types:

    typeCons = product(any,any,list/1).
    main = fun/2.
    product/3 = product(any,any,any).
    mylist = product(list(product/3),sum/2,any).
    lst = list/1.
    functorArg = type/1.

Here is mix of dependance of concrete types and partially constructed:

    mixed(T,A) = product(A,any,sum(T,list(T)),product/3,fun/2,functor(list/1)).

Kinds Notion
------------

Type constructors 'cat', 'fun', 'sum' and 'product' belongs to type 'type' which
is also typeable by number of arguments and signature of type construdctor:

    Typed Erlang      System Fω
    ------------      ---------
    type(A)           * -> *
    type(A,B)         * -> * -> *
    type(A::type/2)   (* -> *) -> *

Here is example of Free Monads using kinds notions:

    functor = cat(Type::type/1) -> fmap = fun(fun(A,B),Type(A),Type(B)). end.
    other = fun((F::type(A))->lift(F,A)).
    pure = fun((A)->lift(type(A))).
    lift(F::type(A)) = sum(pure(A),other(F(A))).
    free(F::functor(A)) = sum(pure(A), free(F(free(F(A))))).

Functions
---------

Value-level function type signature are supposed to be compatible with dializer:

    fun((A,B,...)->C).

And also to have simpler form:

    fun(A,B,...,C).

Function arguments are parametrised by concrete types:

    listmap = fun((Fun::fun(A::X,B::Y),In::list(X))->list(Y)) ->
        io:format("In: ~p",[In]),
        Out = [ Fun(E) || E <- In],
        io:format("Out: ~p",[Out]),
        Out end.

However you can define function types using any type constructor.

    fmap = fun(fun(any,any),list/1).
    listmap = fun(fun(A,B),list(A),list(B)).
    unimap(A,B,T::type/1) = fun(fun(A,B),T(A),T(B)).

Historically Erlang uses identifiers started with capital latter for variables
ans started with small letter for types -- reversed to Haskell. So if in left
side from matching operator = is small latter started indentifier i.e. type,
if capital -- variable.

    a = tree(integer()).
    B = {1}.

Exeption is functions which are treated as fun types with binded body values:

    b = tree(integer()) -> {1}.
    join = fun(A,B) -> lists:join(A,B) end.

Modules
-------

Modules are paramerized by type constructors, which form local programs.

    tree(A) = sum(product(A),product(tree(A),tree(A))) = {A} | {tree(A),tree(A)}.
    Functor = cat(Type::type/1) -> fmap = fun(fun(A,B),Type(A),Type(B)). end.
    Listfunctor = Functor(list/1) -> fmap(F,X) -> listmap(F,X). end.
    Square = fun(X) -> X * X end.
    Listfunctor:fmap(Square,[1,2,3,4]).

So here we have fully typed version of list map.
Let us look on more Erlangish example:

    TreeFunctor = Functor(tree/1) ->
        fmap(F,{X}) -> { F(X) };
        fmap(F,{L,R}) -> { fmap(F,{L}), fmap(F,{R}) }. end.

This is cat instance that includes erlang code which turns typed.

Example
-------

    program = cat ->
        a = product/2 -> {string,integer}.
        a = product(string,integer).
        a = {string,integer}.
        lst(Type) = type/1 -> {Type,lst(Type)} | none.
        lst = type(Type) -> sum(product(Type,lst(Type)),none).
        strlst = type/0 -> lst(string).
        b = list(string).
        Join = fun(A::a,B::b) -> lists:join(element(1,A),hd(B),program:join(A,B)) end.
        Program = cat(EP::program,List::type/1) ->
            b = list(string).
            join = fun((A::a,B::b) -> string()) -> v2:join(A,B) end.
        end.
        functor = cat(Type::type/1) ->
            fmap = fun(fun(A::x,B::y),Type(x),Type(y)).
        end.
        listFunctor = functor(list/1).
        atomFunctor = functor(product/1).
    end.

