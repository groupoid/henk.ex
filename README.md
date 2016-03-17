Om: Lambda Assembler
====================

[![Build Status](https://travis-ci.org/groupoid/om.svg?branch=master)](https://travis-ci.org/groupoid/om)

Abstract
--------

Om library provides backbone CoC lambda assembler with predicative universes
as target language for general purpose languages or macrosystems, possibly with
dependent types. This work is based on Henk lambda cube assembler, and impredicative
Morte implementation by Gabriel Gonzalez. Om is intended to be a compatible version
of Morte and supports two typecheckers: predicative and impredicative.
Om is useful as an intermediate language for high level front-end languages
with <b>System F<sub>ω<sub></b>, <b>System F<sub>&lt;:</sub></b> or pure <b>CoC</b> type systems.

Types
-----

Om AST provides very few types: only universe constants, lambda abstractions, pi types, arrow types and applications.

```
    data Om: * :=
         (star: nat → Om)
         (var: string → Om)
         (pi: string → Om →  Om)
         (arrow: string → Om → Om)
         (app: Om → Om → Om)
```

Just like Morte, Om doesn't have built in recursive types. Instead, Exe language
adds a layer of syntactic sugar and internally encodes types such as lists using
F-algebras (so called Boehm-Berarducci encoding).

The richness of type system makes type inference impossible, so type information
is mandatory as in type systems a la Church, and not mere annotations. Also, as 
Om is designed as an intermediate language for machines, partial inference/elaboration 
is not planned in Om but may be supported by higher level languages built on top of it.

Users
-----

wget install:

```sh
$ brew install erlang
$ wget https://github.com/groupoid/om/releases/download/0.3/om
$ chmod +x om
$ ./om
```

Developers
----------

```
$ git clone git://github.com/groupoid/om && cd om
$ make
$ ./mad sh
```

Session example with Om:

```erlang
> om:all().
...
> om:scan().
...
> om:modes().
["erased","girard","hurkens","normal","setoids"]
> om:mode("normal").
> om:extract().
>
Active: module loaded: {reloaded,'List'}
Active: module loaded: {reloaded,'Maybe'}
Active: module loaded: {reloaded,'Nat'}
Active: module loaded: {reloaded,'Vector'}
> ch:main().
Zero: 0
Cons/Nil: [2,1]
Test Big List: [2,3,5,8,11,19]
Two: 2
ok
Pack/Unpack 1 000 000 Inductive List: {714483,'_'}
Pack/Unpack 1 000 000 Inductive Nat: {743755,1000000}
```

Exe Language
------------

   General purpose functional language with lambdas on types, recursive algebraic types,
   higher order functions and embedded process calculus with corecursion. This language will be called
   Exe and dedicated to be high level general purpose functional programming language frontend to small core
   of dependent type system without recursion called Om. This language indended to be useful
   enough to encode our KVS, N2O and BPE applications.

Om Intermediate Language
------------------------

   An intermediate Om language is based on Henk languages described first
   by Erik Meyer and Simon Peyton Jones in 1997. Later on in 2015 a new impementation of the ideas
   in Haskell appeared. It used Boem-Berrarducci encoding of recursive data types into non-recursive terms.
   Morte has constants, variables, and kinds, is based only on *pi*, *lambda* and *apply* constructions,
   one axiom and four deduction rules. The Om language resembles Henk and Morte both in design
   and in implementation. This language indended to be small, concise, easily provable, clean and be able
   to produce verifiable programs that can be distributed over the networks and compiled at target with
   safe linkage.

   The Om Systax is the following:

```
   <> ::= #option

    I ::= #identifier

    U ::= * < #number >

    O ::= U | I | ( O ) | O O | λ ( I : O ) → O
                              | ∀ ( I : O ) → O
```

## Parser Term Specification
     
This information is subject to change.

### Result AST

```erlang
     {  star,          Universe  }  -- universe
     { {"λ",{Name,I}}, {In,Out}  }  -- lambda
     { {"∀",{Name,I}}, {In,Out}  }  -- pi
     {  "→",           {In,Out}  }  -- anonymous pi
     {  var,           {Name,I}  }  -- var
     {  app,           {Fun,Arg} }  -- app
```

### Intermediate AST

```erlang
     {  open     }
     {  close    }
     {  arrow    }
     {  colon    }
     {  lambda   }
     {  pi       }
     {  var,      {Name,Depth} }
     {  app,      {Fun,Arg}    }
     {  typevar,  {Name,Depth} }    -- argument name,
                                       transforms to lambda or pi
                                       during rewind
```

## Commands

### mode

Set the environment folder:

```erlang
> om:mode("normal").
ok
```

### a

Inline some terms:

```erlang
> om:a("\\(x:*)->\\(y:#List/id)->y").
{{"λ",{x,0}},
 {{star,1},
  {{"λ",{y,0}},
   {{{"λ",{'X',0}},
     {{star,1},
      {{"λ",{'Cons',0}},
       {{star,1},{{"λ",{'Nil',0}},{{star,1},{var,{'X',0}}}}}}}},
    {var,{y,0}}}}}}
```

### term

Check how term inlining and loading works:

```erlang
 > om:a("#List/map") == om:term("List/map").
 true
```

### show

Use internal functions:

```erlang
> om:show("priv/normal/List/@").

===[ File: priv/normal/List/@ ]==========
Cat: λ(a : *) → ∀(List : *) → ∀(Cons : ∀(head : a) → ∀(tail : List) → List) → ∀(Nil : List) → List
Term: 279
{{"λ",{a,0}},
 {{star,1},
  {{"∀",{'List',0}},
   {{star,1},
    {{"∀",{'Cons',0}},
     {{{"∀",{head,0}},
       {{var,{a,0}},
        {{"∀",{tail,0}},{{var,{'List',0}},{var,{'List',0}}}}}},
      {{"∀",{'Nil',0}},{{var,{'List',0}},{var,{'List',0}}}}}}}}}}
```

### parse

Parse raw expressions:

```erlang
> om:parse("∀ (a: *) → λ (b: * → * → *) → λ (c: * → a) → (((b (c a)) a) a))").
{[],
 [{{"∀",{a,0}},
   {{star,1},
    {{"λ",{b,0}},
     {{"→",{{star,1},{"→",{{star,1},{star,1}}}}},
      {{"λ",{c,0}},
       {{"→",{{star,1},{var,{a,0}}}},
        {app,{{app,{{app,{{var,{b,0}},{app,{{var,...},{...}}}}},
                    {var,{a,0}}}},
              {var,{a,0}}}}}}}}}}]}
```

### extract

Extract Erlang Modules:

```erlang
> om:extract("priv/normal/List").
ok
Active: module loaded: {reloaded,'List'}
> om:mode("normal").
> om:extract().
ok
Active: module loaded: {reloaded,'Bool'}
Active: module loaded: {reloaded,'List'}
```

### main

Sandbox for testing from bare Erlang.
Example of usage of compiled modules `List` and `Nat`:

```erlang
> ch:main().
Zero: 0
Cons/Nil: [2,1]
Test Big List: [2,3,5,8,11,19]
Two: 2
ok
Pack/Unpack 1 000 000 Inductive List: {733256,'_'}
Pack/Unpack 1 000 000 Inductive Nat: {748433,1000000}
```

`foldl` version of `stdlib` (for comparison):

```erlang
> timer:tc(lists,foldl,[fun(X,A) -> A end,0,lists:seq(1,1000000)]).
{735410,0}
```

### type

Typechecking:

```
> om:type("DEP.AND.PR-L-test").
** exception error: ["∀",
                     {app,{{{"λ",{'B',0}},
                            {{{"∀",{"_",0}},{{var,{'A',0}},{star,1}}},
                             {{"∀",{'AND',0}},
                              {{star,1},
                               {{"∀",{pair,0}},
                                {{{"∀",{a,0}},
                                  {{var,{'A',0}},
                                   {{"∀",{b,0}},
                                    {{app,{{var,{'B',...}},{var,{...}}}},{var,{'AND',0}}}}}},
                                 {var,{'AND',0}}}}}}}},
                           {var,{'B',0}}}}]
```

### scan

Scan modules in current mode:

```erlang
> om:mode("erased").
ok
> om:scan().
PASSED
[{true,"priv/erased/Bool/False"},
 {true,"priv/erased/Bool/True"},
 {true,"priv/erased/Bool/id"},
 {true,"priv/erased/List/Cons"},
 {true,"priv/erased/List/Nil"},
 {true,"priv/erased/List/id"},
 {true,"priv/erased/Nat/Succ"},
 {true,"priv/erased/Nat/Zero"},
 {true,"priv/erased/Nat/id"},
 {true,"priv/erased/Prod/Mk"},
 {true,"priv/erased/Prod/id"},
 {true,"priv/erased/Prod/pr1"},
 {true,"priv/erased/Prod/pr2"},
 {true,"priv/erased/Ret/Error"},
 {true,"priv/erased/Ret/Io"},
 {true,"priv/erased/Ret/Ok"},
 {true,"priv/erased/Ret/id"}]
```

Target Erlang VM and LLVM platforms
-----------------------------------

   This release only supports Erlang as the target platform.
   Erlang version is expected to be useful both on LING and BEAM Erlang virtual machines.
   The desired implementation should translate Om directly to Erlang AST.

Publications
------------

* Compilation from OM to Erlang and LLVM
* Om Intermediate Language
* Categorical semantic of general purpose functional language that compiles to Om
* Categorical encoding of inductive constructions to CoC
* Exe Processing Language

OM A HUM

