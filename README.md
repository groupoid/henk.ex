Om: Lambda Assembler
====================

Abstract
--------

Om library provides backbone CoC lambda assembler with predicative universes
as target language for general purpose languages or macrosystems, possibly with
dependent types. This work is based on lambda cube assembler Henk, and impredicative
Morte implementation by Gabriel Gonzalez. Om is intended to be a compatible version
of Morte and supports two typecheckers: predicative and impredicative.
Om is useful as an intermediate language for high level front-end languages
with <b>System F<sub>ω<sub></b>, <b>System F<sub>&lt;:</sub></b> or pure <b>CoC</b> type systems.

Types
-----

Om AST provides very little types, among them only constants, variables, applications, lambda and pi types.
Here is description in Exe language.

```
    data Om: * :=
         (star: nat → Om)
         (var: string → Om)
         (app: Om → Om → Om)
         (arrow: string → Om → Om)
         (pi: string → Om →  Om)
```

As defined in Morte, Om doesn't support recursive types. To see how you can encode List
using F-algebras please refer to Exe macrosystem over Om. Also Om doesn't support type inference,
so you should anotate aforehand all the Types in order to produce correct Om programs.
Om just checks the given terms in its own language.

Run Samples
-----------

* `brew install erlang`
* `wget https://raw.githubusercontent.com/synrc/mad/master/mad && chmod +x mad`
*  `mad dep com plan sh`


Exe Langauge
------------

   General purpose functional language with lambdas on types, recursive algebraic types,
   higher order functions and embedded process calculus with corecursion. This language will be called
   Exe and dedicated to be high level general purpose functional programming language frontend to small core
   of dependent type system without recursion called Exe. This language indended to be useful
   enough to encode KVS, N2O and BPE applications.

Om Intermediate Language
------------------------

   An intermediate Om language is based on Henk languages described first
   by Erik Meyer and Simon Peyton Jones in 1997. Later on in 2015 Morte impementation
   of Henk design appeared in Haskell, using Boem-Berrarducci encoding of non-recursive lamda terms.
   It has constants, variables, and kinds, is based only on *pi*, *lambda* and *apply* constructions,
   one axiom and four deduction rules. The design of Om language resembles Henk and Morte both in design
   and in implementation. This language indended to be small, concise, easily provable, clean and be able
   to produce verifiable programs that can be distributed over the networks and compiled at target with
   safe linkage.

   The Om Systax is the following:

```
     I := #identifier
     O := ∅ | ( O ) |
          □ | ∀ ( I : O ) → O |
          * | λ ( I : O ) → O |
          I | O → O | O O
```

Set the environment folder:

```erlang
> application:set_env(om,mode,"normal").
ok
```

Check how term inlining and loading works:

```
 > om:a("#List/map") == om:type("List/map").
 true
```

Inline some terms:

```
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

Use internal functions:

```
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

Parse raw expressions:

```
> om_parse:expr("",om:str("",<<"∀ (a: *) → λ (b: * → * → *) → λ (c: * → a) → (((b (c a)) a) a))"/utf8>>),[]).
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

Extract Erlang Modules:

```
> application:set_env(om,mode,"erased").
ok
> om_extract:scan().
Active: module loaded: {loaded_new,'Bool'}
Active: module loaded: {loaded_new,'List'}
Active: module loaded: {loaded_new,'Nat'}
Active: module loaded: {loaded_new,'Prod'}
Active: module loaded: {loaded_new,'Ret'}
```

Example of usage of compiled modules `List` and `Nat`:

```
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

```
> timer:tc(lists,foldl,[fun(X,A) -> A end,0,lists:seq(1,1000000)]).
{735410,0}
```

Target Erlang VM and LLVM platforms
-----------------------------------

   This works expect to compile to limited target platforms. For now Erlang is awaiting.
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

