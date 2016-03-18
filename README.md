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
