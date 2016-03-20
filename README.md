Om: Lambda Assembler
====================

[![Build Status](https://travis-ci.org/groupoid/om.svg?branch=master)](https://travis-ci.org/groupoid/om)

Abstract
--------

Om is intended to be used as a back-end library to facilitate 
implementation of general purpose typed functional languages.

Om is a low-level implementation of several Pure Type Systems - a class of typed 
lambda calculi with pluggable features such as polymorphism, dependent types, universes
and predicativity.  It includes a type checker, a compile time partial evaluator 
and a compiler into Erlang. Om can be used with a wide range of type systems 
which can be expressed with PTS triples, such as <b>System F<sub>ω<sub></b>, 
<b>System F<sub>&lt;:</sub></b> or <b>Calculus of Constructions</b>. 

Just like Morte, Om doesn't have built in recursive types. Instead, Exe language
adds a layer of syntactic sugar and internally encodes types such as lists using
F-algebras (so called impredicative Boehm-Berarducci encoding).

Many other traditional features missing from Om such as let, arrow types, 
pattern matching can be implemented purely as macros or syntactic sugar on top
of the Om core calculus.

Although Om supports arbitrary PTSes, our primary focus is a particular yet
unnamed PTS that can be seen as an extension of the classical non-inductive 
Calculus of Constructions with predicative universes. We develop a way to encode 
inductive types using categorical semantics instead of adding them to the core
language. A related works are Church, Parigot and Boehm-Berarducci encodings, 
although they are inherently impredicative so they cannot be used with 
the chosen type system.

This work is based on Henk lambda cube assembler, and the implementaton of its
predicative variant by Gabriel Gonzalez called Morte. Om has an option of full
compatibility with Morte, but also supports variations of the type system:
with or without countably many universes, and with or without predicativity.

Representation of Terms
-----------------------

Om intermediate representation is typical for a PTS:

```
    data Om: * :=
         (star: nat → Om)             -- universe constants
         (var: string → Om)           -- lambda abstractions
         (pi: string → Om →  Om)      -- pi types
         (arrow: string → Om → Om)    -- arrow types, a sugar for non-dependent cases of pi
         (app: Om → Om → Om)          -- applications
```

Concrete syntax and modularity
------------------------------

To simplify debugging, Om terms have concrete text representation, and there is
a minimalistic module system based on folders/files, just like in Morte.

Om provides construction of the IR term from folder tree (parsing and name resolution
for external references) as a utility. 

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
