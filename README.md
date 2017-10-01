Om — Lambda Assembler
---------------------

[![Build Status](https://travis-ci.org/groupoid/om.svg?branch=master)](https://travis-ci.org/groupoid/om)
[![Gitter Chat](https://img.shields.io/gitter/room/badges/shields.svg)](https://gitter.im/groupoid/om)

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

OM is an implementation of PTS with Infinite Number of Universes,
the pure lambda calculus with dependent types. It can be compiled (code extraction) to bytecode
of Erlang virtual machines BEAM and LING.

OM — Trusted PTS with Infinite Universes
----------------------------------------

In repository OM you may found following parts of core:

* [Parser](https://github.com/groupoid/om/blob/master/src/om_parse.erl)
* [Typechecker](https://github.com/groupoid/om/blob/master/src/om_type.erl)
* [Eraser](https://github.com/groupoid/om/blob/master/src/om_erase.erl)
* [Code Extractor](https://github.com/groupoid/om/blob/master/src/om_extract.erl)

OM ships with different `modes` (spaces of types with own encodings), or `preludes`
you may find in `priv` directory. They are selectable with `om:mode("normal")`.

#### [normal](https://github.com/groupoid/om/tree/master/priv/normal)

This is minimal practical prelude similar to Morte's base library of Gabriel Gonzalez.
For modeling inductive constructions we use here plain Church (or Boehm-Berrarducci if you wish),
we have here basic I/O monads: IO (free monad, for limited I/O) and IOI (free comonad,
for infinitary I/O, long-term processes). The generated code is being sewed with
Erlang effects that are passed as parameters to pure functions.

#### [setoids](https://github.com/groupoid/om/tree/master/priv/setoids)

This is an implementation of Setoid structure, that provides us Equality. However
we switched lately to more compact `poset` encoding.

#### [posets](https://github.com/groupoid/om/tree/master/priv/posets)

This is implementation of non-reflexive partial order sets which
has more compact representation than setoids for our needs.
It has only `Bool`, `Empty` and `Unit` encoded just to show the general idea.
Dependent eliminator of `Bool` you can found
here [Data/Bool](https://github.com/groupoid/om/tree/master/priv/posets/Data/Bool/)

###

Note: all these folders (modules) are encoded in plain CoC in OM repository to demonstrate
you the basic principles how things work. Later all these should be written in EXE languages and translated to OM
automatically. You may think of OM as the low-level typed assembler of type theory.

Credits
-------

* Paul Lyutko
* Maxim Sokhatsky
* Andy Melnikov

OM A HUM
