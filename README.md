Henk: Pure Type System
----------------------

<img src="https://henk.groupoid.space/img/Henk%20Barendregt.jpg">

**Henk** languages described first by Erik Meijer and Simon Peyton Jones in 1997.
Later on in 2015 a new implementation of the ideas in Haskell appeared, called Morte.
It used the Böhm-Berarducci encoding of recursive data types into non-recursive terms.
Morte has constants, variables, and kinds, is based only on **П**, **λ** and **apply** constructions,
one axiom and four deduction rules. The Henk language resembles Henk design and Morte implementation.
This language is indended to be small, concise, easily provable, clean and
be able to produce verifiable programs that can be distributed over the networks and compiled at target with safe linkage.

```
$ curl -fsSL https://raw.github.com/synrc/mad/master/mad > mad
$ chmod +x mad"
$ ./mad dep com pla bun henk

```

The Henk Syntax is the following:

```
   <> ::= #option

    I ::= #identifier

    U ::= * < #number >

    O ::= U | I | ( O ) | O O
            | λ ( I : O ) → O
            | ∀ ( I : O ) → O
```

Henk is an implementation of PTS with an Infinite Number of Universes, the pure lambda calculus with dependent types.
It can be compiled (code extraction) to bytecode of Erlang virtual machines BEAM and LING.

Trusted PTS with an Infinite Universes
--------------------------------------

In repository `henk` you may found following parts of core:

* [Parser](https://github.com/groupoid/om/blob/master/src/om_parse.erl)
* [Typechecker](https://github.com/groupoid/om/blob/master/src/om_type.erl)
* [Eraser](https://github.com/groupoid/om/blob/master/src/om_erase.erl)
* [Code Extractor](https://github.com/groupoid/om/blob/master/src/om_extract.erl)

PTS ships with different "modes" (spaces of types with own encodings), or "preludes", which
you may find in `priv` directory. They are selectable with `om:mode("normal")`.

#### [normal](https://github.com/groupoid/henk/tree/main/lib/normal)

This is a minimal practical prelude similar to Morte's base library of Gabriella Gonzalez.
It contains common inductive constructions encoded using plain Church (or Böhm-Berarducci if you wish) encoding,
and two basic (co)monadic effect systems: IO (free monad, for finite I/O) and IOI (free comonad,
for infinitary I/O, long-term processes). The generated code is being sewed with
Erlang effects that are passed as parameters to pure functions.

Note: all these folders (modules) are encoded in plain CoC in Henk repository to demonstrate
you the basic principles how things work. Later all these should be written in EXE
languages and translated to OM automatically. You may think of OM as the low-level
typed assembler of type theory.

Articles
--------

* <a href="https://henk.groupoid.space/doc/pts.pdf">Henk: Pure Type System for Erlang</a> [Sokhatskyi]
* <a href="https://henk.groupoid.space/doc/pts_ua.pdf">Система доведення теорем з однiєю аксiомою</a> [Sokhatskyi]

Credits
-------

* <a itemprop="sameAs" content="https://orcid.org/0000-0001-7127-8796" href="https://orcid.org/0000-0001-7127-8796" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;white-space: nowrap;">Maksym Sokhatskyi <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png"> 🇺🇦</a>

OM A HUM
