Om: Lambda Assembler
====================

Maxim Sokhatsky maxim@synrc.com

![Om](http://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/Georg_Simon_Ohm3.jpg/200px-Georg_Simon_Ohm3.jpg)

Georg Simon Ohm -- a German physicist and mathematician who was born in German town Erlangen.

Abstract
--------

This library is created to provide backbone lambda assembler as target language for
general purpose languages, possibly with dependent types. This work is based on lambda
cube assembler Henk, and Morte implementation by Gabriel Gonzalez. Om is indended
to be compatible version of Morte. Om is useful as an intermediate language for
high level front-end languages with <b>System F<sub>ω<sub></b>, <b>System F<sub>&lt;:</sub></b> or
<b>CoC</b> type systems.

Types
-----

Om AST provides very little types, among them only constants, variables, applications, lambda and pi types

```
          data Expr a = Const Const
                      | Var   Var
                      | Lam   Text    (Expr a) (Expr a)
                      | Pi    Text    (Expr a) (Expr a)
                      | App  (Expr a) (Expr a)
```

As defined in Morte, Om doesn't support recursive types, to see how you can encode List
using F-algebras please refer to Morte.Turtorial. Also Om doesn't support type infering,
so you should anotate aforehand all the Types in order to produce correct Om programs.
Om just check the given terms in its own language.

Publications
------------

* Compilation from OM to Erlang and LLVM
* Om Intermediate Language
* Categorical semantic of general purpose functional language that compiles to Om
* Exe Processing Language

OM A HUM

