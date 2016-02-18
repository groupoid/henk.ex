Om: Lambda Assembler
====================

Maxim Sokhatsky maxim@synrc.com

![Om](http://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/Georg_Simon_Ohm3.jpg/200px-Georg_Simon_Ohm3.jpg)

Georg Simon Ohm -- a German physicist and mathematician who was born in German town Erlangen.

Abstract
--------

This library is created to provide backbone lambda assembler as target language for
general purpose languages, possibly with dependent types. This work is based on lambda
cube assembler Henk, and Morte implementation by Gabriel Gonzalez. Om is intended
to be a compatible version of Morte. Om is useful as an intermediate language for
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

As defined in Morte, Om doesn't support recursive types. To see how you can encode List
using F-algebras please refer to Morte.Turtorial. Also Om doesn't support type inference,
so you should anotate aforehand all the Types in order to produce correct Om programs.
Om just checks the given terms in its own language.

Run Samples
-----------

* `brew install erlang`
* `wget https://github.com/synrc/mad/blob/master/mad`
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
    EXPR :=                   EXPR             EXPR
          | "λ" "(" LABEL ":" EXPR ")" "arrow" EXPR
          | "∀" "(" LABEL ":" EXPR ")" "arrow" EXPR
          |                   EXPR     "arrow" EXPR
          |         LABEL
          | "*"
          | "[]"
          |     "("           EXPR ")"
```

```erlang
 > om:a("#List/map") == om:type("List/map").
 true

> om:a("\\(x:*)->\\(y:#List/map)->y").
{lambda,{{arg,x},
         {const,star},
         {lambda,{{arg,y},
                  {lambda,{{arg,a},
                           {const,star},
                           {lambda,{{arg,b},
                                    {const,star},
                                    {lambda,{{arg,f},...

> om:show("priv/List/@").

=INFO REPORT==== 22-Dec-2015::10:01:10 ===
"priv/List/@"
  λ (a: *)
→ ∀ (List: *)
→ ∀ (Cons:
    ∀ (head: a)
  → ∀ (tail: List)
  → List)
→ ∀ (Nil: List)
→ List

{[],
 [{lambda,{{arg,a},
           {const,star},
           {pi,{{arg,'List'},
                {const,star},
                {pi,{{arg,'Cons'},
                     {pi,{{arg,head},
                          {var,{a,0}},
                          {pi,{{arg,tail},
                               {var,{'List',0}},
                               {var,{'List',0}}}}}},
                     {pi,{{arg,'Nil'},
                          {var,{'List',0}},
                          {var,{'List',0}}}}}}}}}}]}

> om:parse(<<"∀ (a: *) → λ (b: * → * → *) → λ (c: * → a) → (((b (c a)) a) a))"/utf8>>).
{[],
 [{pi,
      {{arg,"a"},
       {const,star},
       {lambda,
           {{arg,"b"},
            {arrow,
                {{const,star},{arrow,{{const,star},{const,star}}}}},
            {lambda,
                {{arg,"c"},
                 {arrow,{{const,star},{var,"a"}}},
                 {app,
                     {{app,
                          {{app,
                               {{var,"b"},
                                {app,{{var,"c"},{var,"a"}}}}},
                           {var,"a"}}},
                      {var,"a"}}}}}}}}}]}
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
* Exe Processing Language

OM A HUM

