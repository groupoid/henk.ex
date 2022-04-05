Henk Hacking
============

Developers
----------

```
$ git clone git://github.com/groupoid/henk && cd henk
$ curl -fsSL https://raw.github.com/synrc/mad/master/mad > mad
$ chmod +x mad"
$ ./mad dep com pla bun henk
$ ./henk
```

Session example with Henk:

```erlang
> om:all().
> om:scan().
> om:modes().
["posets","normal","setoids"]
> om:extract("normal").
> ch:main().
Zero: 0
Cons/Nil: [2,1]
Test Big List: [2,3,5,8,11,19]
Two: 2
ok
Pack/Unpack 1 000 000 Inductive List: {714483,'_'}
Pack/Unpack 1 000 000 Inductive Nat: {743755,1000000}
```

## Extract IO Sample

```
> om:rec().
> 12
: 12
> 2341414
: 2341414
> kjhfjkashdfk
: kjhfjkashdfk
> 13
: 13
> 132
: 132
#Fun<List.28.113171260>

> om:corec().
> 1
: 1
> 0
: 0
#Fun<List.3.113171260>

```

## Parser Term Specification

This information is subject to change.

### Result AST

```erlang
data pts
    = star             (n: nat)
    | var    (n: name) (n: nat)
    | remote (n: name) (n: nat)
    | app                       (f a: pts)
    | lambda (x: name)          (d c: pts)
    | arrow                     (d c: pts)
    | pi     (x: name)          (d c: pts)
```

## Commands

### mode

Set the environment folder:

```erlang
> om:modes().
["normal","setoids","posets"]
> om:mode("normal").
ok
```

### a

Inline some terms:

```erlang
> om:a("\\(x:*)->\\(y:#List/pure)->y").
```

### norm

Normalization expand remote terms:

```erlang
11> om:a("#List/map").
12> om:norm(om:a("#List/map")).
```

### show

Use internal functions:

```erlang
>  om:show("List/@").
```

### parse, str

Wrong Typecheck Example:

```erlang
> A = om:str("∀ (a: *) → λ (b: * → * → *) → λ (c: * → a) → (((b (c a)) a) a))").
> B = om:parse(A).
> om:type(om:snd(B)).
** exception error: no match of right hand side value {error,{"==",{star,1},{var,{a,0}}}}
```

### extract

Extract Erlang Modules:

```erlang
> om_extract:extract("priv/normal/List").
ok
> om_extract:extract("priv/normal").
ok
>
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

```erlang
> om:type(om:a("#IO/[>>=]")).
```

### scan

Scan modules in current mode:

```erlang
> om:mode("normal").
ok
> om:scan().
ok
```
