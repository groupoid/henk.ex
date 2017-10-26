Om Hacking
==========

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
>  om_extract:extract("priv/normal").
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

### parse, str

Wrong Typecheck Example:

```erlang
4> om:type(om:snd(om:parse(om:str("∀ (a: *) → λ (b: * → * → *) → λ (c: * → a) → (((b (c a)) a) a))")))).
** exception error: no match of right hand side value {error,{"==",{star,1},{var,{a,0}}}}
     in function  om_type:type/2 (/Users/maxim/depot/groupoid/om/src/om_type.erl, line 70)
     in call from om_type:type/2 (/Users/maxim/depot/groupoid/om/src/om_type.erl, line 66)
     in call from om_type:type/2 (/Users/maxim/depot/groupoid/om/src/om_type.erl, line 65)
     in call from om_type:type/2 (/Users/maxim/depot/groupoid/om/src/om_type.erl, line 62)
```

### extract

Extract Erlang Modules:

```erlang
> om:extract("priv/normal/List").
ok
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

```erlang
> om:type(om:a("#IO/[>>=]")).
{{"∀",{a,0}},
 {{star,1},
  {{"∀",{b,0}},
   {{star,1},
    {{"∀",{m,0}},
     {{{"∀",{'IO',0}},
       {{star,1},
        {{"∀",{'GetLine_',0}},
         {{{"∀",{'_',0}},
           {{{"∀",{'_',0}},
             {{{"∀",{'List',0}},{{star,1},{{...},...}}},{var,{'IO',0}}}},
            {var,{'IO',0}}}},
          {{"∀",{'PutLine_',0}},
           {{{"∀",{'_',0}},
             {{{"∀",{'List',...}},{{star,...},{...}}},
              {{"∀",{...}},{{...},...}}}},
            {{"∀",{'Pure_',0}},
             {{{"∀",{...}},{{...},...}},{var,{...}}}}}}}}}},
      {{"∀",{f,0}},
       {{{"∀",{'_',0}},
         {{var,{a,0}},
          {{"∀",{'IO',0}},
           {{star,1},
            {{"∀",{'GetLine_',0}},
             {{{"∀",{...}},{{...},...}},{{[...],...},{...}}}}}}}},
        {{"∀",{'IO',0}},
         {{star,1},
          {{"∀",{'GetLine_',0}},
           {{{"∀",{'_',0}},{{{"∀",{...}},{{...},...}},{var,{...}}}},
            {{"∀",{'PutLine_',0}},
             {{{[...],...},{...}},{{...},...}}}}}}}}}}}}}}}
```

### scan

Scan modules in current mode:

```erlang
> om:mode("normal").
ok
> om:scan().
["priv/normal/id"]
{"normal","PASSED",
 [{[],"priv/normal/Bool/@"},
  {[],"priv/normal/Bool/False"},
  {[],"priv/normal/Bool/True"},
  {[],"priv/normal/Bool/[&&]"},
  {[],"priv/normal/Bool/[||]"},
  {[],"priv/normal/Bool/and"},
  {[],"priv/normal/Bool/eif"},
  {[],"priv/normal/Bool/eiff"},
  {[],"priv/normal/Bool/if"},
  {[],"priv/normal/Bool/iff"},
  {[],"priv/normal/Bool/induction_on.type"},
  {[],"priv/normal/Bool/not"},
  {[],"priv/normal/Bool/or"},
  {[],"priv/normal/Cmd/@"},
  {[],"priv/normal/Cmd/Bind"},
  {[],"priv/normal/Cmd/Monad"},
  {[],"priv/normal/Cmd/Monad.old"},
  {[],"priv/normal/Cmd/Pure"},
  {[],"priv/normal/Cmd/[>>=]"},
  {[],"priv/normal/Cmd/embed"},
  {[],"priv/normal/Cmd/lift"},
  {[],"priv/normal/Cmd/map"},
  {[],"priv/normal/Cmd/sequence_"},
  {[],[...]},
  {[],...},
  {...}|...]}
```
