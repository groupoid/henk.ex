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
["posets","normal","setoids"]
> om:extract("normal").
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

## Extract IO Sample

```
> om:show("Morte/recursive").

((#IO/replicateM #Nat/Five) ((((#IO/[>>=] #IO/data) #Unit/@) #IO/getLine) #IO/putLine))

{app,{{app,{{remote,"IO/replicateM"},{remote,"Nat/Five"}}},
      {app,{{app,{{app,{{app,{{remote,"IO/[>>=]"},
                              {remote,"IO/data"}}},
                        {remote,"Unit/@"}}},
                  {remote,"IO/getLine"}}},
            {remote,"IO/putLine"}}}}}

> ch:ma().
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
{{"λ",{x,0}},
 {{star,1},{{"λ",{y,0}},{{remote,"List/pure"},{var,{y,0}}}}}}
```

### norm

Normalization expand remote terms:

```erlang
11> om:a("#List/map").
{remote,"List/map"}
12> om:norm(om:a("#List/map")).
{{"λ",{a,0}},
 {{star,1},
  {{"λ",{b,0}},
   {{star,1},
    {{"λ",{f,0}},
     {{{"∀",{'_',0}},{{var,{a,0}},{var,{b,0}}}},
      {{"λ",{xs,0}},
       {{{"∀",{'List',0}},
         {{star,1},
          {{"∀",{'Cons',0}},
           {{{"∀",{head,0}},{{var,{a,0}},{{"∀",{...}},{{...},...}}}},
            {{"∀",{'Nil',0}},{{var,{'List',...}},{var,{...}}}}}}}},
        {app,{{app,{{app,{{var,{xs,0}},{{"∀",{...}},{{...},...}}}},
                    {{"λ",{head,0}},{{var,{a,...}},{{[...],...},{...}}}}}},
              {{"λ",{'List',0}},
               {{star,1},
                {{"λ",{'Cons',0}},
                 {{{[...],...},{...}},{{...},...}}}}}}}}}}}}}}}
```

### show

Use internal functions:

```erlang
>  om:show("List/@").

( λ (A: *1)
→ ( ∀ (List: *1)
→ ( ∀ (Cons: ( ∀ (Head: A)
  → ( ∀ (Tail: (List
    → List)
)
  → List)))
→ ( ∀ (Nil: List)
→ List))))

{{"λ",{'A',0}},
 {{star,1},
  {{"∀",{'List',0}},
   {{star,1},
    {{"∀",{'Cons',0}},
     {{{"∀",{'Head',0}},
       {{var,{'A',0}},
        {{"∀",{'Tail',0}},
         {{"→",{{var,{'List',0}},{var,{'List',0}}}},
          {var,{'List',0}}}}}},
      {{"∀",{'Nil',0}},{{var,{'List',0}},{var,{'List',0}}}}}}}}}}
```

### parse, str

Wrong Typecheck Example:

```erlang
> A = om:str("∀ (a: *) → λ (b: * → * → *) → λ (c: * → a) → (((b (c a)) a) a))").
[pi,open,
 {var,{a,0}},
 colon,
 {star,1},
 close,arrow,lambda,open,
 {var,{b,0}},
 colon,
 {star,1},
 arrow,
 {star,1},
 arrow,
 {star,1},
 close,arrow,lambda,open,
 {var,{c,0}},
 colon,
 {star,1},
 arrow,
 {var,{a,0}},
 close,arrow,open,open|...]
> B = om:parse(A).
{{3,3},
 [{{"∀",{a,0}},
   {{star,1},
    {{"λ",{b,0}},
     {{"→",{{star,1},{"→",{{star,1},{star,1}}}}},
      {{"λ",{c,0}},
       {{"→",{{star,1},{var,{a,0}}}},
        {app,{{app,{{app,{{var,{b,0}},{app,{{var,...},{...}}}}},
                    {var,{a,0}}}},
              {var,{a,0}}}}}}}}}}]}
> om:type(om:snd(B)).
** exception error: no match of right hand side value {error,{"==",{star,1},{var,{a,0}}}}
```

### extract

Extract Erlang Modules:

```erlang
> om_extract:extract("priv/normal/List").
ok
Active: module loaded: {reloaded,'List'}
> om_extract:extract("priv/normal").
ok
>
Active: module loaded: {loaded_new,normal}
Active: module loaded: {loaded_new,'Prod'}
Active: module loaded: {loaded_new,'Cmd'}
Active: module loaded: {loaded_new,'Unit'}
Active: module loaded: {loaded_new,'Prop'}
Active: module loaded: {loaded_new,'Monoid'}
Active: module loaded: {loaded_new,'Nat'}
Active: module loaded: {loaded_new,'Path'}
Active: module loaded: {loaded_new,'IO'}
Active: module loaded: {loaded_new,'IOI'}
Active: module loaded: {loaded_new,'Maybe'}
Active: module loaded: {loaded_new,'Simple'}
Active: module loaded: {loaded_new,'Frege'}
Active: module loaded: {loaded_new,'Mon'}
Active: module loaded: {loaded_new,'Sigma'}
Active: module loaded: {loaded_new,'List'}
Active: module loaded: {loaded_new,'Bool'}
Active: module loaded: {loaded_new,'StrictPos'}
Active: module loaded: {loaded_new,'Morte'}
Active: module loaded: {loaded_new,'String'}
Active: module loaded: {loaded_new,'Monad'}
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
