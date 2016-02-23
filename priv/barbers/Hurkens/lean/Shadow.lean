/- Shadow -/

set_option pp.universes true
set_option pp.metavar_args false
universe variable u

abbreviation Star := Type.{0}
abbreviation Box := Type.{1}

definition EquBox (El : Star) : Box := ∀(e1 e2 : El), Prop
definition PredBox (El : Star) : Box := ∀(e : El), Prop

section withEqu
    variables {El : Star} (Equ : EquBox El)
    definition Equ.ReflProp : Prop := ∀{e : El}, Equ e e
    definition Equ.TransProp : Prop := ∀{e1 e2 e3 : El}, Equ e1 e2 → Equ e2 e3 → Equ e1 e3
    definition Equ.SymProp : Prop := ∀{e1 e2 : El}, Equ e1 e2 → Equ e2 e1
end withEqu

record IsoTupleBox :=
    (AB : Star)
    (BA : Star)
    (ABOk : PredBox AB)
    (BAOk : PredBox BA)
    (AAOk : ∀(ab : AB), ∀(ba : BA), Prop)
    (BBOk : ∀(ab : AB), ∀(ba : BA), Prop)
check IsoTupleBox

record IsoTuple (box : IsoTupleBox) :=
    (isoAB : IsoTupleBox.AB box)
    (isoBA : IsoTupleBox.BA box)
    (isoABOk : IsoTupleBox.ABOk box isoAB)
    (isoBAOk : IsoTupleBox.BAOk box isoBA)
    (isoAAOk : IsoTupleBox.AAOk box isoAB isoBA)
    (isoBBOk : IsoTupleBox.BBOk box isoAB isoBA)
check IsoTuple

record IsoTupleMap (box1 box2 : IsoTupleBox) :=
    (onAB : IsoTupleBox.AB box1 → IsoTupleBox.AB box2)
    (onBA : IsoTupleBox.BA box1 → IsoTupleBox.BA box2)
    (onABOk : ∀(isoAB : IsoTupleBox.AB box1),
        IsoTupleBox.ABOk box1 isoAB → IsoTupleBox.ABOk box2 (onAB isoAB))
    (onBAOk : ∀(isoBA : IsoTupleBox.BA box1),
        IsoTupleBox.BAOk box1 isoBA → IsoTupleBox.BAOk box2 (onBA isoBA))
    (onAAOk : ∀(isoAB : IsoTupleBox.AB box1), ∀(isoBA : IsoTupleBox.BA box1),
        IsoTupleBox.AAOk box1 isoAB isoBA → IsoTupleBox.AAOk box2 (onAB isoAB) (onBA isoBA))
    (onBBOk : ∀(isoAB : IsoTupleBox.AB box1), ∀(isoBA : IsoTupleBox.BA box1),
        IsoTupleBox.BBOk box1 isoAB isoBA → IsoTupleBox.BBOk box2 (onAB isoAB) (onBA isoBA))
check IsoTupleMap

record IsoGraph :=
    (Ob : Box)
    (Iso : ∀(X Y : Ob), IsoTupleBox)
check IsoGraph

record IsoGraphMap (G1 G2 : IsoGraph) :=
    (onOb : IsoGraph.Ob G1 → IsoGraph.Ob G2)
    (onIso : ∀(X Y : IsoGraph.Ob G1), IsoTupleMap (IsoGraph.Iso G1 X Y) (IsoGraph.Iso G2 (onOb X) (onOb Y)))
check IsoGraphMap

--------------------------------------------
--
-- Setoids
--

record SetoidBox :=
    (El : Star)
    (ElOk : PredBox El)
    (Equ : EquBox El)
    (Refl : Equ.ReflProp Equ)
    (Trans : Equ.TransProp Equ)
    (Sym : Equ.SymProp Equ)
check SetoidBox

definition Setoid.Hom.El (A B : SetoidBox) : Star :=
    ∀(a : SetoidBox.El A), SetoidBox.El B
definition Setoid.Hom.ElOk (A B : SetoidBox) (map : Setoid.Hom.El A B) : Prop := and
    ( ∀(a : SetoidBox.El A), ∀(aok : SetoidBox.ElOk A a), SetoidBox.ElOk B (map a) )
    ( ∀{a1 a2 : SetoidBox.El A}, ∀(eq : SetoidBox.Equ A a1 a2), (SetoidBox.Equ B (map a1) (map a2)) )
definition Setoid.Hom.Equ (A B : SetoidBox) : EquBox (Setoid.Hom.El A B) :=
    λ f1 f2, ∀(a : SetoidBox.El A), SetoidBox.Equ B (f1 a) (f2 a)
definition Setoid.Hom.Refl (A B : SetoidBox)  : Equ.ReflProp (Setoid.Hom.Equ A B) :=
    λ f1, λ a, @SetoidBox.Refl B (f1 a)
definition Setoid.Hom.Trans (A B : SetoidBox)  : Equ.TransProp (Setoid.Hom.Equ A B) :=
    λ f1 f2 f3, λ eq12 eq23, λ a, @SetoidBox.Trans B (f1 a) (f2 a) (f3 a) (eq12 a) (eq23 a)
definition Setoid.Hom.Sym (A B : SetoidBox)  : Equ.SymProp (Setoid.Hom.Equ A B) :=
    λ f1 f2, λ eq12, λ a, @SetoidBox.Sym B (f1 a) (f2 a) (eq12 a)

definition Setoid.HomSetoid (A B : SetoidBox) : SetoidBox :=
    SetoidBox.mk
        (@Setoid.Hom.El A B)
        (@Setoid.Hom.ElOk A B)
        (@Setoid.Hom.Equ A B)
        (@Setoid.Hom.Refl A B)
        (@Setoid.Hom.Trans A B)
        (@Setoid.Hom.Sym A B)

definition Setoid.IsoAB.El (A B : SetoidBox) : Star :=
    Setoid.Hom.El A B
definition Setoid.IsoBA.El (A B : SetoidBox) : Star :=
    Setoid.Hom.El B A
definition Setoid.IsoAB.ElOk (A B : SetoidBox) (isoAB : Setoid.IsoAB.El A B) : Prop :=
    Setoid.Hom.ElOk A B isoAB
definition Setoid.IsoBA.ElOk (A B : SetoidBox) (isoBA : Setoid.IsoBA.El A B) : Prop :=
    Setoid.Hom.ElOk B A isoBA
definition Setoid.IsoAA.ElOk (A B : SetoidBox) (isoAB : Setoid.IsoAB.El A B) (isoBA : Setoid.IsoBA.El A B) : Prop :=
    ∀(a : SetoidBox.El A), SetoidBox.Equ A a (isoBA (isoAB (a)))
definition Setoid.IsoBB.ElOk (A B : SetoidBox) (isoAB : Setoid.IsoAB.El A B) (isoBA : Setoid.IsoBA.El A B) : Prop :=
    ∀(b : SetoidBox.El B), SetoidBox.Equ B b (isoAB (isoBA (b)))

definition Setoid.Iso (A B : SetoidBox) : IsoTupleBox :=
    IsoTupleBox.mk
        (@Setoid.IsoAB.El A B)
        (@Setoid.IsoBA.El A B)
        (@Setoid.IsoAB.ElOk A B)
        (@Setoid.IsoBA.ElOk A B)
        (@Setoid.IsoAA.ElOk A B)
        (@Setoid.IsoBB.ElOk A B)
definition Setoids : IsoGraph :=
    IsoGraph.mk SetoidBox Setoid.Iso

--------------------------------------------
--
-- Setoids with selected point (algebras with one 0-ary operation)
--

record PointedSetoidBox extends SetoidBox :=
    (Point: El)
    (PointOk : ElOk Point)
check PointedSetoidBox

definition PointedSetoid.Hom.El (A B : PointedSetoidBox) : Star :=
    Setoid.Hom.El A B
definition PointedSetoid.Hom.ElOk (A B : PointedSetoidBox) (map : PointedSetoid.Hom.El A B) : Prop := and
    (Setoid.Hom.ElOk A B map)
    (SetoidBox.Equ B (map (PointedSetoidBox.Point A)) (PointedSetoidBox.Point B))
definition PointedSetoid.Hom.Equ (A B : PointedSetoidBox) : EquBox (PointedSetoid.Hom.El A B) :=
    Setoid.Hom.Equ A B
definition PointedSetoid.Hom.Refl (A B : PointedSetoidBox)  : Equ.ReflProp (PointedSetoid.Hom.Equ A B) :=
    @Setoid.Hom.Refl A B
definition PointedSetoid.Hom.Trans (A B : PointedSetoidBox)  : Equ.TransProp (PointedSetoid.Hom.Equ A B) :=
    @Setoid.Hom.Trans A B
definition PointedSetoid.Hom.Sym (A B : PointedSetoidBox)  : Equ.SymProp (PointedSetoid.Hom.Equ A B) :=
    @Setoid.Hom.Sym A B
definition PointedSetoid.Hom.Point (A B : PointedSetoidBox)  : PointedSetoid.Hom.El A B :=
    λ (a : SetoidBox.El A), PointedSetoidBox.Point B
definition PointedSetoid.Hom.PointOk (A B : PointedSetoidBox)  : PointedSetoid.Hom.ElOk A B (PointedSetoid.Hom.Point A B) :=
    and.intro sorry sorry

definition PointedSetoid.HomSetoid (A B : PointedSetoidBox) : SetoidBox :=
    PointedSetoidBox.mk
        (@PointedSetoid.Hom.El A B)
        (@PointedSetoid.Hom.ElOk A B)
        (@PointedSetoid.Hom.Equ A B)
        (@PointedSetoid.Hom.Refl A B)
        (@PointedSetoid.Hom.Trans A B)
        (@PointedSetoid.Hom.Sym A B)
        (@PointedSetoid.Hom.Point A B)
        (@PointedSetoid.Hom.PointOk A B)

definition PointedSetoid.IsoAB.El (A B : PointedSetoidBox) : Star :=
    PointedSetoid.Hom.El A B
definition PointedSetoid.IsoBA.El (A B : PointedSetoidBox) : Star :=
    PointedSetoid.Hom.El B A
definition PointedSetoid.IsoAB.ElOk (A B : PointedSetoidBox) (isoAB : PointedSetoid.IsoAB.El A B) : Prop :=
    PointedSetoid.Hom.ElOk A B isoAB
definition PointedSetoid.IsoBA.ElOk (A B : PointedSetoidBox) (isoBA : PointedSetoid.IsoBA.El A B) : Prop :=
    PointedSetoid.Hom.ElOk B A isoBA
definition PointedSetoid.IsoAA.ElOk (A B : PointedSetoidBox) (isoAB : PointedSetoid.IsoAB.El A B) (isoBA : PointedSetoid.IsoBA.El A B) : Prop :=
    ∀(a : SetoidBox.El A), SetoidBox.Equ A a (isoBA (isoAB (a)))
definition PointedSetoid.IsoBB.ElOk (A B : PointedSetoidBox) (isoAB : PointedSetoid.IsoAB.El A B) (isoBA : PointedSetoid.IsoBA.El A B) : Prop :=
    ∀(b : SetoidBox.El B), SetoidBox.Equ B b (isoAB (isoBA (b)))

definition PointedSetoid.Iso (A B : PointedSetoidBox) : IsoTupleBox :=
    IsoTupleBox.mk
        (@PointedSetoid.IsoAB.El A B)
        (@PointedSetoid.IsoBA.El A B)
        (@PointedSetoid.IsoAB.ElOk A B)
        (@PointedSetoid.IsoBA.ElOk A B)
        (@PointedSetoid.IsoAA.ElOk A B)
        (@PointedSetoid.IsoBB.ElOk A B)
definition PointedSetoids : IsoGraph :=
    IsoGraph.mk PointedSetoidBox PointedSetoid.Iso

--------------------------------------------

definition ForgetPoint : IsoGraphMap PointedSetoids Setoids := sorry

--------------------------------------------
--
-- setoid of polymorphic functions
--

definition PolyFun.El (Gr : IsoGraph) (S : SetoidBox) : Star :=
    ∀(A : IsoGraph.Ob Gr), SetoidBox.El S
definition PolyFun.ElOkOk (Gr : IsoGraph) (S : SetoidBox) (poly : PolyFun.El Gr S) : Prop :=
    ∀(A : IsoGraph.Ob Gr), SetoidBox.ElOk S (poly A)
definition PolyFun.ElOkIso (Gr : IsoGraph) (S : SetoidBox) (poly : PolyFun.El Gr S) : Prop :=
    ∀(A B : IsoGraph.Ob Gr), ∀(iso : IsoTuple (IsoGraph.Iso Gr A B)),
        SetoidBox.Equ S (poly A) (poly B)
definition PolyFun.ElOk (Gr : IsoGraph) (S : SetoidBox) (poly : PolyFun.El Gr S) : Prop :=
    and (PolyFun.ElOkOk Gr S poly) (PolyFun.ElOkIso Gr S poly)
definition PolyFun.ElOk.getOk (Gr : IsoGraph) (S : SetoidBox) (poly : PolyFun.El Gr S)
    : PolyFun.ElOk Gr S poly → PolyFun.ElOkOk Gr S poly := and.left
definition PolyFun.ElOk.getIso (Gr : IsoGraph) (S : SetoidBox) (poly : PolyFun.El Gr S)
    : PolyFun.ElOk Gr S poly → PolyFun.ElOkIso Gr S poly := and.right
definition PolyFun.Equ (Gr : IsoGraph) (S : SetoidBox) : EquBox (PolyFun.El Gr S) :=
    λ(polyX polyY),
    ∀(A : IsoGraph.Ob Gr), SetoidBox.Equ S (polyX A) (polyY A)
definition PolyFun.Refl (Gr : IsoGraph) (S : SetoidBox) : Equ.ReflProp (PolyFun.Equ Gr S) :=
    λ poly1, λ A, @SetoidBox.Refl S (poly1 A)
definition PolyFun.Trans (Gr : IsoGraph) (S : SetoidBox) : Equ.TransProp (PolyFun.Equ Gr S) :=
    λ poly1 poly2 poly3, λ eq12 eq23, λ A,
        @SetoidBox.Trans S (poly1 A) (poly2 A) (poly3 A) (eq12 A) (eq23 A)
definition PolyFun.Sym (Gr : IsoGraph) (S : SetoidBox) : Equ.SymProp (PolyFun.Equ Gr S) :=
    λ poly1 poly2, λ eq12, λ A,
        @SetoidBox.Sym S (poly1 A) (poly2 A) (eq12 A)
definition PolyFun (Gr : IsoGraph) (S : SetoidBox) : SetoidBox :=
    SetoidBox.mk
        (@PolyFun.El Gr S)
        (@PolyFun.ElOk Gr S)
        (@PolyFun.Equ Gr S)
        (@PolyFun.Refl Gr S)
        (@PolyFun.Trans Gr S)
        (@PolyFun.Sym Gr S)

definition Shadow.El (Gr : IsoGraph) : Star :=
    ∀(S : SetoidBox), ∀(Mk : PolyFun.El Gr S), ∀(MkOk : PolyFun.ElOk Gr S Mk),
        SetoidBox.El S
definition Shadow.ElOkOk (Gr : IsoGraph) (sh : Shadow.El Gr) : Prop :=
    ∀(S : SetoidBox), ∀(Mk : PolyFun.El Gr S), ∀(MkOk : PolyFun.ElOk Gr S Mk),
        SetoidBox.ElOk S (sh S Mk MkOk)
definition Shadow.ElOkLim (Gr : IsoGraph) (sh : Shadow.El Gr) : Prop :=
    ∀(X : SetoidBox), ∀(XMk : PolyFun.El Gr X), ∀(XMkOk : PolyFun.ElOk Gr X XMk),
    ∀(Y : SetoidBox), ∀(YMk : PolyFun.El Gr Y), ∀(YMkOk : PolyFun.ElOk Gr Y YMk),
    ∀(mor : Setoid.Hom.El X Y), ∀(morOk : Setoid.Hom.ElOk X Y mor),
    ∀(morLim : ∀(Q : IsoGraph.Ob Gr), SetoidBox.Equ Y (mor (XMk Q)) (YMk Q)),
        SetoidBox.Equ Y (mor (sh X XMk XMkOk)) (sh Y YMk YMkOk)
definition Shadow.ElOk (Gr : IsoGraph) (sh : Shadow.El Gr) : Prop :=
    and (Shadow.ElOkOk Gr sh) (Shadow.ElOkLim Gr sh)
definition Shadow.ElOk.getOk (Gr : IsoGraph) (sh : Shadow.El Gr)
    : Shadow.ElOk Gr sh → Shadow.ElOkOk Gr sh := and.left
definition Shadow.ElOk.getLim (Gr : IsoGraph) (sh : Shadow.El Gr)
    : Shadow.ElOk Gr sh → Shadow.ElOkLim Gr sh := and.right
definition Shadow.Equ (Gr : IsoGraph) : EquBox (Shadow.El Gr) :=
    λ(shA shB),
    ∀(S : SetoidBox), ∀(Mk : PolyFun.El Gr S), ∀(MkOk : PolyFun.ElOk Gr S Mk),
        SetoidBox.Equ S (shA S Mk MkOk) (shB S Mk MkOk)
definition Shadow.Refl (Gr : IsoGraph) : Equ.ReflProp (Shadow.Equ Gr) :=
    λ sh1, λ S Mk MkOk, @SetoidBox.Refl S
        (sh1 S Mk MkOk)
definition Shadow.Trans (Gr : IsoGraph) : Equ.TransProp (Shadow.Equ Gr) :=
    λ sh1 sh2 sh3, λ eq12 eq23, λ S Mk MkOk, @SetoidBox.Trans S
        (sh1 S Mk MkOk) (sh2 S Mk MkOk) (sh3 S Mk MkOk) (eq12 S Mk MkOk) (eq23 S Mk MkOk)
definition Shadow.Sym (Gr : IsoGraph) : Equ.SymProp (Shadow.Equ Gr) :=
    λ sh1 sh2, λ eq12, λ S Mk MkOk, @SetoidBox.Sym S
        (sh1 S Mk MkOk) (sh2 S Mk MkOk) (eq12 S Mk MkOk)

definition ShadowSetoid (Gr : IsoGraph) : SetoidBox :=
    SetoidBox.mk
        (@Shadow.El Gr)
        (@Shadow.ElOk Gr)
        (@Shadow.Equ Gr)
        (@Shadow.Refl Gr)
        (@Shadow.Trans Gr)
        (@Shadow.Sym Gr)

definition Shadow.Mk.El (Gr : IsoGraph) : PolyFun.El Gr (ShadowSetoid Gr) :=
    λ Arg, λ S Mk MkOk, Mk Arg
definition Shadow.Mk.ElOkOkOk (Gr : IsoGraph) (Arg : IsoGraph.Ob Gr) : Shadow.ElOkOk Gr (Shadow.Mk.El Gr Arg) :=
    λ S Mk MkOk, (PolyFun.ElOk.getOk Gr S Mk MkOk) Arg
definition Shadow.Mk.ElOkOkLim (Gr : IsoGraph) (Arg : IsoGraph.Ob Gr) : Shadow.ElOkLim Gr (Shadow.Mk.El Gr Arg) :=
    λ(X : SetoidBox), λ(XMk : PolyFun.El Gr X), λ(XMkOk : PolyFun.ElOk Gr X XMk),
    λ(Y : SetoidBox), λ(YMk : PolyFun.El Gr Y), λ(YMkOk : PolyFun.ElOk Gr Y YMk),
    λ(mor : Setoid.Hom.El X Y), λ(morOk : Setoid.Hom.ElOk X Y mor),
    λ(morLim : ∀(Q : IsoGraph.Ob Gr), SetoidBox.Equ Y (mor (XMk Q)) (YMk Q)),
        morLim Arg
definition Shadow.Mk.ElOkOk (Gr : IsoGraph) : PolyFun.ElOkOk Gr (ShadowSetoid Gr) (Shadow.Mk.El Gr) :=
    λ Arg, and.intro (Shadow.Mk.ElOkOkOk Gr Arg) (Shadow.Mk.ElOkOkLim Gr Arg)
definition Shadow.Mk.ElOkIso (Gr : IsoGraph) : PolyFun.ElOkIso Gr (ShadowSetoid Gr) (Shadow.Mk.El Gr) :=
    λ(A B : IsoGraph.Ob Gr), λ(iso : IsoTuple (IsoGraph.Iso Gr A B)),
    λ(S : SetoidBox), λ(Mk : PolyFun.El Gr S), λ(MkOk : PolyFun.ElOk Gr S Mk),
        (PolyFun.ElOk.getIso Gr S Mk MkOk) A B iso
definition Shadow.Mk.ElOk (Gr : IsoGraph) : PolyFun.ElOk Gr (ShadowSetoid Gr) (Shadow.Mk.El Gr) :=
    and.intro (Shadow.Mk.ElOkOk Gr) (Shadow.Mk.ElOkIso Gr)

------------------------------------------------------------------

definition backPoly.El
    {Gr1 Gr2 : IsoGraph} (map : IsoGraphMap Gr1 Gr2)
    (S : SetoidBox)
    : PolyFun.El Gr2 S → PolyFun.El Gr1 S
    := sorry
definition backPoly.ElOk
    {Gr1 Gr2 : IsoGraph} (map : IsoGraphMap Gr1 Gr2)
    (S : SetoidBox) (Mk : PolyFun.El Gr2 S)
    : PolyFun.ElOk Gr2 S Mk → PolyFun.ElOk Gr1 S (backPoly.El map S Mk)
    := sorry

definition forwardShadow.El
    {Gr1 Gr2 : IsoGraph} (map : IsoGraphMap Gr1 Gr2)
    : Shadow.El Gr1 → Shadow.El Gr2
    :=
        λ(psh : Shadow.El Gr1),
        λ(S : SetoidBox), λ(Mk : PolyFun.El Gr2 S), λ(MkOk : PolyFun.ElOk Gr2 S Mk),
            psh S (backPoly.El map S Mk) (backPoly.ElOk map S Mk MkOk)

definition TheFibration.onEl
    : Shadow.El PointedSetoids → Shadow.El Setoids
    := forwardShadow.El ForgetPoint
definition TheFibration.onElOk
    : ∀(sh : Shadow.El PointedSetoids),
        Shadow.ElOk PointedSetoids sh → Shadow.ElOk Setoids (TheFibration.onEl sh)
    := sorry
definition TheFibration.onEqu
    : ∀(sh1 sh2 : Shadow.El PointedSetoids),
        Shadow.Equ PointedSetoids sh1 sh2 → Shadow.Equ Setoids (TheFibration.onEl sh1) (TheFibration.onEl sh2)
    := sorry
-----

-- TODO: Shadow in initial object
-- TODO: isomorphism: Shadow.Mk & TheFiber are inverse
-- TODO: Hurken's paradox
