/- Shadow -/

set_option pp.universes true
set_option pp.metavar_args false
universe variable u

abbreviation Star := Type.{0}
abbreviation Box := Type.{1}

definition EquBox (El : Star) : Box := ∀(e1 e2 : El), Prop

section withEqu
    variables {El : Star} (Equ : EquBox El)
    definition Equ.ReflProp : Prop := ∀{e : El}, Equ e e
    definition Equ.TransProp : Prop := ∀{e1 e2 e3 : El}, Equ e1 e2 → Equ e2 e3 → Equ e1 e3
    definition Equ.SymProp : Prop := ∀{e1 e2 : El}, Equ e1 e2 → Equ e2 e1
end withEqu

record SetoidBox :=
    (El : Star)
    (ElOk : ∀(e : El), Prop)
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
definition Setoid.Hom.Equ (A B : SetoidBox) (f1 f2 : Setoid.Hom.El A B) : Prop :=
    ∀(a : SetoidBox.El A), SetoidBox.Equ B (f1 a) (f2 a)
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

definition Poly.Hom.El (S : SetoidBox) : Star :=
    ∀(A : SetoidBox), SetoidBox.El S
definition Poly.Hom.ElOkOk (S : SetoidBox) (poly : Poly.Hom.El S) : Prop :=
    ∀(A : SetoidBox), SetoidBox.ElOk S (poly A)
definition Poly.Hom.ElOkIso (S : SetoidBox) (poly : Poly.Hom.El S) : Prop :=
    ∀(A B : SetoidBox), ∀(isoAB : Setoid.IsoAB.El A B), ∀(isoBA : Setoid.IsoBA.El A B),
    ∀(isoABOk : Setoid.IsoAB.ElOk A B isoAB), ∀(isoBAOk : Setoid.IsoBA.ElOk A B isoBA),
    ∀(isoAAOk : Setoid.IsoAA.ElOk A B isoAB isoBA), ∀(isoBBOk : Setoid.IsoBB.ElOk A B isoAB isoBA),
    SetoidBox.Equ S (poly A) (poly B)
definition Poly.Hom.ElOk (S : SetoidBox) (poly : Poly.Hom.El S) : Prop :=
    and (Poly.Hom.ElOkOk S poly) (Poly.Hom.ElOkIso S poly)

definition Shadow.El : Star :=
    ∀(S : SetoidBox), ∀(Mk : Poly.Hom.El S), ∀(MkOk : Poly.Hom.ElOk S Mk),
        SetoidBox.El S
definition Shadow.ElOkOk (sh : Shadow.El) : Prop :=
    ∀(S : SetoidBox), ∀(Mk : Poly.Hom.El S), ∀(MkOk : Poly.Hom.ElOk S Mk),
        SetoidBox.ElOk S (sh S Mk MkOk)
definition Shadow.ElOkLim (sh : Shadow.El) : Prop :=
    ∀(X : SetoidBox), ∀(XMk : Poly.Hom.El X), ∀(XMkOk : Poly.Hom.ElOk X XMk),
    ∀(Y : SetoidBox), ∀(YMk : Poly.Hom.El Y), ∀(YMkOk : Poly.Hom.ElOk Y YMk),
    ∀(mor : Setoid.Hom.El X Y), ∀(morOk : Setoid.Hom.ElOk X Y mor), ∀(A : SetoidBox),
        SetoidBox.Equ Y (mor (XMk A)) (YMk A)
definition Shadow.ElOk (sh : Shadow.El) : Prop :=
    and (Shadow.ElOkOk sh) (Shadow.ElOkLim sh)
definition Shadow.Equ (shA shB : Shadow.El) : Prop :=
    ∀(S : SetoidBox), ∀(Mk : Poly.Hom.El S), ∀(MkOk : Poly.Hom.ElOk S Mk),
        SetoidBox.Equ S (shA S Mk MkOk) (shB S Mk MkOk)
definition Shadow.Refl : Equ.ReflProp Shadow.Equ :=
    λ sh1, λ S Mk MkOk, @SetoidBox.Refl S
        (sh1 S Mk MkOk)
definition Shadow.Trans : Equ.TransProp Shadow.Equ :=
    λ sh1 sh2 sh3, λ eq12 eq23, λ S Mk MkOk, @SetoidBox.Trans S
        (sh1 S Mk MkOk) (sh2 S Mk MkOk) (sh3 S Mk MkOk) (eq12 S Mk MkOk) (eq23 S Mk MkOk)
definition Shadow.Sym : Equ.SymProp Shadow.Equ :=
    λ sh1 sh2, λ eq12, λ S Mk MkOk, @SetoidBox.Sym S
        (sh1 S Mk MkOk) (sh2 S Mk MkOk) (eq12 S Mk MkOk)

definition ShadowSetoid : SetoidBox :=
    SetoidBox.mk
        @Shadow.El
        @Shadow.ElOk
        @Shadow.Equ
        @Shadow.Refl
        @Shadow.Trans
        @Shadow.Sym
