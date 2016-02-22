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
