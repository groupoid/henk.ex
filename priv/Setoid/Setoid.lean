/- Setoid -/

-- equivalence
definition EquType (El : Type) : Type := El → El → Prop

-- axioms of equivalence
section withEqu
  variables {El : Type} (Equ : EquType El)
  definition ReflProp : Prop := ∀{e : El}, Equ e e
  definition TransProp : Prop := ∀{e1 e2 e3 : El}, Equ e1 e2 → Equ e2 e3 → Equ e1 e3
  definition SymProp : Prop := ∀{e1 e2 : El}, Equ e1 e2 → Equ e2 e1
end withEqu

-- objects of the category `Setoid`
record Setoid_Ob : Type :=
       (el: Type)
       (equ: EquType el)
       (refl: ReflProp equ)
       (trans: TransProp equ)
       (sym: SymProp equ)

-- carrier of setoid
notation `[` a `]` := Setoid_Ob.el a
notation a `≈⦉` S `⦊≈` b := Setoid_Ob.equ S a b

-- morphisms in the category `Setoid`
record Setoid_Hom_El (A B : Setoid_Ob) : Type :=
       (onel: [A] → [B])
       (onequ: ∀{a1 a2 : [A]}, (a1 ≈⦉ A ⦊≈ a2) → (onel a1 ≈⦉ B ⦊≈ onel a2))

-- action on carrier
infixl `$`:100 := Setoid_Hom_El.onel
infixl `$$`:100 := Setoid_Hom_El.onequ

definition Setoid_Hom (A B : Setoid_Ob) : Setoid_Ob := Setoid_Ob.mk 
           (Setoid_Hom_El A B) 
           (λ f g, ∀ (a : [A]), (f $ a) ≈⦉ B ⦊≈ (g $ a)) 
           (λ f , λ a, Setoid_Ob.refl B)
           (λ f g h, λ fg gh, λ a, Setoid_Ob.trans B (fg a) (gh a)) 
           (λ f g, λ fg, λ a, Setoid_Ob.sym B (fg a))

infixr `⟶`:10 := Setoid_Hom

-- carrier of category
definition HomType (Ob : Type) : Type := Ob → Ob → Setoid_Ob

-- structure of category
section withHom
  variables {Ob : Type} (Hom : HomType Ob)
  definition IdType : Type := Π{a : Ob}, [Hom a a]
  definition MulType : Type := Π{a b c : Ob}, [Hom b c ⟶  Hom a b ⟶  Hom a c]
end withHom

-- identity in category `Setoid`
definition Setoid_Id : IdType Setoid_Hom := 
           λ (A : Setoid_Ob), Setoid_Hom_El.mk (λ (a : [A]), a) (λ (a1 a2 : [A]), λ (e12 : a1 ≈⦉ A ⦊≈ a2), e12)

definition Setoid_Mul : MulType Setoid_Hom := 
  λ(A B C : Setoid_Ob), Setoid_Hom_El.mk
    (λ(f : [B ⟶  C]), Setoid_Hom_El.mk 
        (λ(g : [A ⟶  B]), Setoid_Hom_El.mk
          (λ(a : [A]), f $ (g $ a))
          (λ(a1 a2 : [A]), λ(a12 : a1 ≈⦉ A ⦊≈ a2), f $$ (g $$ a12)))
        (λ(g1 g2 : [A ⟶  B]), λ(g12 : g1 ≈⦉ A ⟶ B ⦊≈ g2), λ(a : [A]), f $$ (g12 a)))
    (λ(f1 f2 : [B ⟶  C]), λ(f12 : f1 ≈⦉ B ⟶  C ⦊≈ f2), λ(g : [A ⟶  B]), λ(a : [A]), f12 (g $ a))

-- axioms of category
section withIdMul
  variables {Ob : Type} {Hom : HomType Ob} (Id : IdType Hom) (Mul : MulType Hom)
  definition UnitLProp : Prop := ∀{a b : Ob}, ∀(f : [Hom a b]), (Mul $ Id $ f) ≈⦉ Hom a b ⦊≈ f
  definition UnitRProp : Prop := ∀{a b : Ob}, ∀(f : [Hom a b]), (Mul $ f $ Id) ≈⦉ Hom a b ⦊≈ f
  definition AssocProp : Prop := ∀{a b c d : Ob}, ∀(f : [Hom c d]), ∀(g : [Hom b c]), ∀(h : [Hom a b]),
                                  (Mul $ (Mul $ f $ g) $ h) ≈⦉ Hom a d ⦊≈ (Mul $ f $ (Mul $ g $ h))
end withIdMul

definition Setoid_UnitL : UnitLProp @Setoid_Id @Setoid_Mul := 
           λ(A B : Setoid_Ob), λ(f : [A ⟶ B]), λ(a : [A]), Setoid_Ob.refl B

definition Setoid_UnitR : UnitRProp @Setoid_Id @Setoid_Mul := 
           λ(A B : Setoid_Ob), λ(f : [A ⟶ B]), λ(a : [A]), Setoid_Ob.refl B

definition Setoid_Assoc : AssocProp @Setoid_Mul :=
           λ(A B C D : Setoid_Ob), λ(f : [C ⟶ D]), λ(g : [B ⟶ C]), λ(h : [A ⟶ B]), λ(a : [A]), Setoid_Ob.refl D

-- objects of 2-category `Cat`
record Cat_Ob : Type :=
       (ob: Type)
       (hom: HomType ob)
       (id: IdType hom)
       (mul: MulType hom)
       (unitl: UnitLProp @id @mul)
       (unitr: UnitRProp @id @mul)
       (assoc: AssocProp @mul)

-- `Setoid` as category
definition Setoid : Cat_Ob := Cat_Ob.mk Setoid_Ob Setoid_Hom
                              @Setoid_Id @Setoid_Mul @Setoid_UnitL @Setoid_UnitR @Setoid_Assoc

-- TODO: Sigma: (B->Type) -> (E->B), UnSigma: (E->B) -> (B->Type)
-- TODO: functors, limits, categories with limits, initial as limit, comma categories, algebras
-- TODO: TT-like recursor, induction
