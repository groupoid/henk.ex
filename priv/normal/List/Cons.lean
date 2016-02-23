

definition Cons := λ (a: Type), λ (head: a), λ (tail: ∀ (List: Type),
  ∀ (Cons:
      ∀ (head: a),
      ∀ (tail: List),
     List),
   ∀ (Nil: List),
   List),
 λ (List: Type),
 λ (Cons:
    ∀ (head: a),
   ∀ (tail: List),
   List),
 λ (Nil: List),
 Cons head (tail List Cons Nil)


check Cons
