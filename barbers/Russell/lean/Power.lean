/- Power -/

-- not .lean - pseudocode

Prop : Setoid
myFalse : Prop
myNot : SetoidHom Prop Prop
isFix : SetoidHom a a -> Prop
aParadox : isFix myNot -> myFalse

Power : SetoidBox -> SetoidBox
Power S = SetoidHom S Prop
fp : Functor Power
fp = {map {A} {B} f p b = {a : A; e : eq (f a) b; ok : p a}}

isFixOn F I = {op: F I -> I; coop: I -> F I}
Fix : (F:Set->Set')->(FF:Functor F)->{I:Set; isFixOb F I}

-- naive set theory as a fixpoint
Naive = Fix Power
good : Naive -> Prop
good n = coop n n
bad n = not (good n)
Barber = op (bad)
badBarber : bad Barber
goodBarber : good Barber

theParadox = badBarber goodBarber
