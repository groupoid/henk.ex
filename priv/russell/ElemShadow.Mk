-- #ElemShadow.Mk : \/(P : #Prop) -> \/(p : P) -> #ElemShadow

-- args
\ (P : #Prop) ->
\ (p : P) ->

-- setoid
\ (ElemShadow : #Star ) ->
\ (ElemShadowOk : ElemShadow -> #Prop ) ->
\ (ElemShadowEqu : ElemShadow -> ElemShadow -> #Prop ) ->
\ (ElemShadowRefl :
    \/(s : ElemShadow) ->
    ElemShadowEqu s s ) ->
\ (ElemShadowTrans :
    \/(s1 : ElemShadow) -> \/(s2 : ElemShadow) -> \/(s3 : ElemShadow) ->
    ElemShadowEqu s1 s2 -> ElemShadowEqu s2 s3 -> ElemShadowEqu s1 s3) ->
\ (ElemShadowSym :
    \/(s1 : ElemShadow) -> \/(s2 : ElemShadow) ->
    ElemShadowEqu s1 s2 -> ElemShadowEqu s2 s1) ->

-- algebra/constructors
\ (Mk : \/(P : #Prop) -> \/(p : P) -> ElemShadow) ->
\ (MkOk : \/(P : #Prop) -> \/(p : P) -> ElemShadowOk (Mk P p)) ->
\ (MkEqu :
    \/(P1 : #Prop) -> \/(p1 : P1) -> \/(P2 : #Prop) -> \/(p2 : P2) ->
    #EquProp P1 P2 -> ElemShadowEqu (Mk P1 p1) (Mk P2 p2)) ->

-- result
Mk P p
