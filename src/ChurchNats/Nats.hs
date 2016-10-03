{-# LANGUAGE Rank2Types #-}


type Nat = forall a. (a -> a) -> a -> a

zero :: Nat
zero = \f x -> x 


suc :: Nat -> Nat
suc  = \n f x -> f (n f x) 


data Bottom

bottom :: Bottom
bottom = bottom

data Z
data S n
