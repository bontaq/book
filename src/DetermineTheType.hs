{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w(q(a))

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (a,b) = ((xz a), (yz b))

munge :: (x -> y)
  -> (y -> (w, z))
  -> x
  -> w
munge xToY yToTup x = fst $ yToTup(xToY x)


