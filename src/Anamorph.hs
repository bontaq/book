module Anamorph where

myIterate :: (a -> a) -> a -> [a]
myIterate fn st = st : myIterate fn (fn st)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr fn st = match $ fn st
  where
    match (Just (a, b)) = a : myUnfoldr fn b

betterIterate :: (a -> a) -> a -> [a]
betterIterate fn st = myUnfoldr (\a -> Just (a, fn(a))) st
