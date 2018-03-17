module Ch22 where

import Control.Applicative
import Data.Maybe

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

t :: Maybe Int -> Int
t = undefined

newtype Reader r a =
  Reader { runReader :: r -> a }

s :: Reader Int Int
s =
  let r :: Reader Int Int
      r = Reader (\x -> x)
  in
    r
