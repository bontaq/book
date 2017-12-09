module Cipher where

import           Data.Char

caesar shift []     = []
caesar shift (x:xs) = changed : caesar shift xs
 where
  shift'  = (+ 97) . (\y -> mod y 26) . (+ (-97))
  changed = chr . shift' . (+ shift) . ord $ x

uncaesar shift []     = []
uncaesar shift (x:xs) = changed : uncaesar shift xs
 where
  shift'  = (+ 97) . (\y -> mod y 26) . (+ (-97))
  changed = chr . shift' . (+ (-shift)) . ord $ x
