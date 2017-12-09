module Vigener where

import Data.Char

-- chr produces integer -> single character
-- ord produces single character -> Integer
-- mod does what you think

-- MEET AT DAWN
-- ALLY AL LYAL

getAdjustment :: Char -> Int
getAdjustment c = (ord c) - 65

applyAdjustment :: Char -> Int -> Char
applyAdjustment c i = chr $ (mod (((ord c) - 65) + i) 26) + 65

vig :: String -> String -> String
vig str forRepeat = map (\(character, adjusterChar) -> applyAdjustment character $ getAdjustment adjusterChar)
                        (zip str repeated)
  where repeated = cycle forRepeat
