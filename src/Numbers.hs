module Numbers where

data Nat =
    Zero
  | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero      = 0
natToInteger (Succ xs) = 1 + natToInteger xs

integerToNat' x | x == 0 = Zero
                | x > 0  = Succ (integerToNat' (x - 1))

integerToNat :: Integer -> Maybe Nat
integerToNat x | x < 0     = Nothing
               | otherwise = Just $ integerToNat' x
