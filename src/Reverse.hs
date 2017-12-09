module Reverse where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if (f x) == True then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem i xs = any (==i) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f a = squish . map f $ a

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = go f xs (head xs)
  where go f (x:xs) top
         | length xs == 0 = if (f x top) == GT then x else top
         | (f x top) == GT = go f xs x
         | (f x top) == LT = go f xs top
         | (f x top) == EQ = go f xs x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = go f xs (head xs)
  where go f (x:xs) top
         | length xs == 0 = if (f x top) == GT then top else x
         | (f x top) == GT = go f xs top
         | (f x top) == LT = go f xs x
         | (f x top) == EQ = go f xs x

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
