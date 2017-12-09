module Review where

import Data.Bool

stops = "pbtdkg"

vowels = "aeiou"

-- p a p
-- p a b
-- p a t
--combino :: [Char] -> [Char] -> [(Char, Char, Char)]
--combino [] [] [] = ([] :: [(Char, Char, Char)])
flatten [] = []
flatten (x:xs) = x ++ flatten xs

combino s1 v s2 =
  flatten $ flatten $ map (\s1 -> map (\v -> map (\s2 -> (s1, v, s2)) s2) v) s1

--cambino :: [Char] -> [Char] -> [Char] -> [(Char, Char, Char)]
--cambino [] [] [] = ([] :: [(Char, Char, Char)])
--cambino [s1] (v:vs) (s2:s2s) = (s1, v, s2) : cambino s1 v s2s
--cambino (s1:s1s) (v:vs) (s2:s2s) = (s1, v, s2) : cambino s1s vs s2s
-- lol ian
-- remember comprehensions because they're ok
cambino :: [a] -> [a] -> [a] -> [(a, a, a)]
cambino s1 v s2 = [(a, b, c) | a <- s1, b <- v, c <- s2]

--seekritFunc :: [Char] -> Int
seekritFunc x =
  (fromIntegral (sum (map length (words x)))) /
  (fromIntegral (length (words x)))

-- average word length of sentance
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr (\b acc -> acc || b == a) False

myElem' a = myAny (== a)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\b acc -> f b : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> bool acc (x : acc) (f x)) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs =
  foldr
    (\x acc ->
       case f x acc of
         GT -> x
         _ -> acc)
    (head xs)
    xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs =
  foldr
    (\x acc ->
       case f x acc of
         LT -> x
         _ -> acc)
    (head xs)
    xs
