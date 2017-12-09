module Phone where

import Data.Char

type Digit = Char
type Options = [Char]
type Presses = Int
data Key = Key Digit Options deriving Show
data DaPhone = DaPhone [Key] deriving Show

keyboard :: DaPhone
keyboard = DaPhone
  [ Key '1' ['1']
  , Key '2' ['A', 'B', 'C', '2']
  , Key '3' ['D', 'E', 'F', '3']
  , Key '4' ['G', 'H', 'I', '4']
  , Key '5' ['J', 'K', 'L', '5']
  , Key '6' ['M', 'N', 'O', '6']
  , Key '7' ['P', 'Q', 'R', 'S', '7']
  , Key '8' ['T', 'U', 'V', '8']
  , Key '9' ['W', 'X', 'Y', 'Z', '9']
  , Key '*' ['^']
  , Key '0' ['+', ' ', '0']
  , Key '#' ['.', ',']
  ]

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok."
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  ]

-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
-- ' ' -> [(0, 2)]

exists :: Ord a => a -> [a] -> Bool
exists a []     = False
exists a (x:xs) = if a == x then True else exists a xs

indexOf :: Eq a => a -> [a] -> Int
indexOf item items = snd . head $ foundItem
 where
  withIndex = zip items [1 ..]
  foundItem = filter (\(char, index) -> char == item) withIndex

charInOptions :: Char -> Key -> Bool
charInOptions char (Key _ options) = exists char options

findKey :: DaPhone -> Char -> Key
findKey (DaPhone keys) char = foldr
  (\og new -> if charInOptions upChar new then new else og)
  (Key ' ' [])
  keys
  where upChar = toUpper char

reverseTap :: DaPhone -> Char -> [(Digit, Presses)]
reverseTap phone char = capitalizeIt ++ [(digit, presses)]
 where
  upChar            = toUpper char
  foundKey          = findKey phone upChar
  (Key digit chars) = foundKey
  presses           = indexOf upChar chars

  capitalizeIt | isUpper char = [('*', 1)]
               | otherwise    = []

reverseTaps :: DaPhone -> String -> [(Digit, Presses)]
reverseTaps keyboard str =
  foldr (\char acc -> (reverseTap keyboard char) ++ acc) [] str

-- cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
-- cellPhonesDead = undefined

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps ps = foldr (+) 0 $ map (\(_, p) -> p) ps

-- map through every character and get a total count
numChars :: Char -> [Char] -> Int
numChars c str =
  foldr (\char total -> if c == char then total + 1 else total) 0 str

max' :: [(Char, Int)] -> (Char, Int)
max' = foldr
  (\(c, num) (tc, tnum) -> if num > tnum then (c, num) else (tc, tnum))
  (' ', 0)

max'' :: [([Char], Int)] -> ([Char], Int)
max'' = foldr
  (\(c, num) (tc, tnum) -> if num > tnum then (c, num) else (tc, tnum))
  (" ", 0)

mostPopularLetter :: String -> (Char, Int)
mostPopularLetter str = max' $ map (\c -> (c, numChars c aStr)) ['a' .. 'z']
  where aStr = str

tapCost :: (Char, Int) -> Presses
tapCost (letter, num) =
  fingerTaps . reverseTaps keyboard $ [ letter | _ <- [0 .. num] ]

coolestLtr :: [String] -> Char
coolestLtr = fst . mostPopularLetter . foldr (++) []

countWords :: [[Char]] -> [([Char], Int)]
countWords myWords = map
  (\w -> (w, (foldr (\w' t -> if w == w' then t + 1 else t) 0 myWords)))
  myWords

coolestWord :: [String] -> (String, Int)
coolestWord =
  max'' . countWords . words . foldr (\s t -> t ++ " " ++ (map toLower s)) []
