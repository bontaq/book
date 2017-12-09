module Thes where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe word  = Just word

replaceThe :: String -> String
replaceThe str = foldr (\c a -> (test c) ++ a) "" $ words str
 where
  test x = case notThe x of
    Just w  -> w ++ " "
    Nothing -> "a "

vowels :: String
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel input = foldr (\a b -> (a == input) || b) False vowels

startsWithVowel :: String -> Bool
startsWithVowel ""        = False
startsWithVowel (input:_) = (> 0) $ length $ filter (\x -> x == input) vowels

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel input = go (words input)
 where
  go ("the":b:rest) | startsWithVowel b == True = 1 + (go rest)
                    | otherwise                 = 0 + (go rest)
  go (x:xs) = go xs
  go _      = 0

countVowels :: String -> Int
countVowels stuff = length $ filter isVowel stuff

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord word = case compare vowelCount consonantCount of
  GT -> Nothing
  _  -> Just $ Word' word
 where
  vowelCount     = countVowels word
  consonantCount = (length word) - vowelCount
