module As where

import Data.Char

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp []       = []
doubleUp xs@(x:_) = x : xs

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = True
isSubseqOf [] _  = True
isSubseqOf _  [] = False
isSubseqOf picks@(p:ps) (o:os) =
  if p == o then isSubseqOf ps os else isSubseqOf picks os

capitalizeWords :: String -> [(String, String)]
capitalizeWords str = map (\word@(x:xs) -> (word, ((toUpper x) : xs))) myWords
  where myWords = words str

capitalizeWord :: String -> String
capitalizeWord (x:xs) = (toUpper x) : xs

capitalizeParagraph :: String -> [String]
capitalizeParagraph str = go False (words str)
 where
  go _     []     = []
  go True  (x:xs) = (capitalizeWord x) : go False xs
  go False (x:xs) = if (last x) == '.' then x : go True xs else x : go False xs

testing = "this is a some. words that should. be capitalized."
