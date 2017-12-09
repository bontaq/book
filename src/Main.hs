module Main where
import Data.Char

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n =
  n
incTimes times n =
  1 + (incTimes (times - 1) n)

applyTimes :: (Eq a, Num a) =>
              a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)

acro :: [Char] -> [Char]
acro xs =
  [x | x <- xs,
   elem x ['A'..'Z']]

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

thing = map (+1) [1,2,3]

holler :: [Char] -> [Char]
holler [] = []
holler (x:xs) = (toUpper x) : holler xs

capo = toUpper . head

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
data DividedResult =
  Result (Integer, Integer)
  | DividedByZero
  deriving (Show)

dividedBy :: Integer -> Integer -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = go (abs num) (abs denom) 0
  where go n  d count
         | n < d = checkResult count n
         | otherwise =
           go (n - d) d (count + 1)
        checkResult count n
          | num < 0 && denom < 0 = Result (count, n)
          | num < 0 || denom < 0 = Result (-count, n)
          | otherwise = Result (count, n)

sumTo :: (Eq a, Num a) => a -> a
sumTo 0 = 0
sumTo n = n + sumTo (n - 1)

multi :: (Integral a) => a -> a -> a
multi _ 0 = 0
multi a b = a + multi a (b - 1)

mc91 n 
  | n > 100 = n - 10
  | n <= 100 = mc91 $ mc91 (n + 11)

f True = error "blah"
f False = 0

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

main :: IO ()
main = do
  putStrLn "ayyyyy amelio"
