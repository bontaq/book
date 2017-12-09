module Database where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 500
  , DbString "Hello, World!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
  foldr
    (\a b ->
       case a of
         DbDate date -> date : b
         _ -> b)
    []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =
  foldr
    (\a acc ->
       case a of
         DbNumber num -> num : acc
         _ -> acc)
    []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent =
  foldr max (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 31423)) .
  filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db =
  (fromIntegral . sumDb $ db) / (fromIntegral . length . filterDbNumber $ db)

fibs =
    go = 1 : scanl (+) 1 go

-- fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
