{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where
import Data.Bool

myHead :: [a] -> Maybe a
myHead []    = Nothing
myHead (x:_) = Just x

myTail :: [a] -> Maybe [a]
myTail []     = Nothing
myTail (x:[]) = Nothing
myTail (_:xs) = Just xs

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

data Price =
  Price Integer deriving (Eq, Show)

data Size =
  Size Integer deriving (Eq, Show)

data Manufacturer =
  Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
  PapuAir
  | Catapults
  | TakeYourChances
  deriving (Eq, Show)

data MyTypes = MyVal Int
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 3000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

data Example = MakeExample Int deriving Show

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)
newtype Cows =
  Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42
instance TooMany (Int, Int) where
  tooMany (a, b) = (a + b) > 42
instance (Num a, TooMany a, Ord a) => TooMany (a, a) where
  tooMany (a, b) = (a + b) > 42
instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

-- cardinality of 4
data BigSmall =
  Big Bool
  | Small Bool
  deriving (Eq, Show)

-- cardinality of 3
data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth
                 deriving (Eq, Show)
-- cardinality of 9
-- data TwoQs =
--   MkTwoQs QuantumBool QuantumBool
--   deriving (Eq, Show)

type TwoQs = (QuantumBool, QuantumBool)

data Person =
  Person { name :: String
         , age :: Int }
  deriving (Eq, Show)

-- a * (b + c) = (a * b) + (a * c)
data Fiction = Fiction deriving Show
data NonFiction = NonFiction deriving Show

data BookType = FictionBook Fiction
              | NonFictionBook NonFiction
              deriving Show

type AuthorName = String
data Author = Author (AuthorName, BookType)

-- data FlowerType = Gardenia
--                 | Daisy
--                 | Rose
--                 | Lilac
--                 deriving Show
type Gardener = String
-- data Garden =
--   Garden Gardener FlowerType
--   deriving Show

data Garden =
  Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener


-- new stuff
data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b }
  deriving (Eq, Show)

type Awesome = Bool
type Name = String

person :: Product Name Awesome
person = Product "Simon" True

-- data Twitter =
--   Twitter deriving (Eq, Show)
--
-- data AskFm =
--   AskFm deriving (Eq, Show)

-- socialNetwork :: Sum Twitter AskFm
-- socialNetwork = First Twitter

data SocialNetwork =
  Twitter
  | AskFm
  deriving (Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmhouse' =
  Product NumCow (Product NumPig NumSheep)

-- type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal =
  Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)

-- data author =
--   fiction authorname
--   | nonfiction authorname

-- foldr
-- (1 ^ (2 ^ (3 ^ 2)))
-- (const 1 (

-- foldl (flip (*)) 1 [1..3]
-- (((1 * 1) * 2) * 3)


-- foldl
-- (((2 ^ 1) ^ 2) ^ 3)

-- ((([] : 1]) : 2) : 3)


main :: IO ()
main = do
  putStrLn "hello world"
