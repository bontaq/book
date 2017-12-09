module Programmers where

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSD
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem
  , lang :: ProgLang
  } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSD, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [[Programmer]]
allProgrammers =
  map
    (\os -> map (\lang -> Programmer {os = os, lang = lang}) allLanguages)
    allOperatingSystems

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten (xs)

data ThereYet =
  There Float
        Int
        Bool
  deriving (Eq, Show)

nope :: Float -> Int -> Bool -> ThereYet
nope = undefined

notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5
