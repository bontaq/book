module Kinds where

data Silly a b c d =
  MkSilly a b c d deriving Show

-- == [] a = [] | a : [a]
data List a = Nil | Cons a (List a)

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)
