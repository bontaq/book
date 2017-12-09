module Trees where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold fn st = match $ fn st
 where
  match Nothing = Leaf
  match (Just (treeA, val, treeB)) =
    Node (unfold fn treeA) val (unfold fn treeB)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\a -> if a < n then Just ((a + 1), a, (a + 1)) else Nothing) 0
