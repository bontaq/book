module Trees where

data Weekday =
  Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday


f Friday = "Miller Time"

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) | b == a = Node left a right
                              | b < a  = Node (insert' b left) a right
                              | b > a  = Node left a (insert' b right)

map' :: (a -> b) -> BinaryTree a -> BinaryTree b
map' _ Leaf                = Leaf
map' f (Node left a right) = Node (map' f left) (f a) (map' f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if map' (+1) testTree' == mapExpected
  then print "yup great!"
  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f init tree = foldr f init $ inorder tree

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testTree2 =
  Node (Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)) 1 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder success!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder
