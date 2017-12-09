module EitherLib where

lefts' :: [Either a b] -> [a]
lefts' items = foldr
  ( \item acc -> case item of
    (Left  a) -> a : acc
    (Right b) -> acc
  )
  []
  items

rights' :: [Either a b] -> [b]
rights' items = foldr
  ( \item acc -> case item of
    (Left  a) -> acc
    (Right b) -> b : acc
  )
  []
  items

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' items = (as, bs)
 where
  as = lefts' items
  bs = rights' items

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' bToC (Right b) = Just $ bToC b
eitherMaybe' bToC _         = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' aToC _    (Left  a) = aToC a
either' _    bToC (Right b) = bToC b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' bToC (Right a) = Just (either' id bToC (Right a))
