module MaybeLib where

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe b aToB (Just a) = aToB a
mayybe b aToB Nothing  = b

fromMaybe :: a -> Maybe a -> a
fromMaybe base mA = mayybe base id mA

listToMaybe :: [a] -> Maybe a
listToMaybe (x:_) = Just x
listToMaybe []    = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing  = []

catMaybes :: [Maybe a] -> [a]
catMaybes (Just x :xs) = x : catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs
catMaybes []           = []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe orig = undefined
