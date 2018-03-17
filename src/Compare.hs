module Compare where

remove :: Char -> String -> String
remove c []     = []
remove c (x:xs) = if x == c then xs else x : remove c xs

rr :: String -> String -> Bool
rr = undefined
