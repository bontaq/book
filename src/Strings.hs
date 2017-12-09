module Strings where

notThe :: String -> Maybe String
notThe ws | ws == "the" = Nothing
          | otherwise   = Just ws

replaceThe :: String -> String
replaceThe = undefined

main :: IO ()
main = do
  putStrLn . show $ notThe "the thing"
